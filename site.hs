--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import           System.Directory(getDirectoryContents)
import           System.FilePath (takeDirectory, splitExtension, takeExtension,
                                  takeFileName, takeBaseName, joinPath, (</>))
import qualified Data.ByteString.Lazy as LBS
import           Data.List (intercalate, isSuffixOf)
import           Control.Monad.Reader (liftIO)

import           Debug.Trace

--
-- image grid related code
--

-- "unflattens" a single list into array of "blocks" or "rows" of given size
-- to be used to generate a "gallery" page from a list of images
-- TODO: rewrite using higher order functions
unflatten :: Int -> [a] -> [[a]]
unflatten n lst = reverse $ helper lst n []
    where
        helper [] _ acc        = acc
        helper list blocks acc = helper remaining count $ block:acc
            where
                count     = minimum [blocks, length list]
                block     = take count list
                remaining = drop count list

gridColumns :: Int
gridColumns = 3

previewSuffix :: String
previewSuffix = "_preview"

-- full path to an element with given name, provided it is in a given directory
nameToUrl :: String -> String -> String
nameToUrl dir name = joinPath ["/", dir,  name]

-- full path to a preview with given name, provided it is in a given directory
nameToPreviewUrl :: String -> String -> String
nameToPreviewUrl dir name = joinPath ["/", dir, base ++ previewSuffix ++ ext]
    where
        (base, ext) = splitExtension name

-- extract file path from CopyFile
getFilePath :: CopyFile -> String
getFilePath (CopyFile fp) = fp

-- extract item body
getBody :: Item a -> a
getBody (Item id body) = body

-- extract item id
getId :: Item a -> Identifier
getId (Item id _) = id

photoCompiler = getResourceLBS >>= withItemBody photoFilter

photoFilter :: LBS.ByteString -> Compiler LBS.ByteString
photoFilter = unixFilterLBS "convert" ["-", "-auto-orient",
                                       "-resize", "300x300",
                                       "-"]

--
-- generic Hakyll stuff
--

-- post context: default but also adds a date
postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

teaserCtx = teaserField "teaser" "content" `mappend` postCtx

-- custom route to turn page.markdown into /page/index.html
noExtRoute :: Routes
noExtRoute = customRoute createIndexRoute
    where
        createIndexRoute ident = takeDirectory p </>
                                 takeBaseName p  </>
                                 "index.html"
            where
                p = toFilePath ident

noExtRouteOneUp :: Routes
noExtRouteOneUp = customRoute createIndexRoute
    where
        createIndexRoute ident = takeDirectory (takeDirectory p) </>
                                 takeBaseName p                  </>
                                 "index.html"
            where
                p = toFilePath ident

-- remove trailing "index.html" from the string
noExtIndex :: String -> String
noExtIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
    where idx = "index.html"

-- remove trailing "index.html" from urls
noExtUrls :: Item String -> Compiler (Item String)
noExtUrls = return . fmap (withUrls noExtIndex)
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/gallery/**.jpg" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/gallery/**.jpg" $ version "preview" $ do
        route   $ gsubRoute ".jpg" (const $ previewSuffix ++ ".jpg")
        compile $ photoCompiler


    -- TODO: move to another file
    -- a rather hacky (and not typesafe) way to generate gallery index pages
    -- match galleries at any level and generate preview pages
    -- gallery directory MUST have index.html inside (which is a clutch too)
    match "img/gallery/**/index.html" $ do
        route   $ setExtension "html"
        compile $ do
            -- path to matched file
            path <- fmap toFilePath getUnderlying
            -- grid template
            grid <- loadBody "templates/grid.html"
            -- load images
            let dir = takeDirectory path
            let pattern = fromGlob $ dir ++ "/*.jpg"
            loaded <- loadAll (pattern .&&. hasNoVersion)
            -- convert images to plain strings; keep file names only
            let names = map (takeFileName . getFilePath . getBody) loaded
            -- unflatten them into a grid
            -- TODO: set number of columns somewhere as a constant
            let structured = unflatten gridColumns names

            -- prepare context with nested loops and nested templates
            -- looks more like Lisp, doesn't it?
            let ctx = listField "rows"
                                (
                                    listFieldWith "cols"
                                                  ((field "name"    (return . itemBody)) `mappend`
                                                   (field "url"     (return . nameToUrl dir . itemBody)) `mappend`
                                                   (field "preview" (return . nameToPreviewUrl dir . itemBody)))
                                                  (sequence . map makeItem . itemBody)
                                )
                                (sequence (map makeItem structured)) `mappend`
                      -- this field is actually for another template, default
                      -- (see below)
                      constField "title" ("Image index: " ++ dir) `mappend`
                      defaultContext

            -- apply template and compile
            -- apply grid template, then load and apply default template
            -- TODO: there's not really a need to load the grid template earlier
            pandocCompiler >>= applyTemplate grid ctx
                           >>= loadAndApplyTemplate "templates/default.html" ctx

    match "css/**" $ do
        route   idRoute
        compile copyFileCompiler
        --compile compressCssCompiler

    -- this will probably be eliminated
    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= noExtUrls

    match "posts/*" $ do
        --route $ setExtension "html"
        route   $ noExtRoute
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            -- >>= loadAndApplyTemplate "templates/post.html"    (teaserField "teaser" "content" `mappend` postCtx)
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= noExtUrls

    match "common/*" $ do
        route   $ noExtRouteOneUp
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
            >>= noExtUrls

    ---create ["archive.html"] $ do
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    --listField "posts" postCtx (return posts) `mappend`
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= noExtUrls

    -- incomplete example generating a single page
    --create ["about.html"] $ do
    --    route idRoute
    --    compile $ do
    --        --about <- load "common/about.markdown"
    --        let aboutCtx =
    --                field "body" (\_ -> return "actual data") `mappend`
    --                defaultContext
    --        makeItem ""
    --            >>= loadAndApplyTemplate "templates/default.html" aboutCtx
    --            >>= relativizeUrls
    --            >>= noExtUrls

    -- save it for later (about page)
    --match "index.html" $ do
    --    route idRoute
    --    compile $ do
    --        posts <- recentFirst =<< loadAll "posts/*"
    --        let indexCtx =
    --                listField "posts" postCtx (return posts) `mappend`
    --                constField "title" "Home"                `mappend`
    --                defaultContext
    --        getResourceBody
    --            >>= applyAsTemplate indexCtx
    --            >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
