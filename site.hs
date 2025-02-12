--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.Reader (forM)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (intercalate, isSuffixOf, sortBy, stripPrefix)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mappend)
import           Data.Ord  (comparing)
import           Hakyll
import           System.Directory (getDirectoryContents)
import           System.FilePath ( takeDirectory
                                 , splitExtension
                                 , takeExtension
                                 , takeFileName
                                 , takeBaseName
                                 , joinPath
                                 , (</>)
                                 )


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
getBody (Item _ body) = body

-- extract item id
getId :: Item a -> Identifier
getId (Item id _) = id

photoCompiler = getResourceLBS >>= withItemBody photoFilter

photoFilter :: LBS.ByteString -> Compiler LBS.ByteString
photoFilter = unixFilterLBS "convert" [ "-"
                                      , "-auto-orient"
                                      , "-resize", "300x300"
                                      , "-"
                                      ]

-- order items by file modification time
mtimeOrdered :: [Item a] -> Compiler [Item a]
mtimeOrdered items = do
    itemsWithTime <- forM items $ \item -> do
        mtime <- getItemModificationTime (itemIdentifier item)
        return (mtime, item)
    return (map snd (sortBy (comparing fst) itemsWithTime))


--
-- generic Hakyll stuff
--

-- post context: default but also adds a date
postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx

taggedCtx :: Tags -> Context String
taggedCtx tags = tagsField "tags" tags `mappend` postCtx

-- context with either one field or missing field,
-- depending on given condition
optionalFieldCtx :: String -> String -> Bool -> Context String
optionalFieldCtx field value condition =
    if condition
        then constField field value
        else missingField

nonEmptyValueCtx :: String -> String -> Context String
nonEmptyValueCtx field value = optionalFieldCtx field value (value /= "")

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

noExtRouteOneUpAlt :: Routes
noExtRouteOneUpAlt = customRoute createIndexRoute
    where
        createIndexRoute ident = takeBaseName (takeDirectory p) </>
                                 takeBaseName p                  </>
                                 "index.html"
            where
                p = toFilePath ident

noExtRouteTwoUp :: Routes
noExtRouteTwoUp = customRoute createIndexRoute
    where
        createIndexRoute ident =
            takeDirectory (takeDirectory (takeDirectory p)) </>
            takeBaseName p                                  </>
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

-- huge function which does all the heavy lifting
-- (perfect candidate for refactoring)
site :: Configuration -> IO ()
site conf = hakyllWith conf $ do
    tags <- buildTags "content/posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        route   $ noExtRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" tag                      `mappend`
                      constField "tag"   tag                      `mappend`
                      listField  "posts" teaserCtx (return posts) `mappend`
                      defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                -- >>= relativizeUrls
                >>= noExtUrls


    -- reserved for structure-related images
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- this is for content-related images; name clashes are not prevented
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- match gallery images

    -- strip content prefix
    match "content/img/gallery/**.jpg" $ do
        route   $ gsubRoute "content/" (const "")
        compile copyFileCompiler

    -- strip content prefix and insert preview suffix
    match "content/img/gallery/**.jpg" $ version "preview" $ do
        route   $ gsubRoute "content/" (const "") `composeRoutes`
                  gsubRoute ".jpg" (const $ previewSuffix ++ ".jpg")
        compile $ photoCompiler


    -- TODO: move to another file
    -- a rather hacky (and not typesafe) way to generate gallery index pages
    -- match galleries at any level and generate preview pages
    -- gallery directory MUST have index.markdown inside (which is a crutch too)
    match "content/img/gallery/**/index.markdown" $ do
        route   $ setExtension "html" `composeRoutes`
                  gsubRoute "content/" (const "")
        compile $ do
            -- path to matched file
            path <- fmap toFilePath getUnderlying
            -- load images
            -- this is an actual directory
            let dir' = takeDirectory path
            -- while this is for the urls (stripped content prefix)
            let dir  = fromMaybe dir' (stripPrefix "content/" dir')
            let pattern = fromGlob $ dir' ++ "/*.jpg"
            loaded <- mtimeOrdered =<< loadAll (pattern .&&. hasNoVersion)
            -- convert images to plain strings; keep file names only
            let names = map (takeFileName . getFilePath . getBody) loaded
            -- unflatten them into a grid
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
                      -- title field is now pulled from index.markdown
                      --constField "title" ("Image index: " ++ dir) `mappend`
                      defaultContext

            -- apply template and compile
            -- apply grid template, then load and apply default template
            -- also apply post template to allow for gallery description
            pandocCompiler
                           >>= loadAndApplyTemplate "templates/grid.html"    ctx
                           >>= loadAndApplyTemplate "templates/post.html"    ctx
                           >>= loadAndApplyTemplate "templates/default.html" ctx
                           -- >>= relativizeUrls
                           >>= noExtUrls



    -- TODO: generate a page with list of galleries
    --create ["galleries.html"] $ do
    --    route idRoute
    --    compile $ do

    match "css/**" $ do
        route   idRoute
        compile copyFileCompiler
        --compile compressCssCompiler

    -- posts
    match "content/posts/*" $ do
        route   $ noExtRouteOneUpAlt
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    (taggedCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (taggedCtx tags)
            -- >>= relativizeUrls
            >>= noExtUrls


    -- non-post content (about, links, contact, etc)
    match "content/*.markdown" $ do
        route   $ noExtRouteTwoUp
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            -- >>= relativizeUrls
            >>= noExtUrls

    ---create ["archive.html"] $ do
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "content/posts/*"
            renderedTags <- renderTagList tags
            -- or something like this:
            --renderedTags <- renderTagCloud 50 100 tags
            let archiveCtx =
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "Posts"                 `mappend`
                    nonEmptyValueCtx "tags" renderedTags       `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                -- >>= relativizeUrls
                >>= noExtUrls

    match "templates/*" $ compile templateBodyCompiler

main :: IO ()
main = do
    -- TODO: override these options; preferably at runtime
    let conf = defaultConfiguration { destinationDirectory = "_site"
                                    , providerDirectory    = "." }
    site conf

--------------------------------------------------------------------------------
