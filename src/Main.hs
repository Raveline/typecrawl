{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import System.Environment
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, 
                        attribute,
                        attributeIs,
                        content,
                        element, 
                        fromDocument, 
                        child, descendant,
                        ($//), (&|), (&/), (&//), (>=>), (&.//))
import System.FilePath.Posix (splitFileName)

type Url = String

-- | Get all the links on a page
getLinks :: Cursor -> [String]
getLinks nodes = map T.unpack $ nodes $// extractLinks &| extractUrl

-- | Parse a list of articles to get each of their URL.
-- This means getting in the "alpha-inner" div, then get
-- all the h3, and then get the <a> href value.
extractLinks :: Cursor -> [Cursor]
extractLinks = element "h3" &/ element "a"

-- | Get the attribute "href" of a cursor
extractUrl :: Cursor -> T.Text
extractUrl = T.concat . attribute "href"

-- | Read a single article on a single page
getArticle :: Cursor -> [String]
getArticle nodes = map T.unpack $ nodes $// extractArticle &| extractContent

extractArticle :: Cursor -> [Cursor]
extractArticle = element "div"
                 >=> attributeIs "class" "entry-body"
                 >=> descendant

extractContent :: Cursor -> T.Text
extractContent = T.concat . content 

readPage :: Url -> IO [String]
readPage url = readWebPage url 
               >>= return . getLinks

readAllArticles :: [Url] -> IO [String]
readAllArticles url = mapM readSingleArticle url

readSingleArticle :: Url -> IO String
readSingleArticle url = readWebPage url 
                        >>= return . getArticle
                        >>= return . concat

-- Get the list of /page/n/ to get summaries
list_of_pages :: Url -> [Url]
list_of_pages url = map (appendUrl url) [1..30] -- Make this changeable, of course
    where appendUrl url n = url ++ "/page/" ++ (show n) ++ "/"

-- Process a single summary
processPage :: Url -> IO ()
processPage url = do articles <- readPage url
                     read_articles <- readAllArticles articles
                     let url_and_content = zip articles read_articles
                     mapM_ (uncurry writeArticle) url_and_content

processSite :: Url -> IO ()
processSite url = mapM_ processPage (list_of_pages url)

writeArticle :: FilePath -> String -> IO ()
writeArticle fp = writeFile destination
    where destination = "output/" ++ postfix_fp
          postfix_fp = snd . splitFileName $ fp

main :: IO ()
main = do args <- getArgs
          case args of
            []  -> error "Enter the base URL of a typepad blog."
            [x] -> processSite x
            _   -> error "Too many arguments."

-- | Read a HTML page and return a Text.XML.Cursor on it
readWebPage :: String -> IO Cursor
readWebPage url = do putStrLn $ "Reading url " ++ url
                     res <- simpleHttp url 
                     return $ fromDocument $ parseLBS res
