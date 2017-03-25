{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe, catMaybes)
import System.Environment
import Text.HTML.Scalpel (scrapeURL, chroots, (//), (@:),
                          attr, attrs, Scraper, hasClass,
                          text, anySelector, html)
import Pipes
import qualified Pipes.Prelude as P

type Url = String
type Title = T.Text
type Content = T.Text
-- We won't compute other these, we just display, so String is fine
type Date = T.Text

data Post = Post Title Date Content

data ScrapingStep = Scrap Int (Maybe String) deriving (Show)


postToText :: Post -> T.Text
postToText (Post t d c) = let title = T.concat ["<h2>", t, "</h2>"]
                              date  = T.concat ["<div>", d, "</div>"]
                          in T.unlines [title, date, c]


-- | The meaty part. Given a URL, get all posts URL here.
-- From these URL, go visit every post link and build
-- posts from these steps.
-- Then try and fetch the link to the next page. Keep
-- track of how many pages we've read so our anamorphism
-- knows when to stop.
--
-- NB: Of course, this is overkill in most case, since full
-- articles might be displayed in the main pages, and there is
-- no need to visit singular, individual blog post pages. But it might
-- not always be the case, so let's play it safe.
--
-- Similarly, paging would typically follow a convention,
-- e.g. "myblog.typepad.com/myblog/page/X/" where X is the page
-- page number. But since I might want to adapt this for other
-- websites / blog platforms, I prefer this more generic solution.
postsOnPage :: Maybe Url -> Producer [Post] IO ()
postsOnPage Nothing    = return ()
postsOnPage (Just url) = do
    postsURL <- liftIO $ getLinks url
    posts <- liftIO $ mapM getArticle (fromMaybe [] postsURL)
    nextPage <- liftIO $ scrapeURL url nextLink
    yield $ catMaybes posts
    postsOnPage nextPage

-- | Get all the links on a page
getLinks :: Url -> IO (Maybe [Url])
getLinks url = scrapeURL url extractPostLinks

-- | On a standard typepad-like blog, URL are
-- inside h3 blocks (post titles). This Scraper
-- extracts every available link on a page.
extractPostLinks :: Scraper String [Url]
extractPostLinks =  attrs "href" $ "h3" // "a"

-- | Typepad blogs typically paginates entries. Pagination links
-- to the next page are available in spans with the "pager-right" class.
nextLink :: Scraper String Url
nextLink = attr "href" $ "span" @: [hasClass "pager-right"] // "a"

-- | Read a single article on a single page
getArticle :: Url -> IO (Maybe Post)
getArticle articleUrl = scrapeURL articleUrl extractArticle

-- | Scrapper that shall read the content of a post.
-- Note: we take everything that is in entry-body,
-- which means that right now, imgs will also be picked.
extractArticle :: Scraper String Post
extractArticle = do
  title <- extractTitle
  content <- extractContent
  date <- extractDate
  return $ Post (T.pack title) (T.pack date) (T.unlines . map T.pack $ content)
  where
    extractTitle :: Scraper String String
    extractTitle = text $ "h3" @: [hasClass "entry-header"]
    extractContent :: Scraper String [String]
    extractContent = chroots ("div" @: [hasClass "entry-body"]) (html anySelector)
    extractDate :: Scraper String String
    extractDate = text $ "h2" @: [hasClass "date-header"]


-- | Starts the whole scrapping shenanigan.
processSite :: Url -> Int -> IO ()
processSite url steps = let
  process :: Producer [Post] IO ()
  process = postsOnPage (Just url) >-> P.take steps
  in runEffect $ for process (lift . storeScrapped)

-- | Placeholder for some magnificent serialization later.
-- (Or at least, slightly more sophisticated than print)
storeScrapped :: [Post] -> IO ()
storeScrapped posts =
  let mainContent = T.unlines . map postToText $ posts in
    TIO.writeFile "test.html" . T.concat $ ["<html>",
                                            "<head>",
                                            "<meta charset='utf-8'/>",
                                            "</head>",
                                            "<body>",
                                            mainContent,
                                            "</body>",
                                            "</html>"]

-- | Bah, ugly !
-- TODO: use cmdlib or something so I don't barf
-- everytime I see these lines.
main :: IO ()
main = do args <- getArgs
          case args of
            []  -> error "Enter the base URL of a typepad blog."
            [url] -> processSite url 1
            [url, profound] -> processSite url (read profound)
            _   -> error "Too many arguments."
