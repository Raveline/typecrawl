{-# LANGUAGE OverloadedStrings #-}
module Typecrawl.Scrapper (postsOnPage) where

import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as T
import Pipes
import Text.HTML.Scalpel (scrapeURL, chroots, (//), (@:),
                          attr, attrs, Scraper, hasClass,
                          text, anySelector, html)

import Typecrawl.Types

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
