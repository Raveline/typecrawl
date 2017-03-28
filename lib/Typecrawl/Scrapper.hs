{-# LANGUAGE OverloadedStrings #-}
module Typecrawl.Scrapper (postsOnPage) where

import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as T
import Pipes
import Text.HTML.Scalpel (Selector, Scraper, scrapeURL, chroots, attr, attrs,
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
postsOnPage :: PlatformParseInstructions -> Maybe Url -> Producer [Post] IO ()
postsOnPage _ Nothing    = return ()
postsOnPage p@(Ppis nLink pLinks pis) (Just url) = do
    postsURL <- liftIO $ getLinks url pLinks
    posts <- liftIO $ mapM (flip getArticle pis) (fromMaybe [] postsURL)
    nextPage <- liftIO $ scrapeURL url (nextLink' nLink)
    yield $ catMaybes posts
    postsOnPage p nextPage


-- | Get all the links on a page
getLinks :: Url -> Selector -> IO (Maybe [Url])
getLinks url = scrapeURL url . extractPostLinks

-- | On a standard typepad-like blog, URL are
-- inside h3 blocks (post titles). This Scraper
-- extracts every available link on a page.
extractPostLinks :: Selector -> Scraper String [Url]
extractPostLinks =  attrs "href"

-- | Typepad blogs typically paginates entries. Pagination links
-- to the next page are available in spans with the "pager-right" class.
nextLink' :: Selector -> Scraper String Url
nextLink' = attr "href"

-- | Read a single article on a single page
getArticle :: Url -> PostInstructions -> IO (Maybe Post)
getArticle articleUrl = scrapeURL articleUrl . extractArticle

-- | Scrapper that shall read the content of a post.
-- Note: we take everything that is in entry-body,
-- which means that right now, imgs will also be picked.
extractArticle :: PostInstructions -> Scraper String Post
extractArticle (Pis iT iD iC) = do
  title <- extractTitle iT
  content <- extractContent iC
  date <- extractDate iD
  return $ Post (T.pack title) (T.pack date) (T.unlines . map T.pack $ content)
  where
    extractTitle :: Selector -> Scraper String String
    extractTitle = text
    extractContent :: Selector -> Scraper String [String]
    extractContent sel = chroots sel (html anySelector)
    extractDate :: Selector -> Scraper String String
    extractDate = text
