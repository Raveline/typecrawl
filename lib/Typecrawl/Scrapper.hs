{-# LANGUAGE OverloadedStrings #-}
module Typecrawl.Scrapper (postsOnPage) where

import Control.Applicative
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as T
import Pipes
import Text.HTML.Scalpel (Selector, Scraper, scrapeURL, chroots, attr, attrs,
                          text, anySelector, innerHTML)

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
    liftIO $ putStrLn $ "Parsing page :" ++ url
    postsURL <- liftIO $ getLinks url pLinks
    posts <- liftIO $ mapM (`getArticle` pis) (fromMaybe [] postsURL)
    nextPage <- liftIO $ scrapeURL url (nextLink' nLink)
    yield $ catMaybes posts
    postsOnPage p nextPage

-- | Get all the links on a page
getLinks :: Url -> Selector -> IO (Maybe [Url])
getLinks url = scrapeURL url . extractPostLinks

-- | On a standard typepad-like blog, URL are
-- inside h3 blocks (post titles). This Scraper
-- extracts every available link on a page.
extractPostLinks :: Selector -> Scraper T.Text [Url]
extractPostLinks =  (fmap .fmap) T.unpack . attrs "href"

-- | Typepad blogs typically paginates entries. Pagination links
-- to the next page are available in spans with the "pager-right" class.
nextLink' :: Selector -> Scraper T.Text Url
nextLink' = fmap T.unpack . attr "href"

-- | Read a single article on a single page
getArticle :: Url -> PostInstructions -> IO (Maybe Post)
getArticle articleUrl = scrapeURL articleUrl . extractArticle

-- | Scrapper that shall read the content of a post.
-- Note: we take everything that is in entry-body,
-- which means that right now, imgs will also be picked.
extractArticle :: PostInstructions -> Scraper T.Text Post
extractArticle (Pis iT iD iC) = let
    extractTitle :: Selector -> Scraper T.Text T.Text
    extractTitle = text
    extractContent :: Selector -> Scraper T.Text T.Text
    extractContent sel = T.unlines <$> chroots sel (innerHTML anySelector)
    extractDate :: Selector -> Scraper T.Text T.Text
    extractDate = text
    in Post <$> extractTitle iT <*> extractDate iD <*> extractContent iC
