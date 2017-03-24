{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import System.Environment
import Text.HTML.Scalpel (scrapeURL, chroots, (//), (@:),
                          attr, attrs, Scraper, hasClass,
                          text, anySelector)
import Pipes

type Url = String
type Title = T.Text
type Content = T.Text

data Post = Post Title Content deriving (Show)

data ScrapingStep = Scrap Int (Maybe String) deriving (Show)


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
extractPostLinks =  attrs "href" $ ("h3" // "a")

-- | Typepad blogs typically paginates entries. Pagination links
-- to the next page are available in spans with the "pager-right" class.
nextLink :: Scraper String Url
nextLink = attr "href" $ ("span" @: [hasClass "pager-right"] // "a")

-- | Read a single article on a single page
getArticle :: Url -> IO (Maybe Post)
getArticle articleUrl = scrapeURL articleUrl extractArticle

-- | Scrapper that shall read the content of a post.
-- This is rather crude because I'm not yet certain what kind
-- of output I'm looking for (raw texte or HTML ?).
-- The extractContent bit is messy and will fail at ordering properly.
-- Typical bug include this case :
--
-- <div class="entry-body">
--      <p>p1</p>
--      <p>p2</p>
--      <blockquote><p>q1</p></blockquote>
--      <p>p3</p>
-- </div>
--
-- Here, you'd expect p1-p2-q1-p3. What you'll get is : p1-p2-p3-q1
-- I'm unsure about what to do : lookup Scalpel and see why, or fix
-- with a <|> solution.
-- Or maybe this will be fixed once I've decided  on the output, since
-- I might take HTML on anySelector rather than text on "p" only.
extractArticle :: Scraper String Post
extractArticle = do
  title <- extractTitle
  content <- extractContent
  return $ Post (T.pack title) (T.unlines . map T.pack $ content)
  where
    extractTitle :: Scraper String String
    extractTitle = text $ "h3" @: [hasClass "entry-header"]
    extractContent :: Scraper String [String]
    extractContent = chroots ("div" @: [hasClass "entry-body"] // "p") (text anySelector)

-- | Anamorphism for scrapping
-- Basically, apply unspool till finished, depending on state.
-- Some people way more clever than me suggest join . foldr,
-- I shall try that next.
anaScrap :: (ScrapingStep -> IO ([Post], ScrapingStep))
         -> (ScrapingStep -> Bool)
         -> ScrapingStep
         -> IO [Post]
anaScrap unspool finished state =
  if finished state
     then return []
     else do
      -- value :: [Post]
      (value, state') <- unspool state
      -- new :: [Post]
      new <- anaScrap unspool finished state'
      return $ value ++ new

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
readPageAndPosts :: ScrapingStep -> IO ([Post], ScrapingStep)
readPageAndPosts (Scrap n (Just url)) = do
  postsURL <- getLinks url
  posts <- mapM getArticle (fromMaybe [] postsURL)
  next <- scrapeURL url nextLink
  return (catMaybes posts, Scrap (n+1) next)
readPageAndPosts (Scrap n _) = return $ ([], Scrap (n+1) Nothing)

-- | Starts the whole scrapping shenanigan.
processSite :: Url -> Int -> IO [Post]
processSite url steps = anaScrap readPageAndPosts goOnScrapping (Scrap 1 (Just url))
  where
    goOnScrapping :: ScrapingStep -> Bool
    goOnScrapping (Scrap step (Just _)) = step > steps
    goOnScrapping _ = False

-- | Placeholder for some magnificent serialization later.
-- (Or at least, slightly more sophisticated than print)
storeScrapped :: [Post] -> IO ()
storeScrapped = print

-- | Bah, ugly !
-- TODO: use cmdlib or something so I don't barf
-- everytime I see these lines.
main :: IO ()
main = do args <- getArgs
          case args of
            []  -> error "Enter the base URL of a typepad blog."
            [url] -> processSite url 1 >>= storeScrapped
            [url, profound] -> processSite url (read profound) >>= storeScrapped
            _   -> error "Too many arguments."
