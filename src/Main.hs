{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Maybe (fromMaybe, catMaybes)
import System.Environment
import Text.HTML.Scalpel (scrapeURL, chroots, (//), (@:),
                          attr, attrs, Scraper, hasClass,
                          text, anySelector)

type Url = String
type Title = T.Text
type Content = T.Text

data Post = Post Title Content deriving (Show)

data ScrapingStep = Scrap Int (Maybe String) deriving (Show)

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

extractArticle :: Scraper String Post
extractArticle = do
  title <- extractTitle
  content <- extractArticle
  return $ Post (T.pack title) (T.unlines . map T.pack $ content)
  where
    extractTitle :: Scraper String String
    extractTitle = text $ "h3" @: [hasClass "entry-header"]
    extractArticle :: Scraper String [String]
    -- Note: this will mess up ordering if there are inner elements.
    -- Typical bug include this case :
    -- <div class="entry-body"><p>p1</p><p>p2</p><blockquote><p>q1</p></blockquote><p>p3</p>
    -- Here, you'd expect p1-p2-q1-p3. What you'll get is : p1-p2-p3-q1
    -- I'm unsure about what to do : lookup Scalpel and see why, or fix with a <|> solution.
    -- Or maybe this will be fixed once I've decided  on the output, since I might take
    -- HTML on anySelector rather than text on "p" only.
    extractArticle = chroots ("div" @: [hasClass "entry-body"] // "p") (text anySelector)

-- | Anamorphism for scrapping
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

readPageAndPosts :: ScrapingStep -> IO ([Post], ScrapingStep)
readPageAndPosts (Scrap n (Just url)) = do
  postsURL <- getLinks url
  posts <- mapM getArticle (fromMaybe [] postsURL)
  next <- scrapeURL url nextLink
  return (catMaybes posts, Scrap (n+1) next)
readPageAndPosts (Scrap n _) = return $ ([], Scrap (n+1) Nothing)

processSite :: Url -> Int -> IO [Post]
processSite url steps = anaScrap readPageAndPosts goOnScrapping (Scrap 1 (Just url))
  where
    goOnScrapping :: ScrapingStep -> Bool
    goOnScrapping (Scrap step (Just url)) = step < steps
    goOnScrapping _ = False

-- | Bah, ugly !
-- TODO: use cmdlib or something so I don't barf
-- everytime I see these lines.
main :: IO ()
main = do args <- getArgs
          case args of
            []  -> error "Enter the base URL of a typepad blog."
            [url] -> processSite url 1 >>= print
            [url, profound] -> processSite url (read profound) >>= print
            _   -> error "Too many arguments."
