{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (join)
import System.Environment
import Typecrawl.Process (processSite)


-- data TypeCrawl = TypeCrawl { url :: Url
--                           , output :: FilePath
--                           , steps :: Integer }


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
