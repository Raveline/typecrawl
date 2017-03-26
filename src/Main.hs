{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (join)
import System.Environment
import Options.Applicative

import Typecrawl.Process (processSite)
import Typecrawl.Types (Url)


data TypeCrawl = TypeCrawl { url :: Url
                           , output :: FilePath
                           , steps :: Int }


crawlCommand :: Parser TypeCrawl
crawlCommand = TypeCrawl
      <$> argument str
         (metavar "URL"
         <> help "Url of the blog to scrap" 
         )
      <*> argument str
         (metavar "FILE"
          <> help "Output")
      <*> option auto
          ( long "depth"
         <> help "Number of pages to scrap"
         <> showDefault
         <> value 1
         <> metavar "INT" )

parser = info (crawlCommand <**> helper)
  ( fullDesc 
    <> progDesc "Scrap a typepad blog to build a local single html page"
    <> header "Typecrawl - a basic typepad blog scrapper")


main :: IO ()
main = crawl =<< execParser parser

crawl :: TypeCrawl -> IO ()
crawl (TypeCrawl url output depth) = processSite url output depth
