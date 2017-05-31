{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate)
import Options.Applicative
import Typecrawl.Process (processSite)
import Typecrawl.Types (Url, PlatformParseInstructions)
import Typecrawl.Platforms
import Text.Read


data TypeCrawl = TypeCrawl Url PlatformParseInstructions FilePath (Maybe Int)

crawlCommand :: Parser TypeCrawl
crawlCommand = TypeCrawl
      <$> argument str
         ( metavar "URL"
        <> help "Url of the blog to scrap")
      <*> argument readScrapper
         ( metavar "SCRAPPER"
        <> help ("One of (" ++ potentialScrappers ++ ")"))
      <*> argument str
         ( metavar "FILE"
        <> help "Output")
      <*> optional (option auto
         ( long "depth"
        <> help "Number of pages to scrap"
        <> metavar "INT"))


scrappers :: [(String, PlatformParseInstructions)]
scrappers = [ ("blogger", blogger)
            , ("leMonde", wpLeMonde)
            , ("typepad", typepad)]

potentialScrappers :: String
potentialScrappers = intercalate "|" . map fst $ scrappers

readScrapper :: ReadM PlatformParseInstructions
readScrapper = str >>= lookupOrError
  where
    lookupOrError :: String -> ReadM PlatformParseInstructions
    lookupOrError = maybe (readerError "Not a valid scrapper") pure . flip lookup scrappers

parser :: ParserInfo TypeCrawl
parser = info (crawlCommand <**> helper)
  ( fullDesc
    <> progDesc "Scrap a typepad blog to build a local single html page"
    <> header "Typecrawl - a basic typepad blog scrapper")


main :: IO ()
main = crawl =<< execParser parser

crawl :: TypeCrawl -> IO ()
crawl (TypeCrawl url scrapper output depth) = processSite scrapper url output depth
