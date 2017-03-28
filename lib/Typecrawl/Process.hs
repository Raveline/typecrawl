module Typecrawl.Process (processSite) where

import Pipes
import qualified Pipes.Prelude as P
import Control.Monad (join)

import Typecrawl.Types
import Typecrawl.Scrapper
import Typecrawl.Exporter

-- | Starts the whole scrapping shenanigan.
-- This could be severely improved, most likely by
-- making scrapping more parametrable, through a
-- collection of commands (where to get nextPage,
-- how to collect direct post links, etc.)
processSite :: PlatformParseInstructions -> Url -> FilePath -> Int -> IO ()
processSite ppis url dest steps = let
  process :: Producer [Post] IO ()
  process = postsOnPage ppis (Just url) >-> P.take steps
  in P.toListM process >>= storeScrapped dest . join
