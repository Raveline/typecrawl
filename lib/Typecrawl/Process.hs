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
processSite :: PlatformParseInstructions -> Url -> FilePath -> Maybe Int -> IO ()
processSite ppis url dest steps = let
  process (Just depth) = process' >-> P.take depth
  process Nothing = process'
  process' = postsOnPage ppis (Just url)
  in P.toListM (process steps) >>= storeScrapped dest . join
