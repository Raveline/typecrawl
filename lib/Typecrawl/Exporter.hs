{-# LANGUAGE OverloadedStrings #-}
module Typecrawl.Exporter where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Typecrawl.Types


postToHTML :: Post -> T.Text
postToHTML (Post t d c) = let title = T.concat ["<h2>", t, "</h2>"]
                              date  = T.concat ["<div>", d, "</div>"]
                          in T.unlines [title, date, c]

-- | A simple storage in a single HTML page containing
-- everything we parsed.
storeScrapped :: [Post] -> IO ()
storeScrapped posts =
  let mainContent = T.unlines . map postToHTML $ posts in
    TIO.writeFile "test.html" . T.concat $ ["<html>",
                                            "<head>",
                                            "<meta charset='utf-8'/>",
                                            "</head>",
                                            "<body>",
                                            mainContent,
                                            "</body>",
                                            "</html>"]
