{-# LANGUAGE OverloadedStrings #-}
module Typecrawl.Types where

import qualified Data.Text as T
import Text.HTML.Scalpel (Selector)

type Url = String
type Title = T.Text
type Content = T.Text
-- We won't compute other these, we just display, so String is fine
type Date = T.Text

-- | A fairly simple view of a blog post.
data Post = Post Title Date Content

-- | Scrapper definition
data PlatformParseInstructions = Ppis {
    nextLink :: Selector
  , postLinks :: Selector
  , postInstructions :: PostInstructions
}

data PostInstructions = Pis {
    postTitle :: Selector
  , postDate :: Selector
  , postContent :: Selector
}
