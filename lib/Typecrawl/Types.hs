{-# LANGUAGE OverloadedStrings #-}
module Typecrawl.Types where

import qualified Data.Text as T

type Url = String
type Title = T.Text
type Content = T.Text
-- We won't compute other these, we just display, so String is fine
type Date = T.Text

-- | A fairly simple view of a blog post.
data Post = Post Title Date Content
