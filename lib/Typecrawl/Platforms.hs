{-# LANGUAGE OverloadedStrings #-}
module Typecrawl.Platforms (typepad, wpLeMonde, blogger)
where

import Text.HTML.Scalpel ((@:), hasClass, (//))

import Typecrawl.Types


-- Should work with any basic typepad
typepad :: PlatformParseInstructions
typepad = Ppis ("span" @: [hasClass "pager-right"] // "a")
               ("h3" // "a")
               (Pis
                  ("h3" @: [hasClass "entry-header"])
                  ("h2" @: [hasClass "date-header"])
                  ("div" @: [hasClass "entry-body"]))

-- Should work with most WordPress on the french Le Monde newspaper
-- blogging platform
wpLeMonde :: PlatformParseInstructions
wpLeMonde = Ppis ("div" @: [hasClass "nav-previous"] // "a")
                 ("h2" @: [hasClass "entry-title"] // "a")
                 (Pis
                   ("h1" @: [hasClass "entry-title"])
                   ("span" @: [hasClass "entry-date"])
                   ("div" @: [hasClass "entry-content"]))

-- Should work with any basic Blogger website
blogger :: PlatformParseInstructions
blogger = Ppis ("a" @: [hasClass "blog-pager-older-link"])
               ("h3" @: [hasClass "post-title"] // "a")
               (Pis
                   ("h3" @: [hasClass "post-title"])
                   ("h2" @: [hasClass "date-header"])
                   ("div" @: [hasClass "entry-content"]))
