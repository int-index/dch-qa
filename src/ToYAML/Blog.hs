{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ToYAML.Blog where

import BasePrelude as P
import Data.Yaml (encode)
import Data.Aeson.Tagged
import qualified Data.ByteString as BS
import Data.Reflection
import Data.Time
import Data.Text
import qualified Data.Text.Lazy as Text.L
import Types
import Lucid
import MarkdownUtil
import ToHTML.Common
import ToHTML.Blog

data JBlog

blogIndexToYaml
  :: (Given Target, Given SiteUrl)
  => [Post Id Person] -> BS.ByteString
blogIndexToYaml posts =
  -- TODO this is also kinda bad
  encode $ object @JBlog
    [ "blog" .= object @JBlog
      [ "posts" .= posts ]
    ]

postToYaml
  :: (Given Target, Given SiteUrl)
  => Post Id Person -> BS.ByteString
postToYaml = encode . TaggedAeson @JBlog

instance (Given Target, Given SiteUrl) => ToJSON JBlog (Post Id Person) where
  toJSON Post{..} =
    object
      [ "id" .= idText postId,
        "title" .= Lucid.renderText (renderMMarkInline (titleMMark postTitle)),
        "published" .= object @JBlog
          [ "short" .= formatTime defaultTimeLocale "%b %-d" (pdPublished postDates),
            "full"  .= formatTime defaultTimeLocale "%B %-d, %Y" (pdPublished postDates)
          ],
        "author" .= postAuthor,
        "body" .= Lucid.renderText (hPostBody postBody)
      ]

instance (Given Target, Given SiteUrl) => ToJSON JBlog Person where
  toJSON Person{..} =
    let Name name = pName
        Alias alias = pAlias
    in
    object $ catMaybes
      [ Just ("name" .= name),
        Just ("alias" .= alias),
        pPic <&> \(PicFile pic) ->
          "pic" .= relativeLink ("/userpics/" <> pic),
        pLink <&> \(Link link) ->
          "link" .= link
      ]

----------------------------------------------------------------------------
-- Common instances
----------------------------------------------------------------------------

deriving via WithAeson Text instance ToJSON JBlog Text
deriving via WithAeson Text.L.Text instance ToJSON JBlog Text.L.Text
deriving via WithAeson Char instance ToJSON JBlog Char
deriving via WithAeson1 [] a instance ToJSON JBlog a => ToJSON JBlog [a]
