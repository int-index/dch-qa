module ToYAML.Blog where

import BasePrelude as P
import Data.Yaml
import qualified Data.ByteString as BS
import Data.Reflection
import Data.Time
import Types
import Lucid
import MarkdownUtil
import ToHTML.Common
import ToHTML.Blog

blogIndexToYaml
  :: (Given Target, Given SiteUrl)
  => [Post Id Person] -> BS.ByteString
blogIndexToYaml posts =
  encode $ object
    [ "blog" .= object
      [ "posts" .= P.map yPost posts ]
    ]

postToYaml
  :: (Given Target, Given SiteUrl)
  => Post Id Person -> BS.ByteString
postToYaml = encode . yPost

yPost
  :: (Given Target, Given SiteUrl)
  => Post Id Person -> Value
yPost Post{..} =
  object
    [ "id" .= idText postId,
      "title" .= Lucid.renderText (renderMMarkInline (titleMMark postTitle)),
      "published" .= formatTime defaultTimeLocale "%b %-d" (pdPublished postDates),
      "author" .= yPerson postAuthor,
      "body" .= Lucid.renderText (hPostBody postBody)
    ]

yPerson
  :: (Given Target, Given SiteUrl)
  => Person -> Value
yPerson Person{..} =
  let Name name = pName
      Alias alias = pAlias
  in
  object $ catMaybes
    [ Just ("name" .= name),
      Just ("alias" .= alias),
      pPic <&> \(PicFile pic) ->
        "pic" .= relativeLink ("userpics/" <> pic),
      pLink <&> \(Link link) ->
        "link" .= link
    ]
