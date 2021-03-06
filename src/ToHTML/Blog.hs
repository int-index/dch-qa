module ToHTML.Blog where

import BasePrelude
import Data.Foldable as F (for_)
import Data.Reflection
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Time
import Types
import Lucid
import MarkdownUtil
import ToHTML.Common

postToHtml
  :: (Given Target, Given SiteUrl)
  => Post Id Person -> Text
postToHtml = toStrict . renderText . hPost

-- TODO get rid of
hPost
  :: (Given Target, Given SiteUrl)
  => Post Id Person -> Html ()
hPost Post{..} = do
  let Id{..} = postId
      Title{..} = postTitle
      PostDates{..} = postDates
  case given @Target of
    Web -> h1_ [class_ "post-title"] (renderMMarkInline titleMMark)
    Feed -> pure ()
  p_ [class_ "post-meta"] $ do
    hPostAuthor postAuthor
    case given @Target of
      Web -> do
        let timeString = formatTime defaultTimeLocale "%b %-d" pdPublished
        span_ [class_ "post-published"] $
          "Published on " <> time_ (toHtmlRaw timeString)
      Feed -> pure ()
  div_ [class_ "post-body"] $
    hPostBody postBody

hPostBody
  :: (Given Target, Given SiteUrl)
  => PostBody -> Html ()
hPostBody PostBody{..} =
  F.for_ postBodyPieces $ \ContentPart{..} -> do
    F.for_ contentPartThumbnail hThumbnail
    let lazyload = case given @Target of
          Web -> True
          Feed -> False
    renderMMarkBlock (#lazyload lazyload) contentPartMMark

hPostAuthor
  :: (Given Target, Given SiteUrl)
  => Person -> Html ()
hPostAuthor Person{..} = do
  let Alias alias = pAlias
  span_ [class_ "post-author"] $ hAuthorLink pLink $
    case given @Target of
      Web -> do
        traverse_ hAuthorPic pPic
        span_ [class_ "post-username"] (toHtml alias)
      Feed -> do
        traverse_ hAuthorPic pPic >> " "
        strong_ (span_ [class_ "post-username"] (toHtml alias))

hAuthorPic
  :: (Given Target, Given SiteUrl)
  => Pic -> Html ()
hAuthorPic (PicFile picFile) = do
  let inDir path = "/userpics/" <> path
  case given @Target of
    Web ->
      img_ [ class_ "post-userpic",
             src_ (relativeLink (inDir picFile)) ]
    Feed ->
      img_ [ class_ "post-userpic",
             src_ (relativeLink (inDir picFile)),
             width_ "20px" ]

hAuthorLink :: Maybe Link -> Html () -> Html ()
hAuthorLink = \case
  Nothing -> id
  Just (Link link) -> a_ [href_ link]
