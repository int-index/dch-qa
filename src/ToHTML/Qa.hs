module ToHTML.Qa where

import BasePrelude
import Data.Foldable as F (for_)
import Data.Reflection
import Data.Text
import Types
import Lucid
import MarkdownUtil
import ToHTML.Common

hConversation
  :: (Given Target, Given SiteUrl)
  => Conversation Person -> Html ()
hConversation Conversation{..} =
  F.for_ conversationMessages $ \Message{..} -> do
    let Person{..} = msgAuthor
        classes =
          [ "qa-message",
            roleClass pRole,
            highlightClass msgHighlight ]
    div_ [classes_ classes] $ do
      hMessageAuthor msgAuthor
      div_ [class_ "qa-content"] $
        F.for_ msgContent $ \ContentPart{..} -> do
          F.for_ contentPartThumbnail hThumbnail
          let lazyload = case given @Target of
                Web -> True
                Feed -> False
          renderMMarkBlock (#lazyload lazyload) contentPartMMark

hMessageAuthor
  :: (Given Target, Given SiteUrl)
  => Person -> Html ()
hMessageAuthor Person{..} = do
  let Alias alias = pAlias
  p_ [class_ "qa-author"] $ hAuthorLink pLink $
    case given @Target of
      Web -> do
        traverse_ hAuthorPic pPic
        span_ [class_ "qa-username"] (toHtml alias)
      Feed -> do
        traverse_ hAuthorPic pPic >> " "
        strong_ (span_ [class_ "qa-username"] (toHtml alias) >> ":")

hAuthorPic
  :: (Given Target, Given SiteUrl)
  => Pic -> Html ()
hAuthorPic (PicFile picFile) = do
  let inDir path = "/userpics/" <> path
  case given @Target of
    Web ->
      img_ [ class_ "qa-userpic",
             src_ (relativeLink (inDir picFile)) ]
    Feed ->
      img_ [ class_ "qa-userpic",
             src_ (relativeLink (inDir picFile)),
             width_ "20px" ]

hAuthorLink :: Maybe Link -> Html () -> Html ()
hAuthorLink = \case
  Nothing -> id
  Just (Link link) -> a_ [href_ link]

roleClass :: Role -> Text
roleClass = \case
  Client -> "qa-role-client"
  Consultant -> "qa-role-consultant"

highlightClass :: Highlight -> Text
highlightClass = \case
  Highlight True -> "qa-msg-highlight"
  Highlight False -> "qa-msg-casual"
