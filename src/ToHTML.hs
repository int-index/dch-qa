module ToHTML where

import BasePrelude
import Data.Foldable as F (for_)
import Data.Reflection
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Time
import Types
import Lucid
import Lucid.Base
import qualified Text.MMark as MMark (render)

data Target = Web | Feed
  deriving (Eq, Show)

qaSessionToHtml
  :: (Given Target, Given SiteUrl)
  => QaSession Id Person -> Text
qaSessionToHtml = toStrict . renderText . hQaSession

hQaSession
  :: (Given Target, Given SiteUrl)
  => QaSession Id Person -> Html ()
hQaSession QaSession{..} = do
  let Id{..} = qassId
  case given @Target of
    Web -> details_ [class_ "qa-session", id_ idText] $ do
      summary_ (hHeader qassId qassTitle qassDate)
      hConversation qassConversation
    Feed -> div_ [id_ idText] $
      hConversation qassConversation

hHeader :: Id -> Title -> Day -> Html ()
hHeader Id{..} Title{..} day = do
  span_ [class_ "qa-header"] $ do
    span_ [class_ "qa-title"] $ do
      a_ [href_ ("#" <> idText)] (toHtml titleText)
    let timeString = formatTime defaultTimeLocale "%Y-%m-%d" day
    time_ [class_ "qa-time"] (toHtmlRaw timeString)

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
      div_ [class_ "qa-content"] (MMark.render msgContent)

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
  img_ [ class_ "qa-userpic",
         src_ (relativeLink picFile),
         srcset_ (relativeLink ("2x_" <> picFile) <> " 2x") ]

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

relativeLink :: (Given Target, Given SiteUrl) => Text -> Text
relativeLink url = case given @Target of
  Web -> url
  Feed -> siteUrl <> "/" <> url  -- for feeds, we want the link to be absolute
  where
    SiteUrl siteUrl = given

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | The @src@ attribute.
srcset_ :: Text -> Attribute
srcset_ = makeAttribute "srcset"
