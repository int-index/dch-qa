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
import MarkdownUtil
import Named

data Target = Web | Feed
  deriving (Eq, Show)

qaYearHeaderToHtml :: Integer -> Text
qaYearHeaderToHtml year = toStrict . renderText $
  h3_ [class_ "qa-year-header"] (toHtml (show year))

qaSessionToHtml
  :: (Given Target, Given SiteUrl)
  => QaSession Id Person -> Text
qaSessionToHtml = toStrict . renderText . hQaSession

hQaSession
  :: (Given Target, Given SiteUrl)
  => QaSession Id Person -> Html ()
hQaSession QaSession{..} = do
  let Id{..} = qassId
      Featured{..} = qassFeatured
      classes =
        [ "qa-session" ] <>
        [ "qa-session-featured" | featured ]
  case given @Target of
    Web -> details_ [classes_ classes, id_ idText] $ do
      summary_ (hHeader qassId qassTitle (qdAnswered qassDates))
      hConversation qassConversation
    Feed -> div_ [id_ idText] $
      hConversation qassConversation

hHeader :: Id -> Title -> Day -> Html ()
hHeader Id{..} Title{..} day = do
  span_ [class_ "qa-header"] $ do
    span_ [class_ "qa-title"] $ do
      a_ [href_ ("#" <> idText)] (renderMMarkInline titleMMark)
    let timeString = formatTime defaultTimeLocale "%b %-d" day
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
      div_ [class_ "qa-content"] $
        F.for_ msgContent $ \ContentPart{..} -> do
          F.for_ contentPartThumbnail hThumbnail
          let lazyload = case given @Target of
                Web -> True
                Feed -> False
          renderMMarkBlock (#lazyload lazyload) contentPartMMark

hThumbnail ::
  (Given Target, Given SiteUrl) =>
  Thumbnail ->
  Html ()
hThumbnail Thumbnail{..} = do
  let
    PicFile picFile = thumbnailPic
    inDir path = "thumbnails/" <> path
    sideClass =
      case thumbnailSide of
        SideLeft -> "qa-thumbnail-left"
        SideRight -> "qa-thumbnail-right"
    otherClasses = ["qa-thumbnail-" <> c | Class c <- toList thumbnailClass]
  div_ [classes_ ("qa-thumbnail" : sideClass : otherClasses)] $ do
    let img = lazyImg (#src   (relativeLink (inDir picFile)))
                      (#src2x (Just (relativeLink (inDir ("2x_" <> picFile)))))
    case thumbnailLink of
      Nothing -> img
      Just (Link url) -> a_ [href_ url] img
    F.for_ thumbnailCaption $ \(Caption caption) ->
      p_ [class_ "qa-thumbnail-caption"] (renderMMarkInline caption)

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
  let inDir path = "userpics/" <> path
  img_ [ class_ "qa-userpic",
         src_ (relativeLink (inDir picFile)),
         srcset_ (relativeLink (inDir ("2x_" <> picFile)) <> " 2x") ]

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

-- | A picture that can be loaded lazily with the @lazyload@ lib.
lazyImg
  :: Given Target
  => "src"   :! Text
  -> "src2x" :! Maybe Text
  -> Html ()
lazyImg (arg #src -> src) (arg #src2x -> mbSrc2x) = case given @Target of
  Web -> lazyload >> noscript_ ordinary
  Feed -> ordinary
  where
    ordinary = img_ $
      src_ src :
      [srcset_ (src2x <> " 2x") | Just src2x <- [mbSrc2x]]
    lazyload = img_ $
      data_ "src" src :
      [data_ "srcset" (src2x <> " 2x") | Just src2x <- [mbSrc2x]]

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | The @src@ attribute.
srcset_ :: Text -> Attribute
srcset_ = makeAttribute "srcset"
