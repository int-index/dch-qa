module ToHTML where

import Data.Foldable as F (for_)
import Data.Monoid
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Time
import QaSession
import Lucid
import qualified Text.MMark as MMark (render)

qaSessionToHTML :: QaSession Person -> Text
qaSessionToHTML = toStrict . renderText . hQaSession

hQaSession :: QaSession Person -> Html ()
hQaSession QaSession{..} = details_ [class_ "qa-session"] $ do
  summary_ (hHeader qassTitle qassDate)
  hConversation qassConversation

hHeader :: Title -> Day -> Html ()
hHeader Title{..} day = span_ [class_ "qa-header"] $ do
  span_ [class_ "qa-title"] (toHtml titleText)
  let timeString = formatTime defaultTimeLocale "%Y-%m-%d" day
  time_ [class_ "qa-time"] (toHtmlRaw timeString)

hConversation :: Conversation Person -> Html ()
hConversation Conversation{..} =
  F.for_ conversationMessages $ \Message{..} -> do
    let Person{..} = msgAuthor
    div_ [class_ ("qa-message " <> roleClass pRole)] $ do
      let Alias alias = pAlias
      p_ [class_ "qa-author"] $ hAuthorLink pLink $ do
        F.for_ pPic $ \(PicUrl link) ->
          img_ [src_ link]
        span_ (toHtml alias)
      div_ [class_ "qa-content"] (MMark.render msgContent)

hAuthorLink :: Maybe Link -> Html () -> Html ()
hAuthorLink Nothing = id
hAuthorLink (Just (Link link)) = a_ [href_ link]

roleClass :: Role -> Text
roleClass Client     = "qa-role-client"
roleClass Consultant = "qa-role-consultant"
