module ToHTML where

import Data.Foldable as F (for_)
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Time
import QaSession
import Lucid
import qualified Text.MMark as MMark (render)

qaSessionToHTML :: QaSession Person -> Text
qaSessionToHTML = toStrict . renderText . hQaSession

hQaSession :: QaSession Person -> Html ()
hQaSession QaSession{..} = (div_ `with` [class_ "qa-session"]) $ do
  hHeader qassTitle qassDate
  hConversation qassConversation

hHeader :: Title -> Day -> Html ()
hHeader Title{..} day = (div_ `with` [class_ "qa-header"]) $ do
  (h2_ `with` [class_ "qa-title"]) (toHtml titleText)
  let timeString = formatTime defaultTimeLocale "%Y-%m-%d" day
  (time_ `with` [class_ "qa-time"]) (toHtmlRaw timeString)

hConversation :: Conversation Person -> Html ()
hConversation Conversation{..} =
  F.for_ conversationMessages $ \Message{..} ->
    (p_ `with` [class_ "qa-message"]) $ do
      let Alias alias = pAlias msgAuthor
      (span_ `with` [class_ "qa-author"]) (toHtml alias)
      (span_ `with` [class_ "qa-message"]) (MMark.render msgContent)
