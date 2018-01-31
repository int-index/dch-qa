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
qaSessionToHTML = toStrict . renderText . hHtmlWrap . hQaSession

hHtmlWrap :: Html () -> Html ()
hHtmlWrap inner = do
  head_ $ do
    link_ [href_ "css.css", rel_ "stylesheet"]
    link_ [href_ "qa.css", rel_ "stylesheet"]
    link_ [href_ "https://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400", rel_ "stylesheet"]
    link_ [href_ "https://fonts.googleapis.com/css?family=Overpass+Mono:300&amp;subset=latin-ext", rel_ "stylesheet"]
    meta_ [charset_ "UTF-8"]
  body_ inner

hQaSession :: QaSession Person -> Html ()
hQaSession QaSession{..} = div_ [class_ "qa-session"] $ do
  hHeader qassTitle qassDate
  hConversation qassConversation

hHeader :: Title -> Day -> Html ()
hHeader Title{..} day = div_ [class_ "qa-header"] $ do
  h2_ [class_ "qa-title"] (toHtml titleText)
  let timeString = formatTime defaultTimeLocale "%Y-%m-%d" day
  time_ [class_ "qa-time"] (toHtmlRaw timeString)

hConversation :: Conversation Person -> Html ()
hConversation Conversation{..} =
  F.for_ conversationMessages $ \Message{..} -> do
    let Person{..} = msgAuthor
    div_ [class_ ("qa-message " <> roleClass pRole)] $ do
      let Alias alias = pAlias
      p_ [class_ "qa-author"] (toHtml alias)
      div_ [class_ "qa-content"] (MMark.render msgContent)

roleClass :: Role -> Text
roleClass Client     = "qa-role-client"
roleClass Consultant = "qa-role-consultant"
