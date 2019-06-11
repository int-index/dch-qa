module ToYAML.Qa where

import BasePrelude as P
import Control.Lens as L hiding ((.=))
import qualified Data.ByteString as BS
import Data.List.NonEmpty as NonEmpty
import Data.Yaml
import Data.Reflection
import Data.Time
import Types
import Lucid
import MarkdownUtil
import ToHTML.Common
import ToHTML.Qa

qaSessionsToYaml
  :: (Given Target, Given SiteUrl)
  => [QaSession Id Person] -> BS.ByteString
qaSessionsToYaml qaSessions =
  let
    groupToYaml sessions = object
      [ "year" .= qassYear (NonEmpty.head sessions),
        "sessions" .= NonEmpty.map yQaSession sessions
      ]
  in
    encode $
    object
      [ "by_year" .= P.map groupToYaml groupedQaSessions
      ]
  where
    qassYear = L.view _1 . toGregorian . qdAnswered . qassDates
    groupedQaSessions =
      NonEmpty.groupWith qassYear $
      P.sortOn (Down . qdAnswered . qassDates &&& qassId) qaSessions

yQaSession
  :: (Given Target, Given SiteUrl)
  => QaSession Id Person -> Value
yQaSession QaSession{..} =
  object
    [ "id" .= idText qassId,
      "featured" .= featured qassFeatured,
      "title" .= Lucid.renderText (renderMMarkInline (titleMMark qassTitle)),
      "answered" .= formatTime defaultTimeLocale "%b %-d" (qdAnswered qassDates),
      "body" .= Lucid.renderText (hConversation qassConversation)
    ]
