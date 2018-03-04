module ToFeed where

import BasePrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.L
import Data.Generics.Uniplate.Data
import Data.Text
import qualified Data.Text.Lazy as Text.L
import Data.Time
import Data.Reflection
import Types
import ToHTML (qaSessionToHtml, Target(..))
import MarkdownUtil (renderMMarkInline)
import Lucid
import qualified Text.Atom.Feed        as Atom
import qualified Text.Atom.Feed.Export as Atom
import qualified Text.Feed.Types as Feed
import qualified Text.Feed.Util  as Feed
import qualified Text.XML.Unresolved as XML
import qualified Data.XML.Types      as XML

qaSessionsToFeed :: Given SiteUrl => [QaSession Id Person] -> BS.ByteString
qaSessionsToFeed =
  BS.L.toStrict .
  XML.renderLBS XML.def .
  -- see https://validator.w3.org/feed/docs/warning/AvoidNamespacePrefix.html
  transformBi (\name -> name {XML.nameNamespace = Nothing}) .
  mkDocument .
  Atom.xmlFeed .
  qaFeed
  where
    mkDocument element =
      XML.Document
        { XML.documentPrologue = XML.Prologue [] Nothing [],
          XML.documentRoot     = element,
          XML.documentEpilogue = []
        }

qaFeed :: Given SiteUrl => [QaSession Id Person] -> Atom.Feed
qaFeed items =
  feedBase
    { Atom.feedEntries = fQaSession <$> sortedItems,
      Atom.feedLinks = [Atom.nullLink fUrl]
    }
  where
    SiteUrl siteUrl = given
    sortedItems =
      sortOn (Down . qassDate &&& qassId) items
    fUrl = siteUrl <> "#library"
    fLastUpdate =
      case sortedItems of
        item:_ ->
          Feed.toFeedDateStringUTC
            Feed.AtomKind
            (UTCTime (qassDate item) 0)
        _ -> ""
    feedBase =
      Atom.nullFeed
        fUrl
        (Atom.TextString "Dirt Cheap Haskell: Library")
        (pack fLastUpdate)

fQaSession :: Given SiteUrl => QaSession Id Person -> Atom.Entry
fQaSession session@QaSession{..} =
  entryBase
    { Atom.entryLinks = [Atom.nullLink fUrl],
      Atom.entryContent = Just (Atom.HTMLContent fContent)
    }
  where
    SiteUrl siteUrl = given
    fContent = give @Target Feed $ qaSessionToHtml session
    fDate = UTCTime qassDate 0
    fUrl = siteUrl <> "#" <> idText qassId
    entryBase =
      Atom.nullEntry
        (idText qassId)
        (Atom.HTMLString $ Text.L.toStrict $ Lucid.renderText $
           renderMMarkInline $ titleMMark qassTitle)
        (pack (Feed.toFeedDateStringUTC Feed.AtomKind fDate))
