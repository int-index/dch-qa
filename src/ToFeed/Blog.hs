module ToFeed.Blog where

import BasePrelude
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.L
import Data.Generics.Uniplate.Data
import Data.Text
import qualified Data.Text.Lazy as Text.L
import Data.Time
import Data.Reflection
import System.FilePath.Posix ((</>))
import Types
import ToHTML.Common (Target(..))
import ToHTML.Blog (postToHtml)
import MarkdownUtil (renderMMarkInline)
import Lucid
import qualified Text.Atom.Feed        as Atom
import qualified Text.Atom.Feed.Export as Atom
import qualified Text.Feed.Types as Feed
import qualified Text.Feed.Util  as Feed
import qualified Text.XML.Unresolved as XML
import qualified Data.XML.Types      as XML

blogToFeed :: Given SiteUrl => [Post Id Person] -> BS.ByteString
blogToFeed =
  BS.L.toStrict .
  XML.renderLBS XML.def .
  -- see https://validator.w3.org/feed/docs/warning/AvoidNamespacePrefix.html
  transformBi (\name -> name {XML.nameNamespace = Nothing}) .
  mkDocument .
  Atom.xmlFeed .
  blogFeed
  where
    mkDocument element =
      XML.Document
        { XML.documentPrologue = XML.Prologue [] Nothing [],
          XML.documentRoot     = element,
          XML.documentEpilogue = []
        }

blogFeed :: Given SiteUrl => [Post Id Person] -> Atom.Feed
blogFeed items =
  feedBase
    { Atom.feedEntries = fPost <$> sortedItems,
      Atom.feedLinks = [Atom.nullLink fUrl]
    }
  where
    SiteUrl siteUrl = given
    sortedItems =
      sortOn (Down . pdPublished . postDates &&& postId) items
    fUrl = pack (unpack siteUrl </> "blog")
    fLastUpdate =
      case sortedItems of
        item:_ ->
          Feed.toFeedDateStringUTC
            Feed.AtomKind
            (UTCTime (pdPublished (postDates item)) 0)
        _ -> ""
    feedBase =
      Atom.nullFeed
        fUrl
        (Atom.TextString "Monadfix blog")
        (pack fLastUpdate)

fPost :: Given SiteUrl => Post Id Person -> Atom.Entry
fPost post@Post{..} =
  entryBase
    { Atom.entryLinks = [Atom.nullLink fUrl],
      Atom.entryContent = Just (Atom.HTMLContent fContent)
    }
  where
    SiteUrl siteUrl = given
    fContent = give @Target Feed $ postToHtml post
    fDate = UTCTime (pdPublished postDates) 0
    fUrl = pack (unpack siteUrl </> "blog" </> unpack (idText postId))
    entryBase =
      Atom.nullEntry
        (idText postId)
        (Atom.HTMLString $ Text.L.toStrict $ Lucid.renderText $
           renderMMarkInline $ titleMMark postTitle)
        (pack (Feed.toFeedDateStringUTC Feed.AtomKind fDate))
