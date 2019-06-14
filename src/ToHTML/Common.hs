module ToHTML.Common where

import BasePrelude
import System.FilePath.Posix ((</>))
import Data.Foldable as F (for_)
import Data.Reflection
import Data.Text
import Types
import Lucid
import Lucid.Base
import MarkdownUtil
import Named

data Target = Web | Feed
  deriving (Eq, Show)

hThumbnail ::
  (Given Target, Given SiteUrl) =>
  Thumbnail ->
  Html ()
hThumbnail Thumbnail{..} = do
  let
    PicFile picFile = thumbnailPic
    inDir path = "/thumbnails/" <> path
    sideClass =
      case thumbnailSide of
        SideLeft -> "thumbnail-left"
        SideRight -> "thumbnail-right"
    otherClasses = ["thumbnail-" <> c | Class c <- toList thumbnailClass]
  div_ [classes_ ("thumbnail" : sideClass : otherClasses)] $ do
    let img = lazyImg (#src   (relativeLink (inDir picFile)))
                      (#src2x (Just (relativeLink (inDir ("2x_" <> picFile)))))
    case thumbnailLink of
      Nothing -> img
      Just (Link url) -> a_ [href_ url] img
    F.for_ thumbnailCaption $ \(Caption caption) ->
      p_ [class_ "thumbnail-caption"] (renderMMarkInline caption)

relativeLink :: (Given Target, Given SiteUrl) => Text -> Text
relativeLink url = case given @Target of
  Web -> url
  Feed -> pack (unpack siteUrl </> unpack url) -- for feeds, we want the link to be absolute
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

-- | The @src@ attribute.
srcset_ :: Text -> Attribute
srcset_ = makeAttribute "srcset"
