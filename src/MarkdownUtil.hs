{-# LANGUAGE QuasiQuotes #-}

module MarkdownUtil where

import BasePrelude
import Data.List.NonEmpty
import Data.Text as Text
import Text.MMark as MMark
import Text.MMark.Extension as MMark
import Text.MMark.Extension.Skylighting as MMark
import Lucid
import Text.URI as URI
import Text.URI.QQ (uri)
import Named

----------------------------------------------------------------------------
-- Main methods
----------------------------------------------------------------------------

-- | Parse Markdown, and 'error' out if there has been an error. We don't
-- want to use 'fail' or 'throw' because then (e.g. in Aeson) we sometimes
-- try other alternatives when parsing fails, while we really shouldn't.
parseMMark :: Text -> MMark
parseMMark text =
  case MMark.parse "" text of
    Left e -> error (MMark.parseErrorsPretty text e)
    Right a -> a

-- | Render a block of Markdown with our preferred extensions.
renderMMarkBlock :: Flag "lazyload" -> MMark -> Html ()
renderMMarkBlock (Flag lazyload) =
  MMark.render .
  MMark.useExtensions
    ([lazyloadExt | lazyload] ++ [MMark.skylighting, nowrapExt, shortcutLinksExt])

-- | Render inline Markdown with our preferred extensions.
--
-- If there are elements other than paragraphs, you HTML will likely be
-- broken.
renderMMarkInline :: MMark -> Html ()
renderMMarkInline =
  MMark.render .
  MMark.useExtensions [nowrapExt, inlineExt, shortcutLinksExt]

----------------------------------------------------------------------------
-- Extensions that we use
----------------------------------------------------------------------------

-- | Don't wrap around:
--
-- * Code strings that are 10 characters or shorter
-- * GHC extension names (“-X...”)
--
nowrapExt :: MMark.Extension
nowrapExt = MMark.inlineRender $ \old node ->
  case node of
    MMark.CodeSpan code
      | Text.length code <= 10      -> nowrapCode code
      | "-X" `Text.isPrefixOf` code -> nowrapCode code
    _ -> old node
  where
    nowrapCode :: Text -> Html ()
    nowrapCode x = code_ [style_ "white-space: nowrap"] (toHtml x)

-- | Render all paragraphs without wrapping tags. Useful for e.g. captions
-- or post titles.
--
-- If there are elements other than paragraphs, you HTML will likely be
-- broken.
inlineExt :: MMark.Extension
inlineExt = MMark.blockTrans $ \case
  MMark.Paragraph inlines -> MMark.Naked inlines
  other -> other

-- | Allow some links to be specified in shortened form.
--
-- * To link to a Hackage package, write @[lens](hackage)@.
--    The result will be monospaced.
shortcutLinksExt :: MMark.Extension
shortcutLinksExt = MMark.inlineTrans $ \case
  MMark.Link content link title
    | link == [uri|hackage|]
    , Just pkg <- getPackage content
    , Just link' <- hackageLink pkg
        -> MMark.Link (MMark.CodeSpan pkg :| []) link' title
  other -> other
  where
    getPackage = \case
      (MMark.Plain pkg :| [])    -> Just pkg
      (MMark.CodeSpan pkg :| []) -> Just pkg
      _                          -> Nothing
    hackageLink pkg = do
      pkgUri <- mkURI pkg
      pkgUri `relativeTo` [uri|https://hackage.haskell.org/package/|]

-- | Transform all images to load them lazily (assuming the @lazyload@
-- library is enabled).
lazyloadExt :: MMark.Extension
lazyloadExt = MMark.inlineRender $ \old node ->
  case node of
    MMark.Image desc srcUri mtitle ->
      let title = maybe [] (pure . title_) mtitle
          src   = URI.render srcUri
          alt   = asPlainText desc
      in  img_ (alt_ alt : data_ "src" src : title) >>
          noscript_ (img_ (alt_ alt : src_ src : title))
    _ -> old node
