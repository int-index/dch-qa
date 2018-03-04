module MarkdownUtil where

import BasePrelude
import Data.Text as Text
import Text.MMark as MMark
import Text.MMark.Extension as MMark
import Text.MMark.Extension.Skylighting as MMark
import Lucid

----------------------------------------------------------------------------
-- Main methods
----------------------------------------------------------------------------

-- | Parse Markdown, and 'fail' if there have been any errors.
parseMMark :: Monad m => Text -> m MMark
parseMMark text =
  case MMark.parse "" text of
    Left e -> fail (MMark.parseErrorsPretty text e)
    Right a -> return a

-- | Render a block of Markdown with our preferred extensions.
renderMMarkBlock :: MMark -> Html ()
renderMMarkBlock =
  MMark.render .
  MMark.useExtensions [MMark.skylighting, nowrapExt]

-- | Render inline Markdown with our preferred extensions.
--
-- If there are elements other than paragraphs, you HTML will likely be
-- broken.
renderMMarkInline :: MMark -> Html ()
renderMMarkInline =
  MMark.render .
  MMark.useExtensions [nowrapExt, inlineExt]

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
