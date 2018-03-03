module MarkdownUtil where

import BasePrelude
import Data.Text as Text
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension as MMark
import Lucid

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
