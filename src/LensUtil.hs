module LensUtil where

import BasePrelude
import Control.Lens

postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])
