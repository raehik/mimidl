module Util.Text where

import Data.Text qualified

tshow :: Show a => a -> Data.Text.Text
tshow = Data.Text.pack . show
