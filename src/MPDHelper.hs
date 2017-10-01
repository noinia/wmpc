{-# LANGUAGE OverloadedStrings #-}
module MPDHelper where

import qualified Network.MPD as MPD
import qualified Data.Map as Map
import Data.Maybe(listToMaybe)

-- instance ToHtml MPD.Value where

lookupTagOf    :: MPD.Song -> MPD.Metadata -> Maybe MPD.Value
lookupTagOf s k = lookupTagOf' s k >>= listToMaybe

lookupTagOf'     :: MPD.Song -> MPD.Metadata -> Maybe [MPD.Value]
lookupTagOf' s k = Map.lookup k . MPD.sgTags $ s
