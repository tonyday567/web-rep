module Web.Page.Html where

import Lucid
import Lucid.Base
import Data.Text
import Protolude
import Data.Generics.Product (field)
import Lens.Micro

toAtts :: [(Text, Text)] -> [Attribute]
toAtts hatts = (\(t, av) -> makeAttribute t av) <$> hatts
