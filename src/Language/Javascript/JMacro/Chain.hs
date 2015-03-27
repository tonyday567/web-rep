{-
A common pattern in javascript is to chain functions together using the js `.` operator, and jmacro falls a bit short on providing support for this.
-}
module Language.Javascript.JMacro.Chain where

import Language.Javascript.JMacro
import Data.Monoid

data ChainExpr = ChainExpr String (Maybe [JExpr]) deriving (Show)

data Chain = Chain { unwrap :: [ChainExpr] } deriving (Show)

instance Monoid Chain where
 mempty = Chain []
 mappend (Chain l) (Chain r) = Chain (r<>l) 

instance ToJExpr ChainExpr where
  toJExpr (ChainExpr a Nothing) = ValExpr (JVar (StrI a))
  toJExpr (ChainExpr a (Just es)) = ApplExpr (ValExpr (JVar (StrI a))) es

instance ToJExpr Chain where
  toJExpr (Chain []) = error "no mempty for a JExpr"
  toJExpr (Chain [x]) = toJExpr x
  toJExpr (Chain (x:xs)) =
    case x of
      (ChainExpr a Nothing) -> SelExpr (toJExpr (Chain xs)) (StrI a)
      (ChainExpr a (Just es)) -> ApplExpr (SelExpr (toJExpr (Chain xs)) (StrI a)) es

instance ToStat Chain where
  toStat = toStat . toJExpr

toChain :: JExpr -> Chain
toChain (ValExpr (JVar (StrI a))) = 
  Chain [ChainExpr a Nothing]
toChain (ApplExpr (ValExpr (JVar (StrI a))) es) = 
  Chain [ChainExpr a (Just es)]
toChain (SelExpr x (StrI a)) = 
  toChain x <> Chain [ChainExpr a Nothing]
toChain (ApplExpr (SelExpr x (StrI a)) es) =
  toChain x <> Chain [ChainExpr a (Just es)]
toChain x = Chain [ChainExpr (show $ renderJs x) Nothing]

chain :: String -> Chain
chain = toChain . jsv
