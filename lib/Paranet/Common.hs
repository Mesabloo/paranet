{-# LANGUAGE DeriveGeneric #-}

module Paranet.Common where

import Data.Aeson
import Data.Int (Int64)

import GHC.Generics

data Expression
    = EInt Integer
    | EId String
    | Expression :| Expression
    | Expression :& Expression
    | Expression :~ Expression
    | Expression :-> Expression
  deriving (Show, Generic)

instance ToJSON Expression
instance FromJSON Expression

data Message
    = Send -- ^ When a client sends some expression to the server
    { calc :: Expression
    , mid  :: MessageID
    }
    | GiveAndReceive -- ^ When the server gives some expression to a client to be calculated
    { from :: String -- ^ The client who sent the 'Send' payload
    , to   :: String -- ^ The client receiving this payload (might be the same)
    , calc :: Expression -- ^ The expression to be evaluated
    , mid  :: MessageID
    }
    | Give -- ^ When a client has evaluated an expression and gives it back to the server
    { to   :: String -- ^ The client who the expression has been evaluated for
    , calc :: Expression -- ^ The result of the computation
    , mid  :: MessageID
    }
    | Receive -- ^ When the server gives back the result of a computation to the original client
    { from :: String -- ^ The client who computed the expression
    , calc :: Expression -- ^ The result
    , mid  :: MessageID
    }
  deriving (Show, Generic)

data MessageID
    = MID
    { nb   :: Integer
    , sent :: Integer
    }
  deriving (Show, Generic, Eq)

instance ToJSON MessageID
instance FromJSON MessageID

instance ToJSON Message
instance FromJSON Message

maxMessageSize :: Int64
maxMessageSize = 32768

class Pretty a where
    pretty :: a -> String

instance Pretty Expression where
    pretty (EInt i) = show i
    pretty (EId i) = i
    pretty (e1 :& e2) = pretty e1 <> " & " <> pretty_ e2
    pretty (e1 :| e2) = pretty_ e1 <> " | " <> pretty e2
    pretty (e1 :~ e2) = pretty e1 <> " ~ " <> pretty_ e2
    pretty (e1 :-> e2) = pretty e1 <> " -> " <> pretty_ e2

pretty_ :: Expression -> String
pretty_ e@(EInt _) = pretty e
pretty_ e@(EId _) = pretty e
pretty_ e = "(" <> pretty e <> ")"
