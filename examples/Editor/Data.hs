{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Editor.Data (
  Definition(..), atDefParameters, atDefBody,
  Builtin(..),
  Parameter(..),
  VariableRef(..), onVariableIRef,
  Apply(..), atApplyFunc, atApplyArg,
  HoleState(..),
    emptyHoleState, atHoleSearchTerm, atHoleCachedSearchResults,
  Expression(..))
where

import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.Store.IRef (IRef)
import qualified Data.AtFieldTH as AtFieldTH

data Apply = Apply {
  applyFunc :: IRef Expression,
  applyArg :: IRef Expression
  }
  deriving (Eq, Ord, Read, Show)

data HoleState = HoleState {
  holeSearchTerm :: String,
  holeCachedSearchResults :: [VariableRef]
  }
  deriving (Eq, Ord, Read, Show)

emptyHoleState :: HoleState
emptyHoleState = HoleState "" []

data Expression =
  ExpressionApply Apply |
  ExpressionGetVariable VariableRef |
  ExpressionHole HoleState
  deriving (Eq, Ord, Read, Show)

data Parameter = Parameter
  deriving (Eq, Ord, Read, Show)

data Definition = Definition {
  defParameters :: [IRef Parameter],
  defBody :: IRef Expression
  }
  deriving (Eq, Ord, Read, Show)

data Builtin = Builtin {
  biModule :: [String],
  biName :: String
  }
  deriving (Eq, Ord, Read, Show)

data VariableRef =
  ParameterRef (IRef Parameter) |
  DefinitionRef (IRef Definition) |
  BuiltinRef (IRef Builtin)
  deriving (Eq, Ord, Read, Show)

onVariableIRef :: (forall a. IRef a -> b) -> VariableRef -> b
onVariableIRef f (ParameterRef i) = f i
onVariableIRef f (DefinitionRef i) = f i
onVariableIRef f (BuiltinRef i) = f i

derive makeBinary ''Apply
derive makeBinary ''Builtin
derive makeBinary ''HoleState
derive makeBinary ''Expression
derive makeBinary ''Parameter
derive makeBinary ''Definition
derive makeBinary ''VariableRef
AtFieldTH.make ''Definition
AtFieldTH.make ''Apply
AtFieldTH.make ''HoleState
