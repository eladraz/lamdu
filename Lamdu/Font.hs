{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Font
    ( Fonts(..), with
    ) where

import qualified Control.Exception as E
import           Control.Monad.Trans.Cont (ContT(..))
import qualified Data.Aeson.Types as Aeson
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import qualified System.Directory as Directory

import           Prelude.Compat

data MissingFont = MissingFont FilePath deriving (Show, Typeable)
instance E.Exception MissingFont

data Fonts a = Fonts
    { fontDefault :: a
    , fontFancy :: a
    , fontAutoName :: a
    , fontMono :: a
    } deriving (Eq, Generic, Show, Functor, Foldable, Traversable)
instance Aeson.ToJSON a => Aeson.ToJSON (Fonts a) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON a => Aeson.FromJSON (Fonts a)

withPath :: E.Exception e => (e -> IO a) -> Int -> FilePath -> (Draw.Font -> IO a) -> IO a
withPath catchError size path action =
    do
        exists <- Directory.doesFileExist path
        if exists
            then Draw.withFontCatch catchError size path action
            else
                let err = MissingFont $ path ++ " does not exist!"
                in E.throwIO err `E.catch` catchError

with :: E.Exception e => (e -> IO a) -> Int -> Fonts FilePath -> (Fonts Draw.Font -> IO a) -> IO a
with catchError size = runContT . traverse (ContT . withPath catchError size)
