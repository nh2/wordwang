{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Common () where

import           Control.Applicative ((<$>))
import           Control.Arrow (first)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Data.Aeson

instance ToJSON v => ToJSON (Map Int v) where
    toJSON = toJSON . Map.fromList . map (first show) . Map.toList

instance FromJSON v => FromJSON (Map Int v) where
    parseJSON j = (Map.fromList . map (first read) . Map.toList) <$> parseJSON j
