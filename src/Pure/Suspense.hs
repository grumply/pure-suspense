{-# LANGUAGE RecordWildCards #-}
module Pure.Suspense (Suspense(..),suspense) where

import Pure.Data.Default
import Pure.Data.View
import Pure.Data.View.Patterns

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.Typeable

data Suspense v = Suspense
  { delay    :: Int
  , fallback :: View
  , render   :: v -> View
  , value    :: Maybe v
  }

instance Typeable v => Pure (Suspense v) where
  view = LibraryComponentIO $ \self ->
    def
      { construct = do
          Suspense {..} <- ask self
          return (fmap render value)
      , executing = void $ forkIO $ do
        Suspense {..} <- ask self
        threadDelay delay
        modify_ self $ \Suspense {..} ->
          maybe (Just fallback) Just
      , receive = \Suspense {..} _ ->
          return (fmap render value)
      , Pure.Data.View.render = \_ ->
          fromMaybe Null
      }

-- | `suspense` displays a fallback if a value has not become `Just` after a
-- given delay.
--
-- NOTE: does not correctly handle changes to delay, but should correctly
--       handle changes to fallback, render, and value.
suspense :: Typeable v => Int -> View -> (v -> View) -> Maybe v -> View
suspense delay fallback render value = View Suspense {..}

