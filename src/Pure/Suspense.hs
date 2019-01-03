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
      { construct = return (False,Null)
      , executing =
        void $ forkIO $ do
        Suspense {..} <- ask self
        threadDelay delay
        modify_ self $ \Suspense {..} (ran,v) ->
          if isJust value then (ran,v) else (True,fallback)
      , receive = \Suspense {..} (ran,v) -> return $
        case value of
          Just v              -> (ran,render v)
          Nothing | ran       -> (ran,v)
                  | otherwise -> (ran,fallback)
      , Pure.Data.View.render = \_ (_,v) -> v
      }

-- | `suspense` is like `maybe` with a delay parameter (in microseconds)
--
-- NOTE: does not correctly handle changes to delay, but should correctly
--       handle changes to fallback, render, and value.
suspense :: Typeable v => Int -> View -> (v -> View) -> Maybe v -> View
suspense delay fallback render value = View Suspense {..}

