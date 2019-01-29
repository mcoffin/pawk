module Main
    ( main
    ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (drop)
import Data.Either (either)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (maybe)
import Data.Monoid (class Monoid, mempty)
import Effect.Console (log)
import Effect.Exception (error, throw)
import Effect (Effect)
import Minimist (Arg(..), parseArgs)
import Node.Process (argv)
import Foreign.Object as O

joinWithSeparator :: forall a f. Eq a => Foldable f => Monoid a => a -> f a -> a
joinWithSeparator sep = foldl combine mempty
    where
        combine fst
          | fst == mempty = identity
          | otherwise = append $ fst <> sep

validate :: forall m e a. MonadThrow e m => Monoid a => Eq a => (a -> Boolean) -> e -> a -> m a
validate f e v
  | f v = pure v
  | otherwise = throwError e

main = flip parseArgs mempty <$> drop 2 <$> argv >>= \args -> do
    cmd <- maybe (throw "No command specified.") pure (O.lookup "_" args) >>=
        \arg -> case arg of
                  ArgArray a -> pure a
                  _ -> throw "Malformatted command received from minimist"
        <#> map show <#>
        joinWithSeparator " " >>=
        (either throw pure <$> validate (not eq mempty) "No command specified")
    log cmd
