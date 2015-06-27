module Control.Process where

import Prelude

import Data.Monoid
import Data.Exists
import qualified Data.List as L

import Control.Monad.Eff

data AwaitF f a r = AwaitF (f r) (r -> Process f a) (Process f a)

data Process f a 
  = Halt
  | Emit a (Process f a)
  | Await (Exists (AwaitF f a))

instance processSemigroup :: Semigroup (Process k a) where
  append Halt p = p
  append (Emit h t) p2 = Emit h (t <> p2)
  append (Await f) p2 = Await (runExists (\(AwaitF req recv fb) -> mkExists (AwaitF req ((`append` p2) <<< recv) (append fb p2))) f)

instance processMonoid :: Monoid (Process k a) where
  mempty = Halt

instance processFunctor :: Functor (Process f) where
  map f Halt = Halt
  map f (Emit h t) = Emit (f h) (map f t)
  map f (Await g) = Await (runExists (\(AwaitF req recv fb) -> mkExists (AwaitF req (map f <<< recv) (map f fb))) g)

instance processApply :: Apply (Process f) where
  apply Halt b = Halt
  apply (Emit h t) b = (h <$> b) <> (t <*> b)
  apply (Await f) b = Await (runExists (\(AwaitF req recv fb) -> mkExists (AwaitF req ((`apply` b) <<< recv) (apply fb b))) f)

instance processApplicative :: Applicative (Process f) where
  pure a = Emit a Halt

instance processBind :: Bind (Process f) where
  bind Halt f = Halt
  bind (Emit h t) f = f h <> (bind t f)
  bind (Await g) f = Await (runExists (\(AwaitF req recv fb) -> mkExists (AwaitF req (\res -> bind (recv res) f) (bind fb f))) g)

instance processMonad :: Monad (Process f)

type Source e a = Process (Eff e) a
type Sink e a = Source e (a -> Eff e Unit)
type Channel e a b = Source e (a -> Eff e b)

await :: forall f r a. f r -> (r -> Process f a) -> Process f a
await req recv = Await (mkExists (AwaitF req recv Halt))

emit :: forall f a. a -> Process f a
emit h = Emit h Halt

eval :: forall f a. f a -> Process f a
eval h = await h emit

translate :: forall f g a. (forall b. f b -> g b) -> Process f a -> Process g a
translate _ Halt = Halt
translate f (Emit h t) = Emit h (translate f t)
translate f (Await g) = Await (runExists (\(AwaitF req recv fb) -> mkExists (AwaitF (f req) (translate f <<< recv) (translate f fb))) g) 

flatten :: forall f a. Process f (f a) -> Process f a
flatten Halt = Halt
flatten (Emit h t) = await h (\a -> Emit a (flatten t))
flatten (Await f) = Await (runExists (\(AwaitF req recv fb) -> mkExists (AwaitF req (flatten <<< recv) (flatten fb))) f)

repeatedly :: forall f a. Process f a -> Process f a
repeatedly p = go p
  where go Halt = go p
        go (Emit h t) = Emit h (go t)
        go (Await f) = Await (runExists (\(AwaitF req recv fb) -> mkExists (AwaitF req (go <<< recv) fb)) f)

evalMap :: forall f a b. (a -> f b) -> Process f a -> Process f b
evalMap f p = flatten (f <$> p)

runFoldMap :: forall f a b. (Monad f, Monoid b) => (a -> b) -> Process f a -> f b
runFoldMap f p = go p mempty
  where go Halt acc = pure acc
        go (Emit h t) acc = go t (acc <> f h)
        go (Await f) acc = runExists (\(AwaitF req recv fb) -> req >>= \s -> go (recv s) acc) f

runLog :: forall f a. (Monad f) => Process f a -> f (L.List a)
runLog = runFoldMap L.singleton

run :: forall f a. (Monad f) => Process f a -> f Unit
run p = const unit <$> runFoldMap (const unit) p
