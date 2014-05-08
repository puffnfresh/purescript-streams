module Control.Process where

  import Control.Monad.Eff
  import Data.Monoid

  data Process f a = Halt
                   | Emit a (Process f a)
                   | Await (forall s. (forall r. f r -> (r -> Process f a) -> Process f a -> s) -> s)

  instance processSemigroup :: Semigroup (Process k a) where
    (<>) Halt p = p
    (<>) (Emit h t) p2 = Emit h (t <> p2)
    (<>) (Await f) p2 = Await (\g -> f (\req recv fb -> g req (\res -> recv res <> p2) (fb <> p2)))

  instance processMonoid :: Monoid (Process k a) where
    mempty = Halt

  instance processFunctor :: Functor (Process f) where
    (<$>) f Halt = Halt
    (<$>) f (Emit h t) = Emit (f h) (f <$> t)
    (<$>) f (Await g) = Await (\h -> g (\req recv fb -> h req (\res -> f <$> recv res) (f <$> fb)))

  instance processApply :: Apply (Process f) where
    (<*>) Halt b = Halt
    (<*>) (Emit h t) b = (h <$> b) <> (t <*> b)
    (<*>) (Await g) b = Await (\h -> g (\req recv fb -> h req (\res -> recv res <*> b) (fb <*> b)))

  instance processApplicative :: Applicative (Process f) where
    pure a = Emit a Halt

  instance processBind :: Bind (Process f) where
    (>>=) Halt f = Halt
    (>>=) (Emit h t) f = f h <> (t >>= f)
    (>>=) (Await g) f = Await (\h -> g (\req recv fb -> h req (\res -> recv res >>= f) (fb >>= f)))

  instance processMonad :: Monad (Process f) where

  type Source e a = Process (Eff e) a
  type Sink e a = Source e (a -> Eff e {})
  type Channel e a b = Source e (a -> Eff e b)

  await :: forall f r a. f r -> (r -> Process f a) -> Process f a
  await req recv = Await (\f -> f req recv Halt)

  emit :: forall f a. a -> Process f a
  emit h = Emit h Halt

  eval :: forall f a. f a -> Process f a
  eval h = await h emit

  translate :: forall f g a. (forall b. f b -> g b) -> Process f a -> Process g a
  translate _ Halt = Halt
  translate f (Emit h t) = Emit h (translate f t)
  translate f (Await g) = Await (\h -> g (\req recv fb -> h (f req) (\b -> translate f (recv b)) (translate f fb)))

  flatten :: forall f a. Process f (f a) -> Process f a
  flatten Halt = Halt
  flatten (Emit h t) = Await (\f -> f h (\a -> Emit a (flatten t)) Halt)
  flatten (Await f) = Await (\g -> f (\req recv fb -> g req (\a -> flatten (recv a)) (flatten fb)))

  repeatedly :: forall f a. Process f a -> Process f a
  repeatedly p = go p
    where go Halt = go p
          go (Emit h t) = Emit h (go t)
          go (Await f) = Await (\g -> f (\req recv fb -> g req (\r -> go (recv r)) fb))

  evalMap :: forall f a b. (a -> f b) -> Process f a -> Process f b
  evalMap f p = flatten (f <$> p)

  runFoldMap :: forall f a b. (Monad f, Monoid b) => (a -> b) -> Process f a -> f b
  runFoldMap f p = go p mempty
    where go Halt acc = pure acc
          go (Emit h t) acc = go t (acc <> f h)
          go (Await f) acc = f (\req recv fb -> req >>= \s -> go (recv s) acc)

  runLog :: forall f a. (Monad f) => Process f a -> f [a]
  runLog = runFoldMap (\x -> [x])

  -- Isomorphic to {}
  data MUnit = MUnit {}

  instance munitSemigroup :: Semigroup MUnit where
    (<>) (MUnit {}) (MUnit {}) = MUnit {}

  instance munitMonoid :: Monoid MUnit where
    mempty = MUnit {}

  run :: forall f a. (Monad f) => Process f a -> f {}
  run p = const {} <$> runFoldMap (const (MUnit {})) p
