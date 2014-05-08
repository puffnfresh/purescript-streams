# Module Documentation

## Module Control.Process

### Types

    type Channel e a b = Source e (a -> Eff e b)

    data MUnit  where
      MUnit :: {  } -> MUnit 

    data Process f a where
      Halt :: Process f a
      Emit :: a -> Process f a -> Process f a
      Await :: forall s. (forall r. f r -> (r -> Process f a) -> Process f a -> s) -> s -> Process f a

    type Sink e a = Source e (a -> Eff e {  })

    type Source e a = Process (Eff e) a


### Type Class Instances

    instance munitMonoid :: Monoid MUnit

    instance munitSemigroup :: Semigroup MUnit

    instance processApplicative :: Applicative (Process f)

    instance processApply :: Apply (Process f)

    instance processBind :: Bind (Process f)

    instance processFunctor :: Functor (Process f)

    instance processMonad :: Monad (Process f)

    instance processMonoid :: Monoid (Process k a)

    instance processSemigroup :: Semigroup (Process k a)


### Values

    await :: forall f r a. f r -> (r -> Process f a) -> Process f a

    emit :: forall f a. a -> Process f a

    eval :: forall f a. f a -> Process f a

    evalMap :: forall f a b. (a -> f b) -> Process f a -> Process f b

    flatten :: forall f a. Process f (f a) -> Process f a

    repeatedly :: forall f a. Process f a -> Process f a

    run :: forall f a. (Monad f) => Process f a -> f {  }

    runFoldMap :: forall f a b. (Monad f, Monoid b) => (a -> b) -> Process f a -> f b

    runLog :: forall f a. (Monad f) => Process f a -> f [a]

    translate :: forall f g a. (forall b. f b -> g b) -> Process f a -> Process g a