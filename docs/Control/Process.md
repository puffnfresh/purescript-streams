## Module Control.Process

#### `AwaitF`

``` purescript
data AwaitF f a r
  = AwaitF (f r) (r -> Process f a) (Process f a)
```

#### `Process`

``` purescript
data Process f a
  = Halt
  | Emit a (Process f a)
  | Await (Exists (AwaitF f a))
```

##### Instances
``` purescript
instance processSemigroup :: Semigroup (Process k a)
instance processMonoid :: Monoid (Process k a)
instance processFunctor :: Functor (Process f)
instance processApply :: Apply (Process f)
instance processApplicative :: Applicative (Process f)
instance processBind :: Bind (Process f)
instance processMonad :: Monad (Process f)
```

#### `Source`

``` purescript
type Source e a = Process (Eff e) a
```

#### `Sink`

``` purescript
type Sink e a = Source e (a -> Eff e Unit)
```

#### `Channel`

``` purescript
type Channel e a b = Source e (a -> Eff e b)
```

#### `await`

``` purescript
await :: forall f r a. f r -> (r -> Process f a) -> Process f a
```

#### `emit`

``` purescript
emit :: forall f a. a -> Process f a
```

#### `eval`

``` purescript
eval :: forall f a. f a -> Process f a
```

#### `translate`

``` purescript
translate :: forall f g a. (forall b. f b -> g b) -> Process f a -> Process g a
```

#### `flatten`

``` purescript
flatten :: forall f a. Process f (f a) -> Process f a
```

#### `repeatedly`

``` purescript
repeatedly :: forall f a. Process f a -> Process f a
```

#### `evalMap`

``` purescript
evalMap :: forall f a b. (a -> f b) -> Process f a -> Process f b
```

#### `runFoldMap`

``` purescript
runFoldMap :: forall f a b. (Monad f, Monoid b) => (a -> b) -> Process f a -> f b
```

#### `runLog`

``` purescript
runLog :: forall f a. (Monad f) => Process f a -> f (List a)
```

#### `run`

``` purescript
run :: forall f a. (Monad f) => Process f a -> f Unit
```


