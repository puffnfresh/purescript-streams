module Control.Monad.Task where

  import Control.Monad.Eff
  import Control.Monad.Free
  import Control.Monad.Trampoline
  import Data.Tuple
  import Debug.Trace

  foreign import data Task :: # ! -> * -> *

  type PureTask a = forall e. Task e a

  data RealWorld = RealWorld

  foreign import bindT "function bindT(t) {\
                       \  return function(f) {\
                       \    return task(function(rw) {\
                       \      return Prelude['>>='](Control_Monad_Free.freeBind({}))(t(rw))(function(u) {\
                       \        var nw = u.values[0];\
                       \        var a = u.values[1];\
                       \        return f(a)(nw);\
                       \      });\
                       \    });\
                       \  };\
                       \}" :: forall e a b. Task e a -> (a -> Task e b) -> Task e b

  instance taskFunctor :: Functor (Task e) where
    (<$>) = liftA1

  instance taskApply :: Apply (Task e) where
    (<*>) = ap

  instance taskApplicative :: Applicative (Task e) where
    pure a = task (\RealWorld -> done (Tuple RealWorld a))

  instance taskBind :: Bind (Task e) where
    (>>=) = bindT

  instance taskMonad :: Monad (Task e)

  foreign import task "function task(f) {\
                      \  return function(rw) {\
                      \    return Control_Monad_Free.Suspend(Control_Monad_Trampoline.Delay(f));\
                      \  };\
                      \}" :: forall e a. (RealWorld -> Trampoline (Tuple RealWorld a)) -> Task e a

  foreign import traceTask "function traceTask(s) {\
                           \  return task(function(rw) {\
                           \    console.log(s);\
                           \    return Control_Monad_Free.pureF(Control_Monad_Trampoline.delayApplicative({}))(Data_Tuple.Tuple(rw)({}));\
                           \  });\
                           \}" :: forall r. String -> Task (trace :: Trace | r) {}

  foreign import unsafePerformTask "function unsafePerformTask(t) {\
                                   \  return Control_Monad_Trampoline.runTrampoline(t(RealWorld)).values[1];\
                                   \}" :: forall e a. Task e a -> a

  foreign import runTask "function runTask(t) {\
                         \  return function() {\
                         \    return unsafePerformTask(t);\
                         \  };\
                         \}" :: forall e a. Task e a -> Eff e {}
