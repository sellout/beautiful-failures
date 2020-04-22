{-# language DataKinds
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}

-- | Various tools for improving the quality of the failures in your programs.
--   This is meant to encourage you to "do the right thing" more often.
module Failure.Principled
  ( -- * A better `Except.throw`
    throw,
    -- * attach a `CallStack` to exceptions
    CallStacked (..),
    catchIgnoringStack,
    handleIgnoringStack,
    addCallStack,
    throwWithCallStack,
    throwIOWithCallStack,
    -- * treat arbitrary types as exceptions
    ExceptFailure (..),
    throwFailure,
    throwIOFailure,
    -- * convert from disjunctions to exceptions
    throwLeft,
    throwIOLeft,
    -- * to make exceptions useful as an exit mechanism
    Display (..),
    displayMain,
    catchIgnoringDisplay,
    handleIgnoringDisplay,
    -- * throw checking
    NotInIO,
  )
where

import           Control.Exception (Exception (..), SomeException, catch, handle, throwIO)
import qualified Control.Exception as Except
import           Control.Monad.Trans.Accum
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.CPS as CPS
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Select
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Kind (Constraint)
import Data.Typeable (Typeable)
import Failure.Principled.Show as Show
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import System.Exit (exitFailure)
import System.IO (IO, hPutStr, stderr)

-- | A replacement for `Except.throw` that complains if you should be calling
--  `throwIO` instead.
throw :: (Exception e, NotInIO a) => e -> a
throw = Except.throw

-- | An Exception that simply wraps another exception to attach a call stack
data CallStacked e = CallStacked { exception :: e, calls :: CallStack }
  deriving Show

-- | See `handleIgnoringStack`.
catchIgnoringStack :: Exception e => IO a -> (e -> IO a) -> IO a
catchIgnoringStack = flip handleIgnoringStack

-- | Like `handle`, but checks for both the "bare" and `CallStacked` versions of
--   an exception.
--
--  __NB__: This does mean that if you re-`throwIO` you'll discard the original
--         `CallStack`.
handleIgnoringStack :: Exception e => (e -> IO a) -> IO a -> IO a
handleIgnoringStack f = handle f . handle (\(CallStacked e _) -> f e)

instance Exception e => Exception (CallStacked e) where
  displayException (CallStacked e calls') = displayException e <> "\n" <> prettyCallStack calls'

-- | Adds the current call stack to the exception.
addCallStack :: HasCallStack => e -> CallStacked e
addCallStack e = CallStacked e callStack

-- | A way to lift reified errors into the exception system. This means we get
--   the best of both worlds when we're in IO - we can fail with a
--   locally-defined message and still leave it up to someone to catch it if
--   necessary.
--
--   Since this is likely to be used far from where an error occurs, it doesn't
--   include a `CallStack`. But `CallStacked . Lifted` is available if you want
--   to add a stack to it.
data ExceptFailure e = ExceptFailure { display :: e -> String, failure :: e }

-- | __NB__: This instance is /not/ `read`able. `Exception` extends `Show`, so
--           to avoid having an extraneous @`Show` e@ constraint on @`Exception`
--          (`ExceptFailure` e)@, we have to skip it here.
instance Show (ExceptFailure e) where
  showsPrec p (ExceptFailure display failure) =
    Show.appPrec
      p
      "ExceptFailure"
      [showString "_", Show.arg $ display failure]

instance Typeable e => Exception (ExceptFailure e) where
  displayException (ExceptFailure display failure) = display failure

-- | Tries to prevent using non-`IO`-safe `throw` operations in `IO`. It covers
--   the standard transformers, but still can't /guarantee/ that there's no `IO`
--   in the stack, in which case the constraint name at least gives an extra
--   hint to callers. The constraint may need to be propagated and it also tries
--   to give a helpful error message when it fails.
type family NotInIO a :: Constraint where
  -- The bulk of the cases are the long way of saying
  -- @`MonadIO` m => m a = `TypeError` ...@ but without being able to catch any
  -- instances we don't know about a priori.
  NotInIO (IO a) =
    TypeError
    ('Text "Can't use a `throw` function in `" ':<>:
     ShowType (IO a) ':<>:
     'Text "`." ':$$:
     'Text "Try using the `throwIO` variant instead.")
  NotInIO (AccumT w m a) = NotInIO (m (a, w))
  NotInIO (ContT r m _) = NotInIO (m r)
  NotInIO (ExceptT e m a) = NotInIO (m (Either e a))
  NotInIO (IdentityT m a) = NotInIO (m a)
  NotInIO (MaybeT m a) = NotInIO (m (Maybe a))
  NotInIO (ReaderT _ m a) = NotInIO (m a)
  NotInIO (SelectT _ m a) = NotInIO (m a)
  NotInIO (CPS.RWST _ w s m a) = NotInIO (m (a, s, w))
  NotInIO (CPS.WriterT w m a) = NotInIO (m (a, w))
  NotInIO (Lazy.RWST _ w s m a) = NotInIO (m (a, s, w))
  NotInIO (Lazy.StateT s m a) = NotInIO (m (a, s))
  NotInIO (Lazy.WriterT w m a) = NotInIO (m (a, w))
  NotInIO (Strict.RWST _ w s m a) = NotInIO (m (a, s, w))
  NotInIO (Strict.StateT s m a) = NotInIO (m (a, s))
  NotInIO (Strict.WriterT w m a) = NotInIO (m (a, w))
  -- and otherwise we're safe ... relatively
  NotInIO _ = ()

-- | Throws any type. The provided function is what's used for serialization if
--   the exception isn't caught.
throwFailure :: (Typeable e, NotInIO a) => (e -> String) -> e -> a
throwFailure f = throw . ExceptFailure f

-- | See `throwFailure`.
throwIOFailure :: Typeable e => (e -> String) -> e -> IO a
throwIOFailure f = throwIO . ExceptFailure f

-- | Throws an exception that's expecting to carry a `CallStack`.
--
--   I /think/ these two should be equivalent
--
-- > throwWithCallStack . CallStacked
-- > throw . addCallStack
throwWithCallStack :: (HasCallStack, Exception e, NotInIO a) => (CallStack -> e) -> a
throwWithCallStack = throw . ($ callStack)

-- | See `throwWithCallStack`.
throwIOWithCallStack :: (HasCallStack, Exception e) => (CallStack -> e) -> IO a
throwIOWithCallStack = throwIO . ($ callStack)

-- | Eliminate an `Either` by throwing the left.
throwLeft :: (Exception e, NotInIO a) => Either e a -> a
throwLeft = either throw id

-- | See `throwLeft`.
throwIOLeft :: Exception e => Either e a -> IO a
throwIOLeft = either throwIO pure

-- | A wrapper to convince GHC to use `displayException` when printing failures.
newtype Display a = Display a

-- | __NB__: This instance is /not/ `read`able. It simply calls
--          `displayException` on the wrapped `Exception`, allowing programmers
--           to not abuse their own `Show` instances while still getting better
--           output from GHC.
instance Exception e => Show (Display e) where
  show (Display e) = displayException e

instance Exception e => Exception (Display e)

-- | This "fixes" @main@ to use `displayException` instead of `show` when
--   failing with an exception.
--
--   Replace @main = x@ with @main = displayMain $ x@
--
--   This works by wrapping the `Exception` in a newtype that has the correct
--   behavior, which means that if you want to try handling these exceptions
--   outside a call to `displayMain`, you should be using
--  `handleIgnoringDisplay` (or `catchIgnoringDisplay`).
--
--   See [Why doesn’t GHC use my `displayException`
--   method?](https://stackoverflow.com/questions/55490766/why-doesn-t-ghc-use-my-displayexception-method)
--   for some explanation. I find it totally unconvincing, and I think Kmett's
--   comment about "\'helpful\' `Show` instances" makes the argument /for/ using
--  `displayException` -- with the current behavior, users are encouraged to
--   define a custom `show` to get GHC to output a useful failure message, which
--   then breaks the /intended/ use of `show` as an syntax printer.
displayMain :: IO () -> IO ()
displayMain =
  -- Using `handleIgnoringDisplay` in order to avoid re-wrapping already-wrapped
  -- exceptions.
  handleIgnoringDisplay (\e -> throwIO (Display (e :: SomeException)))

-- | Handles an exception that /may/ be wrapped in the `Display` @newytpe@,
--   unwrapping it if necessary.
handleIgnoringDisplay :: Exception e => (e -> IO a) -> IO a -> IO a
handleIgnoringDisplay f = handle f . handle (\(Display e) -> f e)

-- | See `handleIgnoringDisplay`.
catchIgnoringDisplay :: Exception e => IO a -> (e -> IO a) -> IO a
catchIgnoringDisplay = flip handleIgnoringDisplay
