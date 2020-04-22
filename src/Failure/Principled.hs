{-# language DataKinds
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}

module Failure.Principled
  ( -- * attach a `CallStack` to exceptions
    CallStacked (..),
    catchIgnoringStack,
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
    displayMain,
  )
where

import Control.Exception (Exception (..), SomeException, catch, handle, throw, throwIO)
import Data.Kind (Constraint)
import Data.Typeable (Typeable)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import System.Exit (exitFailure)
import System.IO (IO, hPutStr, stderr)

-- | An Exception that simply wraps another exception to attach a call stack
data CallStacked e = CallStacked { exception :: e, calls :: CallStack }
  deriving Show

-- | Like `catch`, but checks for both the "bare" and `CallStacked` versions of
--   an exception. This does mean that if you re-throw you'll discard the
--   original `CallStack`.
catchIgnoringStack :: Exception e => IO a -> (e -> IO a) -> IO a
catchIgnoringStack input f =
  catch (catch input f) (\(CallStacked e _) -> f e)

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

showInfix :: (Show a, Show b) => Int -> String -> a -> b -> Int -> ShowS
showInfix n op x y p =
  showParen (p > n) $ showsPrec (n + 1) x . showString op . showsPrec (n + 1) y

showInfixl :: (Show a, Show b) => Int -> String -> a -> b -> Int -> ShowS
showInfixl n op x y p =
  showParen (p > n) $ showsPrec n x . showString op . showsPrec (n + 1) y

showInfixr :: (Show a, Show b) => Int -> String -> a -> b -> Int -> ShowS
showInfixr n op x y p =
  showParen (p > n) $ showsPrec (n + 1) x . showString op . showsPrec n y

showApp :: String -> [ShowS] -> Int -> ShowS
showApp app args p =
  showParen (p > 10) $ showString app . foldMap (showString " " .) args

showArg :: Show a => a -> ShowS
showArg = showsPrec 11

-- | This instance is _not_ `read`able. Unfortunately, GHC doesn't use
--  `displayException` to print exceptions to `stderr`, so we're forced to abuse
--  `show` instead.
instance Show e => Show (ExceptFailure e) where
  showsPrec p (ExceptFailure _ failure) =
    showApp "ExceptFailure" [showString "_", showArg failure] p

-- | This instance (and thus, `throw` functions built on it) require a @`Show`
--   e@ only incidentally, as `Exception` requires `Show` (even though this
--   instance doesn't use the `Show` constraint).
instance (Show e, Typeable e) => Exception (ExceptFailure e) where
  displayException (ExceptFailure display failure) = display failure

-- | Prevent using non-`IO`-safe `throw` operations in `IO`. This also tries to
--   give a helpful error message when someone tries.
type family NotInIO a :: Constraint where
  NotInIO (IO a) =
    TypeError
    ('Text "Can't use a `throw` function in `" ':<>:
     ShowType (IO a) ':<>:
     'Text "`." ':$$:
     'Text "Try using the `throwIO` variant instead.")
  NotInIO _ = ()

-- | Throws any type. The provided function is what's used for serialization if
--   the exception isn't caught.
throwFailure :: (Show e, Typeable e, NotInIO a) => (e -> String) -> e -> a
throwFailure f = throw . ExceptFailure f

-- | See `throwFailure`.
throwIOFailure :: (Show e, Typeable e) => (e -> String) -> e -> IO a
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

-- | This "fixes" @main@ to use `displayException` instead of `show` when failing with an exception.
--   Replace @main = x@ with @main = displayMain $ x@
--
--   See [Why doesn’t GHC use my `displayException`
--   method?](https://stackoverflow.com/questions/55490766/why-doesn-t-ghc-use-my-displayexception-method)
--   for some explanation. I find it totally unconvincing, and I think Kmett's comment about
--  "\'helpful\' `Show` instances" makes the argument /for/ using `displayException` -- with the
--   current behavior, users are encouraged to define a custom `show` to get GHC to output a useful
--   failure message, which then breaks the _intended_ use of `show` as an syntax printer.
displayMain :: IO () -> IO ()
displayMain =
  handle
    (\e ->
      hPutStr stderr (displayException (e :: SomeException)) *> exitFailure)
