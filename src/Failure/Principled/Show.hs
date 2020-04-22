-- | Helpers for writing `showsPrec` implementations that respect precedence
--   properly. You should, of course, still prefer @deriving `Show`@ (or, better
--   yet, eschewing `Show` all together) whenever possible.
--
-- > data MyType a
-- >  = MyType a :+: MyType a
-- >  | MyType a :*: MyType a
-- >  | Const a
-- >  | Convert (a -> a) a -- ^ this prevents @deriving `Show`@
-- >
-- > infixl 6 :+:
-- > infixl 7 :*:
-- >
-- > instance Show a => Show (MyType a) where
-- >   showsPrec p = \case
-- >     x :+: y = infixlPrec p 6 x ":+:" y
-- >     x :*: y = infixlPrec p 7 x ":*:" y
-- >     Const x = appPrec p "Const" [arg x]
-- >     Convert _f x = appPrec p "Convert" [showString "_", arg x]
--
-- The pattern of the @infix*@ functions is that the first argument is the
-- current precedence from `showsPrec`, the second is the precedence from the
-- fixity delaration, then argument-operator-argument.
module Failure.Principled.Show where

-- | For `show`ing operators with an @infix@ fixity declaration.
infixPrec
  :: (Show a, Show b)
  => Int -- ^ The precedence passed in from `showsPrec`
  -> Int -- ^ The precedence from the fixity declaration
  -> a -- ^ The left argument to the operator
  -> String -- ^ The display of the operator
  -> b -- ^ The right argument to the operator
  -> ShowS
infixPrec p n x op y =
  showParen (p > n) $
    showsPrec (n + 1) x . showString (" " <> op <> " ") . showsPrec (n + 1) y

-- | For `show`ing operators with an @infixl@ fixity declaration.
infixlPrec
  :: (Show a, Show b)
  => Int -- ^ The precedence passed in from `showsPrec`
  -> Int -- ^ The precedence from the fixity declaration
  -> a -- ^ The left argument to the operator
  -> String -- ^ The display of the operator
  -> b -- ^ The right argument to the operator
  -> ShowS
infixlPrec p n x op y =
  showParen (p > n) $
    showsPrec n x . showString (" " <> op <> " ") . showsPrec (n + 1) y

-- | For `show`ing operators with an @infixr@ fixity declaration.
infixrPrec
  :: (Show a, Show b)
  => Int -- ^ The precedence passed in from `showsPrec`
  -> Int -- ^ The precedence from the fixity declaration
  -> a -- ^ The left argument to the operator
  -> String -- ^ The display of the operator
  -> b -- ^ The right argument to the operator
  -> ShowS
infixrPrec p n x op y =
  showParen (p > n) $
    showsPrec (n + 1) x . showString (" " <> op <> " ") . showsPrec n y

-- | For `show`ing non-operator data constructors.
appPrec :: Int -> String -> [ShowS] -> ShowS
appPrec p app args =
  showParen (p > 10) $ showString app . foldMap (showString " " .) args

-- | For `show`ing arguments to a non-operator data constructor.
arg :: Show a => a -> ShowS
arg = showsPrec 11
