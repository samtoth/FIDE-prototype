{-| This @lens@ tutorial targets Haskell beginners and assumes only basic
    familiarity with Haskell.  By the end of this tutorial you should:

    * understand what problems the @lens@ library solves,

    * know when it is appropriate to use the @lens@ library,

    * be proficient in the most common @lens@ idioms,

    * understand the drawbacks of using lenses, and:

    * know where to look if you wish to learn more advanced tricks.

    If you would like to follow along with these examples, just import this
    module:

> $ ghci
> >>> import Control.Lens.Tutorial

-}

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}

module Control.Optics.Tutorial (
    -- * Motivation
    -- $motivation

    -- * Lenses
    -- $lenses

    -- * Accessor notation
    -- $accessors

    -- * First-class
    -- $firstclass

    -- * Traversals
    -- $traversals

    -- * Types
    -- $types

    -- * Drawbacks
    -- $drawbacks

    -- * Conclusion
    -- $conclusion

    -- * Exports
    -- $exports
      Atom(..)
    , element
    , point
    , Point(..)
    , x
    , y
    , Molecule(..)
    , atoms
    , Pair(..)
    , traverse
    ) where

import Control.Applicative (Applicative)
import Optics hiding (element)
import Optics.TH
import Data.Foldable (Foldable)
import Data.Monoid (Monoid)

-- $motivation
--
--     The simplest problem that the @lens@ library solves is updating deeply
--     nested records.  Suppose you had the following nested Haskell data types:
-- 
-- > data Atom = Atom { _element :: String, _point :: Point }
-- >
-- > data Point = Point { _x :: Double, _y :: Double }
-- 
--     If you wanted to increase the @x@ coordinate of an `Atom` by one unit, you
--     would have to write something like this in Haskell:
-- 
-- > shiftAtomX :: Atom -> Atom
-- > shiftAtomX (Atom e (Point x y)) = Atom e (Point (x + 1) y)
-- 
--     This unpacking and repacking of data types grows increasingly difficult the
--     more fields you add to each data type or the more deeply nested your data
--     structures become.
-- 
--     The @lens@ library solves this problem by letting you instead write:
-- 
-- > -- atom.hs
-- >
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Optics hiding (element)
-- > import Optics.TH
-- >
-- > data Atom = Atom { _element :: String, _point :: Point } deriving (Show)
-- >
-- > data Point = Point { _x :: Double, _y :: Double } deriving (Show)
-- >
-- > $(makeLenses ''Atom)
-- > $(makeLenses ''Point)
-- >
-- > shiftAtomX :: Atom -> Atom
-- > shiftAtomX = over (point % x) (+ 1)
-- 
--     Let's convince ourselves that this works:
-- 
-- >>> let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
-- >>> shiftAtomX atom
-- Atom {_element = "C", _point = Point {_x = 2.0, _y = 2.0}}
-- 
--     The above solution does not change no matter how many fields we add to
--     @Atom@ or @Point@.
-- 
--     Now suppose that we added yet another data structure:
-- 
-- > data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)
-- 
--     We could shift an entire @Molecule@ by writing:
-- 
-- > $(makeLenses ''Molecule)
-- >
-- > shiftMoleculeX :: Molecule -> Molecule
-- > shiftMoleculeX = over (atoms % traversed % point % x) (+ 1)
-- 
--     Again, this works the way we expect:
-- 
-- >>> let atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
-- >>> let atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
-- >>> let molecule = Molecule { _atoms = [atom1, atom2] }
-- >>> shiftMoleculeX molecule  -- Output formatted for clarity
-- Molecule {_atoms = [Atom {_element = "C", _point = Point {_x = 2.0, _y = 2.0}},Atom {_element = "O", _point = Point {_x = 4.0, _y = 4.0}}]}
--
-- ... or formatted for clarity:
--
-- > Molecule
-- >     { _atoms =
-- >         [ Atom { _element = "C", _point = Point { _x = 2.0, _y = 2.0 } }
-- >         , Atom { _element = "O", _point = Point { _x = 4.0, _y = 4.0 } }
-- >         ]
-- >     }
-- 
--     Many people stumble across lenses while trying to solve this common problem
--     of working with data structures with a large number of fields or deeply
--     nested values.  These sorts of situations arise commonly in:
-- 
--     * games with complex and deeply nested state
-- 
--     * scientific data formats
-- 
--     * sensor or instrument output
-- 
--     * web APIs
-- 
--     * XML and JSON
-- 
--     * enterprise code where data structures can have tens, hundreds, or even
--       thousands of fields (true story!)

{- $lenses
    You might have some basic questions like:

    /Question:/ What is a lens?

    /Answer:/ A lens is a first class getter and setter

    We already saw how to use lenses to update values using `over`, but we can
    also use lenses to retrieve values using `view`:

>>> let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
>>> view (point % x) atom
1.0

    In other words, lenses package both \"get\" and \"set\" functionality into
    a single value (the lens).  You could pretend that a lens is a record
    with two fields:

> data Lens a b = Lens
>     { view :: a -> b
>     , over :: (b -> b) -> (a -> a)
>     }

    That's not how lenses are actually implemented, but it's a useful
    starting intuition.

    /Question:/ What is the type of a lens?

    /Answer:/ We used two lenses in the above @Atom@ example, with these types:

> point :: Lens' Atom  Point
> x     :: Lens' Point Double

    The @point@ lens contains all the information we need to get or set the
    @_point@ field of the @Atom@ type (which is a `Point`).  Similarly, the @x@
    lens contains all the information we need to get or set the @_x@ field of
    the @Point@ data type (which is a `Double`).

    The convention for the `Lens'` type parameters is:

> --    +-- Bigger type
> --    |
> --    v
> Lens' bigger smaller
> --           ^
> --           |
> --           +--  Smaller type within the bigger type


    /Question:/ How do I create lenses?

    /Answer:/ You can either auto-generate them using Template Haskell or
    create them by hand

    In our @Atom@ example, we auto-generated the lenses using Template Haskell,
    like this:

> makeLenses ''Atom
> makeLenses ''Point

    This created four lenses of the following types:

> element :: Lens' Atom String
> point   :: Lens' Atom Point
> x       :: Lens' Point Double
> y       :: Lens' Point Double

    `makeLenses` creates one lens per field prefixed with an underscore.  The
    lens has the same name as the field without the underscore.

    However, sometimes Template Haskell is not an option, so we can also use
    the `lens` utility function to build lenses.  This utility has type:

> lens :: (a -> b) -> (a -> b -> a) -> Lens' a b

    The first argument is a \"getter\" (a way to extract a @\'b\'@ from an
    @\'a\'@).  The second argument is a \"setter\" (given a @b@, update an
    @a@).  The result is a `Lens'` built from the getter and setter.  You would
    use `lens` like this:

> point :: Lens' Atom Point
> point = lens _point (\atom newPoint -> atom { _point = newPoint })

    You can even define lenses without incurring a dependency on the full
    @optics@ library. You only need the @optics-core@ library which has
    minimal dependencies itself.


    /Question:/ How do I combine lenses?

    /Answer:/ You compose them using the composition operator `%`.

    You can think of the lens composition operator as having this type:

> (%) :: Lens' a b -> Lens' b c -> Lens' a c

    In our original @Atom@ example, we composed the @point@ and @x@ lenses to
    create a new composite lens:

> point     :: Lens' Atom Point
> x         :: Lens' Point Double
>
> point % x :: Lens' Atom Double

    This composite lens lets us get or set the @x@ coordinate of an @Atom@.
    We can use `over` and `view` on the composite `Lens'` and they will behave
    exactly the way we expect:

> view (point % x) :: Atom -> Double
>
> over (point % x) :: (Double -> Double) -> (Atom -> Atom)

    /Question:/ How do I consume lenses?

    /Answer:/ Using `view`, `set` or `over`

    Here are their types:

> view :: Lens' a b -> a -> b
>
> over :: Lens' a b -> (b -> b) -> a -> a
>
> set  :: Lens' a b ->       b  -> a -> a
> set lens b = over lens (\_ -> b)

    `view` and `over` are the two fundamental functions on lenses.  `set` is
    just a special case of `over`.

    `view` and `over` are fundamental because they distribute over lens
    composition:

> view (lens1 % lens2) = (view lens2) % (view lens1)
>
> view id = id

> over (lens1 % lens2) = (over lens1) % (over lens2)
>
> over id = id

    /Question:/ What else do I need to know?

    /Answer:/ That's pretty much it!

    For 90% of use cases, you just:

    * Create lenses (using `makeLens`, `lens` or plain-old `fmap`)

    * Compose them (using (`%`))

    * Consume them (using `view`, `set`, and `over`)

    You could actually stop reading here if you are in a hurry since this
    covers the overwhelmingly common use case for the library.  On the other
    hand, keep reading if you would like to learn additional tricks and
    features.
-}

{- $accessors
    You might be used to object-oriented languages where you could retrieve a
    nested field using:

> atom.point.x

    You can do almost the exact same thing using the @lens@ library, except
    that the first dot will have a @^@ right before the dot:

>>> let atom = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
>>> atom^.point%x
1.0

    You can better understand why this works, by adding whitespace and
    explicit parentheses:

> atom ^. (point % x)

    This trick uses (`^.`), which is an infix operator equivalent to `view`:

> (^.) :: a -> Lens' a b -> b
> x ^. l = view l x

    ... and you just keep adding dots after that for each lens you compose.
    This gives the appearance of object-oriented accessors if you omit the
    whitespace around the operators.
-}

{- $firstclass
    Lenses are \"first class\" values, meaning that you can manipulate them
    using ordinary functional programming techniques.  You can take them as
    inputs, return them as outputs, or stick them in data structures.  Anything
    goes!

    For example, suppose we don't want to define separate shift functions for
    @Atom@s and @Molecule@s:

> shiftAtomX :: Atom -> Atom
> shiftAtomX = over (point % x) (+ 1)

> shiftMoleculeX :: Molecule -> Molecule
> shiftMoleculeX = over (atoms % traversed % point % x) (+ 1)

    We can instead unify them into a single function by parametrizing the
    shift function on the lens:

> shift lens = over lens (+ 1)

    This lets us write:

> shift (point % x) :: Atom -> Atom
>
> shift (atoms % traversed % point % x) :: Molecule -> Molecule

    Even better, we can define synonyms for our composite lenses:

> atomX :: Lens' Atom Double
> atomX = point % x
>
> -- We'll learn what `Traversal` means shortly
> moleculeX :: Traversal' Molecule Double
> moleculeX = atoms % traversed % point % x

    Now we can write code almost identical to the original code:

> shift atomX :: Atom -> Atom
>
> shift moleculeX :: Molecule -> Molecule

    ... but we also get several other utilities for free:

> set atomX :: Double -> Atom -> Atom
>
> set moleculeX :: Double -> Molecule -> Molecule
>
> view atomX :: Atom -> Double
>
> -- We can't use `view` for `Traversal'`s.  Read on to find out why
> toListOf moleculeX :: Molecule -> [Double]

    That's much more reusable, but you might wonder what this `Traversal'` and
    `toListOf` business is all about.
-}

-- $traversals
--     /Question:/ What is a traversal?
-- 
--     /Answer:/ A first class getter and setter for an arbitrary number of values
-- 
--     A traversal lets you get all the values it points to as a list and it also
--     lets you update or set all the values it points to.  Think of a traversal
--     as a record with two fields:
-- 
-- > data Traversal' a b = Traversal'
-- >     { toListOf :: a -> [b]
-- >     , over     :: (b -> b) -> (a -> a)
-- >     }
-- 
--     That's not how traversals are actually implemented, but it's a useful
--     starting intuition.
-- 
--     We can still use `over` and `set` (a special case of `over`) with a
--     traversal, but we use `toListOf` instead of `view`.
-- 
--     /Question:/ What is the type of a traversal?
-- 
--     /Answer:/ We used one traversal in the above @Molecule@ example:
-- 
-- > moleculeX :: Traversal' Molecule Double
-- 
--     This `Traversal'` lets us get or set an arbitrary number of x coordinates,
--     each of which is a `Double`.  There could be less than one x coordinate
--     (i.e. 0 coordinates) or more than one x coordinate.  Contrast this with a
--     `Lens'` which can only get or set exactly one value.
-- 
--     Every `Lens'` can be turned into a `Traversal'` using `castOptic`:
-- 
-- > castOptic atoms   :: Traversal' Molecule [Atom]
-- > castOptic element :: Traversal' Atom     String
-- > castOptic point   :: Traversal' Atom     Point
-- > castOptic x       :: Traversal' Point    Double
-- > castOptic y       :: Traversal' Point    Double
--
--     In practice, you rarely need to use `castOptic`, because composition
--     does this for you automatically. So composing a lens with a traversal
--     just works and yields a traversal (see below).
-- 
--     We actually used yet another `Traversal'`, which was `traversed`:
-- 
-- > traversed :: Traversable t => Traversal' (t a) a
--
--     which is the traversal corresponding to the Haskell function `traverse`,
--     that operates on all elements of a traversable functor.
-- 
--     In our @Molecule@ example, we were using the special case where @t = []@:
-- 
-- > traversed :: Traversal' [a] a
-- 
--     In Haskell, you can derive `Functor`, `Data.Foldable.Foldable` and
--     `Traversable` for many data types using the @DeriveFoldable@ and
--     @DeriveTraversable@ extensions.  This means that you can autogenerate a
--     valid `traversed` for these data types:
-- 
-- > {-# LANGUAGE DeriveFoldable    #-}
-- > {-# LANGUAGE DeriveFunctor     #-}
-- > {-# LANGUAGE DeriveTraversable #-}
-- >
-- > import Optics
-- > import Data.Foldable
-- >
-- > data Pair a = Pair a a deriving (Functor, Foldable, Traversable)
-- 
--     We could then use `traversed` to navigate from `Pair` to its two children:
-- 
-- > traversed :: Traversal' (Pair a) a
-- >
-- > over traverse :: (a -> a) -> (Pair a -> Pair a)
-- >
-- > over traverse (+ 1) (Pair 3 4) = Pair 4 5
-- 
--     /Question:/ How do I create traversals?
-- 
--     /Answer:/ There are three main ways to create primitive traversals:
-- 
--     * `traversed` is a `Traversal'` that you get for any type that implements
--       `Traversable`
-- 
--     * Every `Lens'` can be composed with a `Traversal'` or manually be
--       coerced into one using `castOptic`.
-- 
--     * You can use Template Haskell to generate `Traversal'`s using `makePrisms`
--       since every `Prism'` is also a `Traversal'` (not covered in this
--       tutorial)
-- 
--     /Question:/ How do I combine traversals?
-- 
--     /Answer:/ You compose them, using function composition
-- 
--     You can think of the function composition operator as having this type:
-- 
-- > (%) :: Traversal' a b -> Traversal' b c -> Traversal' a c
--
--     or also any of these types:
--
-- > (%) :: Lens' a b -> Traversal b c -> Traversal' a c
-- > (%) :: Traversal' a b -> Lens' b c -> Traversal' a c 
-- 
--     In our original @Molecule@ example, we composed four optics
--     (lenses or traversals) together to create a new `Traversal'`:
-- 
-- > -- Remember that `atoms`, `point`, and `x` are also `Traversal'`s
-- > atoms                         :: Lens'      Molecule [Atom]
-- > traversed                     :: Traversal' [Atom]   Atom
-- > point                         :: Lens'      Atom     Point
-- > x                             :: Lens'      Point    Double
-- >
-- > -- Now compose them
-- > atoms                         :: Lens'      Molecule [Atom]
-- > atoms % traversed             :: Traversal' Molecule Atom
-- > atoms % traversed % point     :: Traversal' Molecule Point
-- > atoms % traversed % point % x :: Traversal' Molecule Double
-- 
--     This composite traversal lets us get or set the @x@ coordinates of a
--     @Molecule@.
-- 
-- > over (atoms % traversed % point % x)
-- >     :: (Double -> Double) -> (Molecule -> Molecule)
-- >
-- > toListOf (atoms % traverse % point % x)
-- >     :: Molecule -> [Double]
-- 
--     /Question:/ How do I consume traversals?
-- 
--     /Answer:/ Using `toListOf`, `set` or `over`
-- 
--     Here are their types:
-- 
-- > toListOf :: Traversal' a b -> a -> [b]
-- >
-- > over :: Traversal' a b -> (b -> b) -> a -> a
-- >
-- > set  :: Traversal' a b ->       b  -> a -> a
-- > set traversal b = over traversal (\_ -> b)
--
--     Note that `toListOf` distributes over traversal composition:
-- 
-- > toListOf (traversal1 % traversal2) = (toListOf traversal1) >=> (toListOf traversal2)
-- >
-- > toListOf simple = return -- simple is the identity traversal
--
-- If you prefer object-oriented syntax you can also use (`^..`), which is an
-- infix operator equivalent to `toListOf`:
--
-- >>> Pair 3 4 ^.. traversed
-- [3,4]

{- $types
    You might wonder why you can use `over` on both a `Lens'` and a
    `Traversal'` but you can only use `view` on a `Lens'`.  We can see why by
    studying the (simplified) type of `over`:

> over :: Is k A_Setter => Optic' k is s a -> (b -> b) -> a -> a

    Both `Lens'` and `Traversal'` are defined in terms of `Optic'`:

> type Lens'      s a = Optic' A_Lens NoIx s a
> type Traversal' s a = Optic' A_Traversal NoIx s a

    There's another type of optic, called a setter. Both lenses and
    traversals can be coerced into setters using `castOptic`:

> castOptic :: Is srcKind destKind => Optic' srcKind is s a -> Optic' destKind is s a

    The `Is` class covers exactly which kinds of optics can be coerced
    into which others. Thus `over` works for lenses, traversals and even setters.

    Now let's study the (simplified) type of `view`:

> view :: Is k A_Getter => Optic' k is a b -> a -> b

    Again, both lenses and traversals can be coerced into getters.

    Let's see what happens if we try to use `view` on a `Traversal`:

>>> let atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
>>> let atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
>>> let molecule = Molecule { _atoms = [atom1, atom2] }
>>> view (atoms % traversed % element) molecule
> <interactive>:141:1: error:
>     • A_Traversal cannot be used as A_Getter
>     • In the expression: view (atoms % traversed % element) molecule
>       In an equation for ‘it’:
>           it = view (atoms % traversed % element) molecule

    However, we could still extract a single value from the traversal
    because the type targeted by the traversal is `String` which is
    a `Monoid`, and we can choose collapse the values using the monoidal operation
    `Data.Monoid.mappend`.

    For this, the library provides `foldOf` instead of `view`:

> foldOf :: (Is k A_Fold, Monoid a) => Optic' k is a b -> a -> b

>>> foldOf (atoms % traversed % element) molecule
"CO"

    If you try to extract the element from an empty molecule:

>>> foldOf (atoms % traversed % element) (Molecule { _atoms = [] })
""

    You get the empty string (i.e. `Data.Monoid.mempty`).

-}

{- $conclusion
    This tutorial covers an extremely small subset of this library.  If you
    would like to learn more, you can begin by skimming the example code in the
    following modules:

    * "Control.Lens.Getter"

    * "Control.Lens.Setter"

    * "Control.Lens.Traversal"

    * "Control.Lens.Tuple"

    * "Control.Lens.Lens"

    * "Control.Lens.Review"

    * "Control.Lens.Prism"

    * "Control.Lens.Iso"

    The documentation for these modules includes several examples to get you
    started and help you build an intuition for more advanced tricks that were
    not covered in this tutorial.

    You can also study several long-form examples here:

    <https://github.com/ekmett/lens/tree/master/examples>

    If you prefer light-weight @lens@-compatible libraries, then check out
    @lens-simple@ or @micro-lens@:

    * <http://hackage.haskell.org/package/microlens microlens>

    * <http://hackage.haskell.org/package/lens-simple lens-simple>

    If you would like a broader survey of lens features, then you can check
    out these tutorials:

    * <https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial A little lens starter tutorial> - Introduces
Prisms, Isos and JSON functionality

    * <http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html Program imperatively using Haskell lenses> - Illustrates lens support for stateful code
-}

{- $exports
    These are the same types and lenses used throughout the tutorial, exported
    for your convenience.
-}

data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)

data Pair a = Pair a a deriving (Functor, Foldable, Traversable)

$(makeLenses ''Atom)
$(makeLenses ''Point)
$(makeLenses ''Molecule)

-- These purely exist to ensure that the examples still type-check.  I don't
-- export them, though, so that they won't conflict with the user's code.
shiftAtomX :: Atom -> Atom
shiftAtomX = over (point % x) (+ 1)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms % traversed % point % x) (+ 1)

shift :: (Is k A_Setter, Num a) => Optic' k ix a a -> a -> a
shift lens = over lens (+ 1)

