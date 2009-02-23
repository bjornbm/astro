-- CVS version control block - do not edit manually
--  $RCSfile: Fad.hs,v $
--  $Revision: 1.23 $
--  $Date: 2008-08-20 12:17:48 $
--  $Source: /home/cvs/stalingrad/documentation/haskell/Fad.hs,v $

{-# LANGUAGE ScopedTypeVariables #-}

-- Forward Automatic Differentiation
module Tmp.Fad (lift, Dual,
            diffUU, diffUF, diffMU, diffMF,
            diffUU2, diffUF2, diffMU2, diffMF2,
            diff, grad, jacobian,
            zeroNewton, inverseNewton, fixedPointNewton, extremumNewton)
where

import List(transpose)

version = "$Id: Fad.hs,v 1.23 2008-08-20 12:17:48 bap Exp $"

-- Forward Automatic Differentiation via overloading to perform
-- nonstandard interpretation that replaces original numeric type with
-- corresponding dual number type.

-- License:

--  Copyright (C) 2008 Barak A. Pearlmutter & Jeffrey Mark Siskind
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with this program; if not, write to the Free Software Foundation, Inc.,
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

-- Credits:

--  Authors: Copyright 2008,
--     Barak A. Pearlmutter <barak@cs.nuim.ie> &
--     Jeffrey Mark Siskind <qobi@purdue.edu>

--  Work started as stripped-down version of higher-order tower code
--  published by Jerzy Karczmarczuk <jerzy.karczmarczuk@info.unicaen.fr>
--  which used a non-standard standard prelude.

--  Initial perturbation-confusing code is a modified version of
--  http://cdsmith.wordpress.com/2007/11/29/some-playing-with-derivatives/

--  Tag trick, called "branding" in the Haskell community, from
--  Bj\"orn Buckwalter <bjorn.buckwalter@gmail.com>
--  http://thread.gmane.org/gmane.comp.lang.haskell.cafe/22308/

-- To Do:

-- Extend to an infinite tower of derivatives (power series) rather
-- than just the first derivative.  Perhaps this could use the
-- existing power series module.

-- Add some more optimization routines, and some examples.

-- Notes:

-- Each invocation of the differentiation function introduces a
-- distinct perturbation, which requires a distinct dual number type.
-- In order to prevent these from being confused, tagging, called
-- branding in the Haskell community, is used.  This seems to prevent
-- perturbation confusion, although it would be nice to have an actual
-- proof of this.  The technique does require adding invocations of
-- lift at appropriate places when nesting is present.

-- The "pass in a lifter" approach, discussed in the HOSC paper, was
-- rediscovered and implemented by David Roundy <droundy@darcs.net>
-- http://thread.gmane.org/gmane.comp.lang.haskell.cafe/22308
-- Unfortunately this would preclude writing in a natural style, and
-- is therefore not used here.  It however could be combined with
-- tagging to allow dynamic nesting, if the type system would allow.

-- The constructor is "Bundle" because dual numbers are tangent-vector
-- bundles, in the terminology of differential geometry.  For the same
-- reason, the accessor is "tangent".

-- The multivariate case is handled as a list on inputs, but an
-- arbitrary functor on outputs.  This asymmetry is because Haskell
-- provides fmap but not fzipWith.

-- Other quirks:

--  would need diffUFF for f:R->[[R]] and diffUFFF for f:R->[Maybe [R]], etc.

--  type signature of diff stuff contaminates diff-using stuff, ick

data Dual tag a = Bundle a a deriving Show

lift :: Num a => a -> Dual tag a
lift = flip Bundle 0

-- Some care has been taken to ensure that correct interoperation with
-- complex numbers.  Particular care must be taken with the
-- non-analytic functions, namely abs and signum.  These are not
-- properly differentiable over the complex domain, but they do admit
-- directional derivatives (push-forwards) where both the primal and
-- tangents are complex.  Unfortunately the Haskell numeric system is
-- modularized in a fashion that appears to make this impossible to
-- express.  Instead, we detect the situation and handle it
-- appropriately.

-- Digression: the reason for this difficulty.

-- The relationship of interest, valid for both real and complex
-- valued x, is:

--   x = (abs x) * (signum x)

-- This gives rise to division in the formulas for abs and signum when
-- x is complex.  For real x, (signum x) = 1/(signum x), so the
-- division can be replaced by multiplication.  But not in the complex
-- case.  And unfortunately, abs & signum are defined for Num, which
-- does not have division.

-- The "right" solution would be to have signumRecip defined for Num,
-- where (signumRecip x) * (signum x) = 1.  This would allow things to
-- be defined so as to be correct in the case of complex numbers
-- without using division, and would be trivial in the real case where
-- signum x = signumRecip x.  Moreover (magnitude (signum z)) == 1, so
-- we could use signumRecip = conjugate . signum for complex numbers.
-- Except that conjugate is defined only for complex numbers, which is
-- strange since it would just be the identity for real numbers.

instance Num a => Num (Dual tag a) where
    (Bundle x x') + (Bundle y y') = Bundle (x + y)    (x' + y')
    (Bundle x x') - (Bundle y y') = Bundle (x - y)    (x' - y')
    (Bundle x x') * (Bundle y y') = Bundle (x * y)    (x' * y + x * y')
    negate (Bundle x x')          = Bundle (negate x) (negate x')
    abs (Bundle 0 _)              = Bundle 0 (error "not differentiable: abs(0)")
    abs (Bundle x x')             = let s = signum x; s' = signum x'
                                    in Bundle (abs x)
                                       (if (s == 1 || s == (-1))
                                               && (s' == 1 || s' == (-1))
                                        then (x' * s)
                                        else error "not differentiable: abs(complex)")
    signum (Bundle 0 _)           = Bundle 0 (error "not differentiable: signum(0)")
    signum (Bundle x x')          = let s = signum x; s' = signum x'
                                    in Bundle s
                                       (if (s == 1 || s == (-1))
                                               && (s' == 1 || s' == (-1))
                                        then 0
                                        else error "not differentiable: signum(complex)")
    fromInteger i                 = lift (fromInteger i)

-- Another problem supporting complex numbers!  This is a show stopper
-- for doing transparent forward AD on functions that are explicitly
-- over complex numbers.

-- The following will not work:

-- instance Complex a => Complex (Dual tag a) where
--     realPart  (Bundle x x') = Bundle (realPart x)  (realPart x')
--     imagPart  (Bundle x x') = Bundle (imagPart x)  (imagPart x')
--     conjugate (Bundle x x') = Bundle (conjugate x) (conjugate x')
--     ...

-- This fails because Complex in the standard prelude is not abstract
-- enough.  It is impossible to make (Dual (Complex a)) a complex
-- number; the system can only do (Complex (Dual a)).  This makes it
-- impossible to take derivatives of complex functions using the same
-- API as non-complex functions.

instance Fractional a => Fractional (Dual tag a) where
    recip (Bundle x x')           = Bundle z (- x' * z * z) where z = recip x
    fromRational x                = lift (fromRational x)

instance Floating a => Floating (Dual tag a) where
    pi                  = lift pi
    exp   (Bundle x x') = Bundle z         (x' * z) where  z = exp x
    sqrt  (Bundle x x') = Bundle z         (x' / (2 * z)) where z = sqrt x
    log   (Bundle x x') = Bundle (log x)   (x' / x)
    (Bundle x x') ** (Bundle y y')
                        = Bundle z         (x' * y * z / x + y' * z * log x)
                          where z = x ** y
    sin   (Bundle x x') = Bundle (sin x)   ( x' * cos x)
    cos   (Bundle x x') = Bundle (cos x)   (-x' * sin x)
    asin  (Bundle x x') = Bundle (asin x)  ( x' / sqrt (1 - x^2))
    acos  (Bundle x x') = Bundle (acos x)  (-x' / sqrt (1 - x^2))
    atan  (Bundle x x') = Bundle (atan x)  (x' / (1 + x^2))
    sinh  (Bundle x x') = Bundle (sinh x)  (x' * cosh x)
    cosh  (Bundle x x') = Bundle (cosh x)  (x' * sinh x)
    asinh (Bundle x x') = Bundle (asinh x) (x' / sqrt (x^2 + 1))
    acosh (Bundle x x') = Bundle (acosh x) (x' / sqrt (x^2 - 1))
    atanh (Bundle x x') = Bundle (atanh x) (x' / (1 - x^2))

-- This is mainly to get atan2 to work, which is by inheritance.

-- Note that atan2 is important in numeric code: to a first
-- approximation, no program should ever use atan, but always atan2
-- instead.  In particular it is virtually impossible for "atan (y/x)"
-- to be correct; "atan2 y x" is almost certainly what is really meant.

{-

 Warning!  If we do not give a special definition of atan2 below,
 instead allowing it to default, there are errors at branch points, in
 particular at (atan2 1 0).  These occur with "probability zero"
 around the unit circule, so you might not notice them with random
 testing.

 *Fad> let shouldBeOne a = diff (\a->atan2 (sin a) (cos a)) a

 *Fad> shouldBeOne (pi/2-1e12)
 1.0

 *Fad> shouldBeOne (pi/2)
 1.0

 *Fad> shouldBeOne (pi/2+1e12)
 1.0

 *Fad> diff shouldBeOne (pi/2-1e12)
 0.0

 *Fad> diff shouldBeOne (pi/2)
 -4.0                          -- <<<<<<<<<<<<<<<< BUG IS HERE

 *Fad> diff shouldBeOne (pi/2+1e12)
 0.0

-}

instance (RealFloat a, RealFrac a) => RealFloat (Dual tag a) where
    floatRadix  (Bundle x x')    = floatRadix x
    floatDigits (Bundle x x')    = floatDigits x
    floatRange  (Bundle x x')    = floatRange x
    decodeFloat (Bundle x x')    = decodeFloat x
    encodeFloat n i              = Bundle (encodeFloat n i)
                                   (error "not differentiable: encodeFloat")
    scaleFloat   i (Bundle x x') = Bundle z (x' * z / x)
        where z = scaleFloat i x
    isNaN          (Bundle x x') = isNaN x
    isInfinite     (Bundle x x') = isInfinite x
    isDenormalized (Bundle x x') = isDenormalized x
    isNegativeZero (Bundle x x') = isNegativeZero x
    isIEEE         (Bundle x x') = isIEEE x
    atan2 (Bundle y y') (Bundle x x')
                                 = Bundle (atan2 y x) ((y'*x-x'*y)/(x^2+y^2))

instance RealFrac a => RealFrac (Dual tag a) where
    properFraction (Bundle x x') = (z1, (Bundle z2 x'))
        where (z1,z2) = properFraction x
    truncate       (Bundle x x') = truncate x
    round          (Bundle x x') = round x
    ceiling        (Bundle x x') = ceiling x
    floor          (Bundle x x') = floor x

instance Real a => Real (Dual tag a) where
    toRational (Bundle x x') = toRational x

instance Eq a => Eq (Dual tag a) where
    (Bundle x x') == (Bundle y y')  =  x == y

instance Ord a => Ord (Dual tag a) where
    (Bundle x x') `compare` (Bundle y y')  =  x `compare` y

instance (Enum a, Num a) => Enum (Dual tag a) where
    succ     (Bundle x x') = Bundle (succ x) x'
    pred     (Bundle x x') = Bundle (pred x) x'
    fromEnum (Bundle x x') = fromEnum x
    toEnum                 = lift . toEnum

-- Differentiation operators.  These have two-letter suffices for the
-- arity of the input and output of the passed functions: U for
-- univariate, meaning a number, M for multivariate, meaning a list of
-- numbers.

-- Perhaps these should be named things like
--   AD.Forward.D.uu
--   AD.Forward.D.um
--   AD.Forward.grad
--   AD.Forward.jacobian

-- When the input is multivariate a directional derivative is
-- calculated; this requires an additional "direction" parameter.  The
-- multivariate case is treated as a list (on input) and as a functor
-- of arbitrary shape, which includes lists as a special case, on
-- output.

diffUU :: Num a => (forall tag. Dual tag a -> Dual tag b) -> a -> b
diffUU f = tangent . f . flip Bundle 1

diffUF :: (Num a, Functor f) =>
          (forall tag. Dual tag a -> f (Dual tag b)) -> a -> f b
diffUF f = fmap tangent . f . flip Bundle 1

diffMU :: Num a =>
          (forall tag. [Dual tag a] -> Dual tag b) -> [a] -> [a] -> b
diffMU f xs = tangent . f . zipWithBundle xs

diffMF :: (Num a, Functor f) =>
          (forall tag. [Dual tag a] -> f (Dual tag b)) -> [a] -> [a] -> f b
diffMF f xs = fmap tangent . f . zipWithBundle xs

-- value and derivative as a pair, for all combos uni/multi in/out

diffUU2 :: Num a => (forall tag. Dual tag a -> Dual tag b) -> a -> (b,b)
diffUU2 f = dual2pair . f . flip Bundle 1

diffUF2 :: (Functor f, Num a) =>
           (forall tag. Dual tag a -> f (Dual tag b)) -> a -> (f b, f b)
diffUF2 f = fduals2pair . f . flip Bundle 1

diffMU2 :: (forall tag. [Dual tag a] -> Dual tag b) -> [a] -> [a] -> (b,b)
diffMU2 f xs = dual2pair . f . zipWithBundle xs

diffMF2 :: Functor f =>
           (forall tag. [Dual tag a] -> f (Dual tag b))
               -> [a] -> [a] -> (f b, f b)
diffMF2 f xs = fduals2pair . f . zipWithBundle xs

-- Common access patterns

diff :: Num a => (forall tag. Dual tag a -> Dual tag b) -> a -> b
diff = diffUU

grad :: Num a => (forall tag. [Dual tag a] -> Dual tag b) -> [a] -> [b]
-- grad f = head . jacobian ((:[]) . f) -- Robot face, robot claw!
grad f xs = map (diffMU f xs) (identity xs)

jacobian :: Num a =>
            (forall tag. [Dual tag a] -> [Dual tag b]) -> [a] -> [[b]]
jacobian f xs = transpose $ map (diffMF f xs) (identity xs)

-- Utility functions for shared code in above

tangent :: Dual tag a -> a
tangent (Bundle _ x') = x'

ftangent :: Functor f => f (Dual tag a) -> f a
ftangent = fmap tangent

primal :: Dual tag a -> a
primal (Bundle x _) = x

fprimal :: Functor f => f (Dual tag a) -> f a
fprimal = fmap primal

flift :: (Functor f, Num a) => f a -> f (Dual tag a)
flift = fmap lift

dual2pair :: Dual tag a -> (a,a)
dual2pair (Bundle x x') = (x, x')

fduals2pair :: Functor f => f (Dual tag a) -> (f a, f a)
fduals2pair fxs = (fprimal fxs, ftangent fxs)

zipWithBundle :: [a] -> [a] -> [Dual tag a]
zipWithBundle [] [] = []
zipWithBundle (x:xs) (y:ys) = (Bundle x y):(zipWithBundle xs ys)
zipWithBundle _ _ = error "zipWithBundle arguments, lengths differ"

-- Lower a function over duals to a function over primals.
-- Four variants, for unary/functorized domain/range.

lowerUU :: (Num a, Num b) =>
           (forall tag. Dual tag a -> Dual tag b) -> a -> b
lowerUU f = primal . f . lift

lowerUF :: (Num a, Functor fb, Num b) =>
            (forall tag. Dual tag a -> fb (Dual tag b)) -> a -> (fb b)
lowerUF f = fprimal . f . lift

lowerFU :: (Functor fa, Num a, Num b) =>
           (forall tag. fa (Dual tag a) -> Dual tag b) -> (fa a) -> b
lowerFU f = primal . f . flift

lowerFF :: (Functor fa, Num a, Functor fb, Num b) =>
           (forall tag. fa (Dual tag a) -> fb (Dual tag b))
               -> (fa a) -> (fb b)
lowerFF f = fprimal . f . flift

-- Create identity matrix, represented as list of lists of numbers.

identity :: Num a => [b] -> [[a]]
identity [] = error "cannot create 0-dimensional identity matrix"
identity xs
    = [unit i xs | (i,_) <- zip [0..] xs]
      where unit i xs = [if j==i then 1 else 0 | (j,_) <- zip [0..] xs]

-- Format matrix for convenient examination.  Also works on vectors.

show2d :: Show a => [a] -> String
show2d = ("["++) . (++"]\n") . (foldl1 $ (++) . (++"\n ")) . map show

-- Optimization
-- Perhaps these should be in a module, named things like
--   AD.Forward.Newton.findZero
--   AD.Forward.Newton.inverse

-- Find a zero of a unary function using Newton's method; produces a
-- stream of increasingly accurate results.

-- TEST CASE:
--  take 10 $ zeroNewton (\x->x^2-4) 1  -- converge to 2.0

-- TEST CASE
--  :module Complex Fad
--  take 10 $ zeroNewton ((+1).(^2)) (1 :+ 1)  -- converge to (0 +: 1)

zeroNewton :: Fractional a =>
              (forall tag. Dual tag a -> Dual tag a) -> a -> [a]
zeroNewton f x0 = iterate (\x -> let (y,y') = diffUU2 f x in x - y/y') x0

-- Invert a unary function using Newton's method; produces a stream of
-- increasingly accurate results.

-- TEST CASE:
--   take 10 $ inverseNewton sqrt 1 (sqrt 10)  -- converge to 10

inverseNewton :: Fractional a =>
                 (forall tag. Dual tag a -> Dual tag a)
                     -> a -> a -> [a]
inverseNewton f x0 y = zeroNewton (\x -> (f x) - (lift y)) x0

-- Find a fixedpoint of a unary function using Newton's method;
-- produces a stream of increasingly accurate results.

fixedPointNewton :: Fractional a =>
                    (forall tag. Dual tag a -> Dual tag a) -> a -> [a]
fixedPointNewton f x0 = zeroNewton (\x -> (f x) - x) x0

-- Find an extremum of a unary function using Newton's method;
-- produces a stream of increasingly accurate results.

extremumNewton :: Fractional a =>
                  (forall tag. forall tag1.
                          Dual tag1 (Dual tag a) -> Dual tag1 (Dual tag a))
                      -> a -> [a]
extremumNewton f x0 = zeroNewton (diffUU f) x0

-- Multivariate optimization, based on naive-gradient-descent from
-- stalingrad/examples/flow-tests/pre-saddle-1a.vlad
-- Produces stream of increasingly accurate results.

argminNaiveGradient :: (Fractional a, Ord a) =>
                       (forall tag. [Dual tag a] -> Dual tag a)
                           -> [a] -> [[a]]
argminNaiveGradient f x0 =
    let
        gf = grad f
        loop x fx gx eta i =
            -- should check gx = 0 here
            let
                x1 = zipWith (+) x (map ((-eta)*) gx)
                fx1 = lowerFU f x1
                gx1 = gf x1
            in
              if eta == 0 then []
              else if (fx1 > fx) then loop x fx gx (eta/2) 0
                   else if all (==0) gx then []
                        -- else if fx1 == fx then loop x1 fx1 gx1 eta (i+1)
                        else x1:(if (i==10)
                                 then loop x1 fx1 gx1 (eta*2) 0
                                 else loop x1 fx1 gx1 eta (i+1))
    in
      loop x0 (lowerFU f x0) (gf x0) 0.1 0

-- BUGS!  BUGS!  BUGS!  Or, test cases.

{-

shouldBe2 = diffUU (\x -> x*(diffUU (x*) 2)) 1     -- type error w/ or w/o tags
is2       = diffUU (\x -> x*(diffUU ((lift x)*) 2)) 1
shouldBe1 = diffUU (\x -> diffUU (x*) 2) 1         -- is 0 w/o tags, type error w/ tags
is1       = diffUU (\x -> diffUU ((lift x)*) 2) 1

constant_one x = diffUU (\y -> x + y) 1 -- fails type check w/ tags

-- Successful tests of directional derivative:

-- should be [54321]
diffMF (\xs -> [sum (zipWith (*) xs [1..5])]) [1,1,1,1,1] (map (10^) [0..4])

-- should be [[1.0,1.0,1.0,1.0,1.0],[120.0,60.0,40.0,30.0,24.0],[1.0,1.0,1.0,1.0,1.0]]
jacobian (\xs->[sum xs,product xs,log $ product $ map (sqrt . (^2) . exp) xs]) [1..5]

-- should be [0,1,2,3,4]
diffMF id [10..14] [0..4]

-- Higher-Order derivatives via nesting, fails type check

ds :: (forall tag. Dual tag a -> Dual tag b) -> a -> [b]

ds f x = y:(ds (diffUU f) x)
    where (y,y') = diffUU2 f x

ds f x = (f x):(ds (diffUU f) x)

ds f x = y:y':(ds (diffUU (diffUU f)) x)
    where (y,y') = diffUU2 f x

-}
