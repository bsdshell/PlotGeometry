{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeFamilies              #-}

{-# LANGUAGE RankNTypes #-}   -- forall

module AronSymbolicAlgebra where
-- /Users/aaa/myfile/bitbucket/stackproject/AronModuleLib/src/xx2-2023-02-27-02-10-34.x
import           Control.Monad                (join, unless, when)
import           Control.Monad.Primitive
import           Data.Char
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Posix.Files
import           System.Posix.Unistd
import           Text.Format
import           Text.Printf

import           Control.Applicative
import qualified Data.Map.Strict              as M
import qualified Data.Text                    as TS

-- /Users/aaa/myfile/bitbucket/stackproject/AronModuleLib/src/xx-2023-02-27-02-06-14.x
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as MV

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.IO

-- xx_xx
-- KEY: symbolic algebra, symbolic calculus
-- URL: https://iagoleal.com/posts/calculus-symbolic/
--
-- Tuesday, 25 April 2023 09:52 PDT
-- BUG: Constf 0 == (negate $ Constf 0) => False
-- TODO: Fix it
-- TODO: Fix it: Constf -0 => Constf 0
--  
infixl 6 :+:      -- Left associative
infixl 7 :*:, :/: -- Left associative with higher precedence than :+:


data Fraction a = X
                | Constf a
                | (Fraction a) :+: (Fraction a)
                | (Fraction a) :*: (Fraction a)
                | (Fraction a) :/: (Fraction a)
                | Apply Func (Fraction a)
     deriving (Show, Eq)

data Func = Cos | Sin | Tan | Log | Exp | Asin | Acos | Atan deriving (Show, Eq)


eval :: Floating a => Fraction a -> a -> a
eval X c = c
eval  (Constf a) _ = a
eval (f :+: g) c = eval f c + eval g c 
eval (f :*: g) c = eval f c * eval g c
eval (f :/: g) c = eval f c / eval g c
eval (Apply f e) c = let g = calculator f 
                     in g (eval e c)

-- Ring
instance Num a => Num (Fraction a) where 
    (*) = (:*:)
    (+) = (:+:)
    negate p = Constf (-1) :*: p
    fromInteger n = Constf (fromInteger n)
    abs = error "No absolute value, SEE: AronSymbolicAlgebra Num"
    signum = error "No signum, SEE: AronSymbolicAlgebra Num"

-- Field
instance Fractional a => Fractional (Fraction a) where
    (/) = (:/:)
    fromRational r = Constf (fromRational r)

-- Real or Complex
instance Floating a => Floating (Fraction a) where
    pi = Constf pi
    sin = Apply Sin
    cos = Apply Cos
    exp = Apply Exp
    log = Apply Log
    asin = Apply Asin
    acos = Apply Acos
    atan = Apply Atan
    sinh x = (exp x - exp(-x))/2
    cosh x = (exp x + exp(-x))/2
    asinh x = log (x + sqrt (x^2 - 1))
    acosh x = log (x + sqrt (x^2 + 1))
    atanh x = (log (1 + x) - log (1 - x)) /2

calculator :: Floating a => Func -> (a -> a)
calculator Sin = sin
calculator Cos = cos 
calculator Tan = tan
calculator Log = log
calculator Exp = exp
calculator Asin = asin
calculator Acos = acos
calculator Atan = atan

cheatsheet :: Floating a => Func -> Fraction a
cheatsheet Sin = cos X
cheatsheet Cos = negate (sin X)
cheatsheet Exp = exp X
cheatsheet Log = 1 / X 
cheatsheet Asin = 1 /sqrt (1 - X^2) 
cheatsheet Acos = -1 / sqrt (1 - X^2)
cheatsheet Tan = 1 / (1 + X^2)

simplify :: (Eq a, Floating a) => Fraction a -> Fraction a
simplify f = let f' = rewrite f
             in if f == f'
                 then f 
                 else simplify f'

{-|
rewrite (f :*: (g :*: h)) = rewrite (f :*: g) :*: rewrite h 
rewrite (f :+: (g :+: h)) = rewrite (f :+: g) :+: rewrite h 
rewrite (f :/: Constf 1) = f 
rewrite (Constf a :*: Constf b) = Constf (a * b)
rewrite (Constf a :+: Constf b) = Constf (a + b)
rewrite (f :*: Constf a) = Constf a :*: f 
rewrite (Constf a :*: f :+: Constf b :*: g) | f == g = rewrite (Constf a :+: Constf b) :*: rewrite f 
                                            | otherwise = rewrite (Constf a :*: f) :+: rewrite (Constf b :*: g) 
                                              
rewrite (Apply func (Constf a)) = Constf (calculator func a)
rewrite (f :+: g) = Constf 1 :*: f :+: Constf 1 :*: g 
rewrite (f :*: g) = rewrite f :*: rewrite g
rewrite (f :/: g) = rewrite f :/: rewrite g
rewrite f = f 
-}

rewrite (Constf a :+: Constf b) = Constf (a + b) 
rewrite (Constf a :*: Constf b) = Constf (a * b)
rewrite (Constf a :/: Constf b) = Constf (a / b)
rewrite (f :+: (g :+: h)) = (rewrite f :+: rewrite g) :+: rewrite h
rewrite (f :*: (g :*: h)) = (rewrite f :*: rewrite g) :*: rewrite h
rewrite (f :+: Constf 0) = rewrite f
rewrite (Constf 0 :+: f) = rewrite f
rewrite (Constf a :+: f) = Constf a :+: rewrite f
rewrite (f :+: Constf a) = Constf a :+: rewrite f
rewrite (f :*: Constf 1) = rewrite f
rewrite (Constf 1 :*: f) = rewrite f
rewrite (Constf 0 :*: f) = Constf 0 
rewrite (f :*: Constf a) = Constf a :*: rewrite f 
rewrite (Constf 0 :/: f) = Constf 0
rewrite (f :/: Constf 1) = rewrite f 
rewrite (f :/: g) | f == g = Constf 1
rewrite ((f :*: g) :/: h) | f == h = rewrite g
                          | g == h = rewrite f 
rewrite (f :+: Constf (-1) :*: g) | f == g = Constf 0

-- rewrite (Constf a :*: f :+: Constf b :*: g) | f == g = Constf (a + b) :*: f
rewrite (f :+: g) = rewrite f :+: rewrite g

rewrite (f :*: g) = rewrite f :*: rewrite g
rewrite (f :/: g) = rewrite f :/: rewrite g

rewrite f = f


diff :: (Eq a, Floating a) => Fraction a -> Fraction a
diff X = 1 
diff (Constf _ ) = 0
diff (f :+: g) = diff f + diff g
diff (f :*: g) = diff f * g + f * diff g 
diff (f :/: g) = (diff f * g - f * diff g) / g^2
diff (Apply f e) = let f' = cheatsheet f
                   in eval f' e * diff e

-- diffS :: (Eq a, Floating a) => Fraction a -> Fraction a
diffS = simplify . diff 

-- xx_xx
