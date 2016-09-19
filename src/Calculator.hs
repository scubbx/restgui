-----------------------------------------------------------------------------
--
-- Module      :  Calculator
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Calculator (
    add,
    mult,
    sub,
    endl
) where

add :: Num a => a -> a -> a
add x y = x + y

mult :: Num a => a -> a -> a
mult x y = x * y

sub :: Num a => a -> a -> a
sub x y = x - y

endl :: [Integer]
endl = [1..]
