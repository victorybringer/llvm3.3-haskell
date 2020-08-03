{-# LANGUAGE TypeOperators  #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Relation
-- Copyright   :  (c) 2011 University of Minho
-- License     :  BSD3
--
-- Maintainer  :  hpacheco@di.uminho.pt
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Pointless Lenses:
-- bidirectional lenses with point-free programming
-- 
-- This module defines relations as sets of pairs and provides some typical point-free combinators for their manipulation.
--
-----------------------------------------------------------------------------

module Data.Relation where

import qualified Data.Set as Set
import Data.Set (Set)

-----------------------------------------------------------------------------
-- * Representation

-- | Type of relations
type Rel a b = Set (a,b)
infixr 8 :<-:
type b :<-: a = Rel a b
infixr 8 :->:
type a :->: b = Rel a b

-- | Build a relation from a list of pairs.
mkRel :: (Ord a, Ord b) => [(a,b)] -> Rel a b
mkRel pairs = Set.fromList pairs

-- | Convert relation to a list of pairs.
pairs :: (Ord a, Ord b) => Rel a b -> [(a,b)]
pairs r = Set.elems r

-- | Obtain the domain of a relation
dom :: (Ord a, Ord b) => Rel a b -> Set a
dom xs = Set.map fst xs

-- | Obtain the range of a relation
rng :: (Ord a, Ord b) => Rel a b -> Set b
rng xs = Set.map snd xs

-- | domain for a specific range value
domOf :: (Eq b,Ord a) => b -> Rel a b -> Set a
domOf b r = Set.fromList [ x | (x,y) <- Set.elems r, y == b ]

-- | range for a specific doman value
rngOf :: (Eq a,Ord b) => a -> Rel a b -> Set b
rngOf a r = Set.fromList [ y | (x,y) <- Set.elems r, x == a]

-- * Relational combinators      

-- | Build an empty relation.              
emptyR :: Rel a b
emptyR = Set.empty

-- | Build identity relation, which contains an edge from each node to itself.
idR :: Ord a => Set a -> Rel a a
idR s = Set.map (\x -> (x,x)) s

-- | Build total relation, which contains an edge from each node to 
--   each other node and to itself.
total :: Ord a => Set a -> Rel a a
total s = Set.fromList [ (x,y) |  x <- l, y <- l ]
    where l = Set.elems s

-- | Take the inverse of a relation
inv :: (Ord a, Ord b) => Rel a b -> Rel b a
inv xs = Set.map (\(x,y) -> (y,x)) xs

-- | Union of two relations
unionR :: (Ord a,Ord b) => Rel a b -> Rel a b -> Rel a b
unionR r s = Set.union r s

-- | Intersection of two relations
intersectionR :: (Ord a,Ord b) => Rel a b -> Rel a b -> Rel a b
intersectionR r s = Set.intersection r s

-- | Compose two relations
infixr 8 .~
(.~) :: (Ord a, Eq b, Ord c) => Rel b c -> Rel a b -> Rel a c
(.~) r s = mkRel [ (x,z) | (x,y) <- Set.elems s , (y',z) <- Set.elems r , y==y' ]

funR :: (Ord a,Ord b) => (a -> b) -> Set a -> Rel a b
funR f sa = mkRel [ (x,f x) | x <- Set.elems sa ]

infixr 9 <.~
(<.~) :: (Ord a, Ord b, Ord b') => (b -> b') -> Rel a b -> Rel a b'
f <.~ r = Set.map (\(x,y) -> (x,f y)) r

infixr 9 ~.>
(~.>) :: (Ord a, Ord b, Ord a') => Rel a b -> (a -> a') -> Rel a' b
r ~.> f = Set.map (\(x,y) -> (f x,y)) r

inlR :: (Ord a,Ord b) => Set a -> Rel a (Either a b)
inlR s = mkRel [ (x,Left x) | x <- Set.elems s ]

inrR :: (Ord a,Ord b) => Set b -> Rel b (Either a b)
inrR s = mkRel [ (x,Right x) | x <- Set.elems s ]

-- | The infix either combinator.
infixr 4 \/~
(\/~) :: (Ord a,Ord b,Ord c) => Rel b a -> Rel c a -> Rel (Either b c) a
(\/~) r s = (r .~ inv (inlR (dom r))) `unionR` (s .~ inv (inrR (dom s)))

-- | The infix sum combinator.
infix 5 -|-~
(-|-~) :: (Ord a,Ord b,Ord c,Ord d) => Rel a b -> Rel c d -> Rel (Either a c) (Either b d)
f -|-~ g = inlR (rng f) .~ f \/~ inrR (rng g) .~ g

fstR :: (Ord a,Ord b) => Set (a,b) -> Rel (a,b) a
fstR s = mkRel [((x,y),x) | (x,y) <- Set.elems s ]

sndR :: (Ord a,Ord b) => Set (a,b) -> Rel (a,b) b
sndR s = mkRel [((x,y),y) | (x,y) <- Set.elems s ]

infix 6  /\~
(/\~) :: (Ord a,Ord b,Ord c) => Rel a b -> Rel a c -> Rel a (b,c)
r /\~ s = (inv (fstR bc) .~ r) `intersectionR` (inv (sndR bc) .~ s)
    where bc = Set.fromList [ (x,y) | x <- Set.elems (rng r) , y <- Set.elems (rng s) ]

infix 7 ><~
(><~) :: (Ord a,Ord b,Ord c,Ord d) => Rel a c -> Rel b d -> Rel (a,b) (c,d)
r ><~ s = r .~ fstR ab /\~ s .~ sndR ab
    where ab = Set.fromList [ (x,y) | x <- Set.elems (dom r) , y <- Set.elems (dom s) ]

-- | Kernel of a relation.
ker :: (Ord a, Ord b) => Rel a b -> Rel a a
ker r = inv r .~ r

-- | Image of a relation.
img :: (Ord a, Ord b) => Rel a b -> Rel b b
img r = r .~ inv r
