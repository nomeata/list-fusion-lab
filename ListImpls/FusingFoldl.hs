-- This is taken from GHC.List in GHC HEAD, 2014-05-09

{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, MagicHash, RankNTypes, CPP #-}
module ListImpls.FusingFoldl
#include "exports"
where

import Prelude (Maybe(..), Bool(..), Int(..), (.), otherwise, (&&), (||), String, error, id)
import GHC.Exts (Int(..), Int#, (+#), (-#), isTrue#, (>=#), (<=#), (<#))
import Data.Coerce (coerce)


build   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE [1] build #-}
build g = g (:) []

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
{-# INLINE [1] augment #-}
augment g xs = g (:) xs

foldr            :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ z []     =  z
-- foldr f z (x:xs) =  f x (foldr f z xs)
{-# INLINE [0] foldr #-}
-- Inline only in the final stage, after the foldr/cons rule has had a chance
-- Also note that we inline it when it has *two* parameters, which are the
-- ones we are keen about specialising!
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys



{-# RULES
"fold/build"    forall k z (g::forall b. (a->b->b) -> b -> b) .
                foldr k z (build g) = g k z

"foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) .
                foldr k z (augment g xs) = g k (foldr k z xs)

"foldr/id"                        foldr (:) [] = \x  -> x
"foldr/app"     [1] forall ys. foldr (:) ys = \xs -> xs ++ ys
        -- Only activate this from phase 1, because that's
        -- when we disable the rule that expands (++) into foldr
"foldr/single"  forall k z x. foldr k z [x] = k x z
"foldr/nil"     forall k z.   foldr k z []  = z

"augment/build" forall (g::forall b. (a->b->b) -> b -> b)
                       (h::forall b. (a->b->b) -> b -> b) .
                       augment g (build h) = build (\c n -> g c (h c n))
"augment/nil"   forall (g::forall b. (a->b->b) -> b -> b) .
                        augment g [] = build g
 #-}


map :: (a -> b) -> [a] -> [b]
{-# NOINLINE [1] map #-}    -- We want the RULE to fire first.
                            -- It's recursive, so won't inline anyway,
                            -- but saying so is more explicit
map _ []     = []
map f (x:xs) = f x : map f xs

-- Note eta expanded
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
{-# INLINE [0] mapFB #-}
mapFB c f = \x ys -> c (f x) ys

{-# RULES
"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (mapFB c f) n xs)
"mapList"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
  #-}

{-# RULES "map/coerce" [1] map coerce = coerce #-}

(++) :: [a] -> [a] -> [a]
{-# NOINLINE [1] (++) #-}    -- We want the RULE to fire first.
                             -- It's recursive, so won't inline anyway,
                             -- but saying so is more explicit
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

{-# RULES
"++"    [~1] forall xs ys. xs ++ ys = augment (\c n -> foldr c n xs) ys
  #-}


head                    :: [a] -> a
head (x:_)              =  x
head []                 =  badHead
{-# NOINLINE [1] head #-}

badHead :: a
badHead = errorEmptyList "head"

{-# RULES
"head/build"    forall (g::forall b.(a->b->b)->b->b) .
                head (build g) = g (\x _ -> x) badHead
"head/augment"  forall xs (g::forall b. (a->b->b) -> b -> b) .
                head (augment g xs) = g (\x _ -> x) (head xs)
 #-}

uncons                  :: [a] -> Maybe (a, [a])
uncons []               = Nothing
uncons (x:xs)           = Just (x, xs)

tail                    :: [a] -> [a]
tail (_:xs)             =  xs
tail []                 =  errorEmptyList "tail"

last                    :: [a] -> a
-- use foldl to allow fusion
last = foldl (\_ x -> x) (errorEmptyList "last")

init                    :: [a] -> [a]
init []                 =  errorEmptyList "init"
init (x:xs)             =  init' x xs
  where init' _ []     = []
        init' y (z:zs) = y : init' z zs

null                    :: [a] -> Bool
null []                 =  True
null (_:_)              =  False


{-# NOINLINE [1] length #-}
length                  :: [a] -> Int
length l                =  lenAcc l 0#

lenAcc :: [a] -> Int# -> Int
lenAcc []     a# = I# a#
lenAcc (_:xs) a# = lenAcc xs (a# +# 1#)

incLen :: a -> (Int# -> Int) -> Int# -> Int
incLen _ g x = g (x +# 1#)

-- These rules make length into a good consumer
-- Note that we use a higher-order-style use of foldr, so that
-- the accumulating parameter can be evaluated strictly
-- See Trac #876 for what goes wrong otherwise
{-# RULES
"length"     [~1] forall xs. length xs = foldr incLen I# xs 0#
"lengthList" [1]  foldr incLen I# = lenAcc
 #-}


{-# NOINLINE [1] filter #-}
filter :: (a -> Bool) -> [a] -> [a]
filter _pred []    = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      = filter pred xs

{-# NOINLINE [0] filterFB #-}
filterFB :: (a -> b -> b) -> (a -> Bool) -> a -> b -> b
filterFB c p x r | p x       = x `c` r
                 | otherwise = r

{-# RULES
"filter"     [~1] forall p xs.  filter p xs = build (\c n -> foldr (filterFB c p) n xs)
"filterList" [1]  forall p.     foldr (filterFB (:) p) [] = filter p
"filterFB"        forall c p q. filterFB (filterFB c p) q = filterFB c (\x -> q x && p x)
 #-}

foldl :: forall a b. (b -> a -> b) -> b -> [a] -> b
{-# INLINE foldl #-}
foldl k z0 xs = foldr (\(v::a) (fn::b->b) (z::b) -> fn (k z v)) (id :: b -> b) xs z0
-- Implementing foldl via foldr is only a good idea if the compiler can optimize
-- the resulting code (eta-expand the recursive "go"), so this needs -fcall-arity!
-- Also see #7994

scanl                   :: (b -> a -> b) -> b -> [a] -> [b]
scanl f q ls            =  q : (case ls of
                                []   -> []
                                x:xs -> scanl f (f q x) xs)

scanl1                  :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs)         =  scanl f x xs
scanl1 _ []             =  []

foldr1                  :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]            =  x
foldr1 f (x:xs)         =  f x (foldr1 f xs)
foldr1 _ []             =  errorEmptyList "foldr1"

scanr                   :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ q0 []           =  [q0]
scanr f q0 (x:xs)       =  f x q : qs
                           where qs@(q:_) = scanr f q0 xs

scanr1                  :: (a -> a -> a) -> [a] -> [a]
scanr1 _ []             =  []
scanr1 _ [x]            =  [x]
scanr1 f (x:xs)         =  f x q : qs
                           where qs@(q:_) = scanr1 f xs

{-# NOINLINE [1] iterate #-}
iterate :: (a -> a) -> a -> [a]
iterate f x =  x : iterate f (f x)

{-# NOINLINE [0] iterateFB #-}
iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
iterateFB c f x = x `c` iterateFB c f (f x)

{-# RULES
"iterate"    [~1] forall f x.   iterate f x = build (\c _n -> iterateFB c f x)
"iterateFB"  [1]                iterateFB (:) = iterate
 #-}

repeat :: a -> [a]
{-# INLINE [0] repeat #-}
-- The pragma just gives the rules more chance to fire
repeat x = xs where xs = x : xs

{-# INLINE [0] repeatFB #-}     -- ditto
repeatFB :: (a -> b -> b) -> a -> b
repeatFB c x = xs where xs = x `c` xs


{-# RULES
"repeat"    [~1] forall x. repeat x = build (\c _n -> repeatFB c x)
"repeatFB"  [1]  repeatFB (:)       = repeat
 #-}


{-# INLINE replicate #-}
replicate               :: Int -> a -> [a]
replicate n x           =  take n (repeat x)

cycle                   :: [a] -> [a]
cycle []                = error "Prelude.cycle: empty list"
cycle xs                = xs' where xs' = xs ++ xs'

takeWhile               :: (a -> Bool) -> [a] -> [a]
takeWhile _ []          =  []
takeWhile p (x:xs)
            | p x       =  x : takeWhile p xs
            | otherwise =  []

dropWhile               :: (a -> Bool) -> [a] -> [a]
dropWhile _ []          =  []
dropWhile p xs@(x:xs')
            | p x       =  dropWhile p xs'
            | otherwise =  xs

take                   :: Int -> [a] -> [a]
drop                   :: Int -> [a] -> [a]
splitAt                :: Int -> [a] -> ([a],[a])

{-# RULES
"take"     [~1] forall n xs . take n xs = takeFoldr n xs
"takeList"  [1] forall n xs . foldr (takeFB (:) []) (takeConst []) xs n = takeUInt n xs
 #-}

{-# INLINE takeFoldr #-}
takeFoldr :: Int -> [a] -> [a]
takeFoldr (I# n#) xs
  = build (\c nil -> if isTrue# (n# <=# 0#) then nil else
                     foldr (takeFB c nil) (takeConst nil) xs n#)

{-# NOINLINE [0] takeConst #-}
-- just a version of const that doesn't get inlined too early, so we
-- can spot it in rules.  Also we need a type sig due to the unboxed Int#.
takeConst :: a -> Int# -> a
takeConst x _ = x


{-# INLINE [0] takeFB #-}
takeFB :: (a -> b -> b) -> b -> a -> (Int# -> b) -> Int# -> b
-- The \m accounts for the fact that takeFB is used in a higher-order
-- way by takeFoldr, so it's better to inline.  A good example is
--     take n (repeat x)
-- for which we get excellent code... but only if we inline takeFB
-- when given four arguments
takeFB c n x xs
  = \ m -> if isTrue# (m <=# 1#)
           then x `c` n
           else x `c` xs (m -# 1#)

{-# INLINE [0] take #-}
take (I# n#) xs = takeUInt n# xs

takeUInt :: Int# -> [b] -> [b]
takeUInt n xs
  | isTrue# (n >=# 0#) = take_unsafe_UInt n xs
  | otherwise          = []

take_unsafe_UInt :: Int# -> [b] -> [b]
take_unsafe_UInt 0#  _  = []
take_unsafe_UInt m   ls =
  case ls of
    []     -> []
    (x:xs) -> x : take_unsafe_UInt (m -# 1#) xs

takeUInt_append :: Int# -> [b] -> [b] -> [b]
takeUInt_append n xs rs
  | isTrue# (n >=# 0#) = take_unsafe_UInt_append n xs rs
  | otherwise          = []

take_unsafe_UInt_append :: Int# -> [b] -> [b] -> [b]
take_unsafe_UInt_append 0#  _ rs  = rs
take_unsafe_UInt_append m  ls rs  =
  case ls of
    []     -> rs
    (x:xs) -> x : take_unsafe_UInt_append (m -# 1#) xs rs


drop (I# n#) ls
  | isTrue# (n# <# 0#) = ls
  | otherwise          = drop# n# ls
    where
        drop# :: Int# -> [a] -> [a]
        drop# 0# xs      = xs
        drop# _  xs@[]   = xs
        drop# m# (_:xs)  = drop# (m# -# 1#) xs

splitAt (I# n#) ls
  | isTrue# (n# <# 0#) = ([], ls)
  | otherwise          = splitAt# n# ls
    where
        splitAt# :: Int# -> [a] -> ([a], [a])
        splitAt# 0# xs     = ([], xs)
        splitAt# _  xs@[]  = (xs, xs)
        splitAt# m# (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt# (m# -# 1#) xs

span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)


break                   :: (a -> Bool) -> [a] -> ([a],[a])
-- HBC version (stolen)
break _ xs@[]           =  (xs, xs)
break p xs@(x:xs')
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)


reverse                 :: [a] -> [a]
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)

and                     :: [Bool] -> Bool
or                      :: [Bool] -> Bool
and []          =  True
and (x:xs)      =  x && and xs
or []           =  False
or (x:xs)       =  x || or xs

{-# NOINLINE [1] and #-}
{-# NOINLINE [1] or #-}

{-# RULES
"and/build"     forall (g::forall b.(Bool->b->b)->b->b) .
                and (build g) = g (&&) True
"or/build"      forall (g::forall b.(Bool->b->b)->b->b) .
                or (build g) = g (||) False
 #-}

-- | Map a function over a list and concatenate the results.
concatMap               :: (a -> [b]) -> [a] -> [b]
concatMap f             =  foldr ((++) . f) []

-- | Concatenate a list of lists.
concat :: [[a]] -> [a]
concat = foldr (++) []

{-# NOINLINE [1] concat #-}

{-# RULES
  "concat" forall xs. concat xs = build (\c n -> foldr (\x y -> foldr c y x) n xs)
-- We don't bother to turn non-fusible applications of concat back into concat
 #-}

errorEmptyList :: String -> a
errorEmptyList fun =
  error (prel_list_str ++ fun ++ ": empty list")

prel_list_str :: String
prel_list_str = "Prelude."


inits                   :: [a] -> [[a]]
inits xs                =  [] : case xs of
                                  []      -> []
                                  x : xs' -> map (x :) (inits xs')

