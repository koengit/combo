module Main where

import qualified Data.Map as M
import Data.Map( Map )
import qualified Data.Set as S
import Data.Set( Set )
import Control.Monad
import Test.QuickCheck

-- Var & Val ------------------------------------------------------------------------

type Var
  = Int

data Val
  = Var Var
  | TRUE
  | FALSE
 deriving ( Ord, Eq, Show )

-- Gate -----------------------------------------------------------------------------

data Gate
  = And Val Val
  | Not Val
  | Con Val
 deriving ( Ord, Eq, Show )

fvs :: Gate -> Set Var
fvs (And x y) = S.fromList [ v | Var v <- [x,y] ]
fvs (Not x)   = S.fromList [ v | Var v <- [x] ]
fvs (Con x)   = S.fromList [ v | Var v <- [x] ]

eval :: Map Var Bool -> Gate -> Maybe Bool
eval mp g = gate g
 where
  is b TRUE    = b
  is b FALSE   = not b
  is b (Var x) = M.lookup x mp == Just b

  gate (And x y)
    | is False x             = Just False
    | is False y             = Just False
    | is True x && is True y = Just True
    | otherwise              = Nothing

  gate (Not x)
    | is False x = Just True
    | is True  x = Just False
    | otherwise  = Nothing

  gate (Con x)
    | is False x = Just False
    | is True  x = Just True
    | otherwise  = Nothing

-- Circuit --------------------------------------------------------------------------

type Circuit = [(Var,Gate)]

sim3 :: Circuit -> Map Var Bool
sim3 c = loop M.empty c
 where
  loop mp c
    | mp == mp' = mp
    | otherwise = loop mp' c
   where
    mp' = run mp c

  run mp []        = mp
  run mp ((x,g):c) =
    case eval mp g of
      Nothing -> run mp c
      Just b  -> mp' `seq` run mp' c
       where
        mp' = M.insert x b mp

order :: [Var] -> Circuit -> Circuit
order outs c = fst (go S.empty S.empty outs)
 where
  def = M.fromList c

  go busy seen [] = ([], seen)
  go busy seen (x:xs)
    | x `S.member` busy = go busy seen xs
    | x `S.member` seen = go busy seen xs
    | otherwise         =
      case M.lookup x def of
        Nothing -> go busy seen xs
        Just g  -> (ds1 ++ [(x,g)] ++ ds2, seen2)
         where
          (ds1, seen1) = go (S.insert x busy) seen (S.toList (fvs g))
          (ds2, seen2) = go busy (S.insert x seen1) xs

simp :: [Var] -> Circuit -> Circuit
simp outs c
  | c == c'   = c
  | otherwise = simp outs c'
 where
  c' = order outs (prop c)

prop :: Circuit -> Circuit
prop c = go M.empty M.empty c
 where
  go vmp gmp [] =
    []

  go vmp gmp ((x,g):c) =
    case gate vmp g of
      And FALSE _    -> go1 FALSE
      And _    FALSE -> go1 FALSE
      And TRUE  y    -> go1 y
      And y     TRUE -> go1 y
      And p q | p==q -> go1 p
      Not FALSE      -> go1 TRUE
      Not TRUE       -> go1 FALSE
      Con v          -> go1 v
      g' ->
        case M.lookup g' gmp of
          Just y  -> go1 (Var y)
          Nothing -> (x,g') : go vmp (M.insert g' x gmp) c
   where
    go1 v = (x,Con v) : go (M.insert x v vmp) gmp c

  gate vmp (And x y)
    | x' <= y'  = And x' y'
    | otherwise = And y' x'
   where
    (x',y') = (look x vmp, look y vmp)
  gate vmp (Not x) = Not (look x vmp)
  gate vmp (Con x) = Con (look x vmp)

  look (Var x) vmp =
    case M.lookup x vmp of
      Nothing -> Var x
      Just v  -> v
  look v vmp       = v

dloop :: Circuit -> Circuit
dloop c =
  case find S.empty c of
    Nothing -> c
    Just y  -> expand y c
 where
  ys = S.fromList ([ y | (x,g) <- c, y <- x : S.toList (fvs g) ])

  find used [] =
    Nothing

  find used ((x,g):c)
    | x `S.member` used = Just x
    | otherwise         = find (used `S.union` fvs g) c

  expand y ((x,g):c)
    | y `S.member` fvs g = copy y ((x,g):c)
    | otherwise          = (x,g) : expand y c

  copy y c =
    [ (pr y, Con FALSE) ]
    ++
    [ (pr z, prgate g)
    | (z,g) <- c1
    ]
    ++
    [ (y, prgate g)
    | (y,g) <- take 1 c2
    ]
    ++
    c1
    ++
    tail c2
   where
    c1 = takeWhile ((/=y).fst) c
    c2 = dropWhile ((/=y).fst) c
    zs = S.fromList (y : [ z | (z,_) <- c1 ])

    prmap = M.fromList (primes (S.toList zs) ys)
     where
      primes [] ys = []
      primes (z:zs) ys = (z,z') : primes zs (S.insert z' ys)
       where
        z':_ = [ z' | z' <- [z..], not (z' `S.member` ys) ]

    pr x = case M.lookup x prmap of
             Nothing -> x
             Just x' -> x'

    prgate (And x y) = And (prval x) (prval y)
    prgate (Not x)   = Not (prval x)
    prgate (Con x)   = Con (prval x)

    prval (Var x) = Var (pr x)
    prval v       = v

dloops :: [Var] -> Circuit -> Circuit
dloops outs c
  | c == c'   = c
  | otherwise = dloops outs c'
 where
  c' = simp outs (dloop c)

-- main ------------------------------------------------------------------------

ex1 :: Circuit
ex1 = [ (4, And (Var 3) (Var 2))
      , (3, And (Var 2) (Var 1))
      , (2, And (Var 5) (Var 1))
      , (1, And (Var 2) (Var 3))
      ]
      
main = quickCheckWith stdArgs{ maxSuccess = 999999 } prop_Correct

-- aux ------------------------------------------------------------------------

mhead :: [a] -> Maybe a
mhead []    = Nothing
mhead (x:_) = Just x

-- prop -----------------------------------------------------------------------

data Circ
  = Circ
  { inps :: [Var]
  , vals :: [Bool]
  , outs :: [Var]
  , netl :: Circuit
  }
 deriving ( Eq, Ord, Show )

instance Arbitrary Circ where
  arbitrary =
    do i  <- choose (1,10)
       bs <- sequence [ arbitrary | _ <- [1..i] ]
       k  <- choose (i,100)
       let val  = liftM Var (choose (1,k))
           gate = frequency [ (10, liftM2 And val val)
                            , (2,  liftM Not val)
                            , (1,  liftM Con val)
                            ] 
       c  <- sequence [ do g <- gate; return (x,g) | x <- [i+1..k] ]
       js <- sequence [ choose (False,True) | _ <- [1..k] ]
       return (Circ [1..i] bs [j | (j,True) <- [1..k] `zip` js] c)

  shrink (Circ inps vals outs netl) =
    [ Circ inps vals (take i outs ++ drop (i+1) outs) netl
    | (_,i) <- outs `zip` [0..]
    ]
    ++
    [ Circ (take i inps ++ drop (i+1) inps) (take i vals ++ drop (i+1) vals) outs netl
    | (_,i) <- inps `zip` [0..]
    ]
    ++
    [ Circ inps vals outs (take i netl ++ drop (i+1) netl)
    | (_,i) <- netl `zip` [0..]
    ]
    ++
    [ Circ inps vals outs (take i netl ++ [(x,Con y)] ++ drop (i+1) netl)
    | ((x,And y z),i) <- netl `zip` [0..]
    ]
    ++
    [ Circ inps vals outs (take i netl ++ [(x,Con z)] ++ drop (i+1) netl)
    | ((x,And y z),i) <- netl `zip` [0..]
    ]
    ++
    [ Circ inps vals outs (take i netl ++ [(x,Con z)] ++ drop (i+1) netl)
    | ((x,Not z),i) <- netl `zip` [0..]
    ]
    ++
    [ Circ inps vals outs (map sub (take i netl) ++ map sub (drop (i+1) netl))
    | ((x,Con y),i) <- netl `zip` [0..]
    , let sub (z, And p q) = (z, And (subv p) (subv q))
          sub (z, Not p)   = (z, Not (subv p))
          sub (z, Con p)   = (z, Con (subv p))

          subv (Var z) | x == z = y
          subv v                = v
    ]

prop_Correct circ =
  let inp  = [ (i, Con (if b then TRUE else FALSE))
             | (i,b) <- inps circ `zip` vals circ
             ]
      out1 = sim3 (inp ++ netl circ)
      outs1 = [ (x,b) | x <- outs circ, Just b <- [M.lookup x out1] ]
   in not (null (M.toList out1)) ==>
        let c    = dloops (map fst outs1) (order (map fst outs1) (netl circ))
            out2 = sim3 (inp ++ c)
            outs2 = [ (x, M.lookup x out2) | (x,_) <- outs1 ]
         in whenFail (
              do print outs1
                 print outs2
            ) $ [ (x,Just b) | (x,b) <- outs1 ] == outs2
