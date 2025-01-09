module Main where

import qualified Data.Map as M
import Data.Map( Map )
import qualified Data.Set as S
import Data.Set( Set )
import Control.Monad
import Data.List( intersperse )
import Test.QuickCheck

-- Var & Val ------------------------------------------------------------------------

type Var
  = Int

data Val
  = Var Var
  | Not Var
  | TRUE
  | FALSE
 deriving ( Ord, Eq, Show )

neg :: Val -> Val
neg (Var x) = Not x
neg (Not x) = Var x
neg FALSE   = TRUE
neg TRUE    = FALSE

-- Gate -----------------------------------------------------------------------------

data Gate
  = And [Val]
 deriving ( Ord, Eq, Show )

fvs :: Gate -> Set Var
fvs (And xs) = S.fromList ([ v | Var v <- xs ] ++ [ v | Not v <- xs ])

eval :: Map Var Bool -> Gate -> Maybe Bool
eval mp g = gate g
 where
  is b TRUE    = b
  is b FALSE   = not b
  is b (Var x) = M.lookup x mp == Just b
  is b (Not x) = M.lookup x mp == Just (not b)

  gate (And xs)
    | any (is False) xs = Just False
    | all (is True)  xs = Just True
    | otherwise         = Nothing

-- Circuit --------------------------------------------------------------------------

type Circuit = [(Var,Gate)]

printC :: [Var] -> [Var] -> Circuit -> IO ()
printC inps outs c = putStr $ unlines $
  [ "[" ++ concat (intersperse ", " [ shV i | i <- inps ]) ++ "] -->"
  , "(" ++ show (length c) ++ " gates)"
  ]
  ++
  defs (S.fromList inps) c
  ++
  [ "--> [" ++ concat (intersperse ", " [ shV i | i <- outs ]) ++ "]"
  ]
 where
  shV x = "v" ++ show x
 
  showV seen (Var x) = (if x `S.member` seen then id else mark) (shV x)
  showV seen (Not x) = "~" ++ showV seen (Var x)
  showV _    FALSE   = "FALSE"
  showV _    TRUE    = "TRUE"
  
  showG seen (And xs) = concat (intersperse " & " (map (showV seen) xs))

  defs seen [] = []
  defs seen ((x,g):c) =
    (shV x ++ " = " ++ showG seen g ++ ";")
    : defs (S.insert x seen) c

  mark s = "\027[4m" ++ s ++ "\027[0m"

-- three-valued simulation
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

-- reorder so that definitions come before uses (if possible), and remove unused definitions
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

-- simplify a circuit by repeatedly doing constant propagation and garbage collection
simp' :: Int -> [Var] -> Circuit -> Circuit
simp' 0 _ _ = error "simp loops"
simp' k outs c
  | c == c'   = c
  | otherwise = simp' (k-1) outs c'
 where
  c' = order outs (prop c)

simp = simp' 999

-- perform constant propagation
prop :: Circuit -> Circuit
prop c = go M.empty M.empty c
 where
  go vmp gmp [] =
    []

  go vmp gmp ((x,g):c) =
    case gate vmp g of
      And []               -> go1 TRUE
      And [y] | y /= Not x -> go1 y
      g'                   ->
        case M.lookup g' gmp of
          Just y  -> go1 (Var y)
          Nothing -> (x,g') : go vmp (M.insert g' x gmp) c
   where
    go1 v = (x,And [v]) : go (M.insert x v vmp) gmp c

  gate vmp (And xs) = And (go S.empty (map (`look` vmp) xs))
   where
    go zs []                = S.toList zs
    go zs (FALSE : _)       = [FALSE]
    go zs (TRUE : xs)       = go zs xs
    go zs (x : xs)
      | x     `S.member` zs = go zs xs
      | neg x `S.member` zs = [FALSE]
      | otherwise           = go (S.insert x zs) xs

  look (Var x) vmp =
    case M.lookup x vmp of
      Nothing -> Var x
      Just v  -> v
  look (Not x) vmp =
    case M.lookup x vmp of
      Nothing -> Not x
      Just v  -> neg v
  look v vmp       = v

-- Combinational Loops ---------------------------------------------------------------

-- find and remove one combinational loop
dloop :: Circuit -> Circuit
dloop c =
  case find S.empty c of
    Nothing -> c
    Just y  -> expand y c
 where
  -- what variables are in use in the circuit
  ys = S.fromList ([ y | (x,g) <- c, y <- x : S.toList (fvs g) ])

  -- find the first variable that is defined *after* its first use
  find used [] =
    Nothing

  find used ((x,g):c)
    | x `S.member` used = Just x
    | otherwise         = find (used `S.union` fvs g) c

  -- expand the path from y's first use to y's definition
  expand y ((x,g):c)
    | y `S.member` fvs g = copy y ((x,g):c)
    | otherwise          = (x,g) : expand y c

  copy y c =
    [ (pr y, And [FALSE]) ]
    ++
    [ (pr z, prgate g)
    | (z,g) <- c1
    ]
    ++
    [ (y, prgate g) ]
    ++
    c1
    ++
    c2
   where
    c1       = takeWhile ((/=y).fst) c
    (_,g):c2 = dropWhile ((/=y).fst) c
    zs       = S.fromList (y : [ z | (z,_) <- c1 ])

    prmap = M.fromList (primes (S.toList zs) ys)
     where
      primes []     ys = []
      primes (z:zs) ys = (z,z') : primes zs (S.insert z' ys)
       where
        z':_ = [ z' | z' <- [z..], not (z' `S.member` ys) ]

    pr x = case M.lookup x prmap of
             Nothing -> x
             Just x' -> x'

    prgate (And xs) = And (map prval xs)

    prval (Var x) = Var (pr x)
    prval (Not x) = Not (pr x)
    prval v       = v

-- find and remove combinational loops, and simplify, until all are gone
dloops :: [Var] -> Circuit -> Circuit
dloops outs c
  | c == c'   = c
  | otherwise = dloops outs c'
 where
  c' = simp outs (dloop c)

-- dloops, but with nice output
dloopsIO :: [Var] -> [Var] -> Circuit -> IO Circuit
dloopsIO inps outs c
  | c == c'   = return c
  | otherwise =
    do putStrLn "==EXPAND-LOOP==>"
       printC inps outs c'
       putStrLn "==SIMPLIFY==>"
       let c2 = simp outs c'
       printC inps outs c2
       dloopsIO inps outs c2
 where
  c' = dloop c

-- main ------------------------------------------------------------------------

{-
get -->
set -->       --> err
  x -->       --> top
  s -->       --> s'
ful -->       --> ful'
-}

{-
buf :: Circuit
buf 



main :: IO ()
main =
  do printC inps outs buf
     putStrLn " ==> "
     printC inps outs (dloops outs buf)
-}

{-
get -->
set -->       --> err
  x -->       --> top
  s -->       --> s'
ful -->       --> ful'
-}


{-
x -->    --> y
c -->
a -->

f(x) = ~x    10 -->f--> 11
g(x) = x&a   20 -->g--> 21

c=1: x-->10, 11-->20, 21-->4
c=0: x-->20, 21-->10, 11-->4
-}

fg :: Circuit
fg = [ (11, And [Not 10])
     , (21, And [Var 20, Var 3])
     ]
     ++ iff 10 (Var 2) (Var 1)  (Var 21) 100
     ++ iff 20 (Var 2) (Var 11) (Var 1)  200
     ++ iff  4 (Var 2) (Var 21) (Var 11) 300
 where
  iff s c a b k =
    [ (s, And [Not k, Not (k+1)])
    , (k, And [c,a])
    , (k+1, And [neg c, b])
    ]

ins = [1,2,3]
ots = [4]

main1 :: IO ()
main1 =
  do printC ins ots fg
     putStrLn "==SORT==>"
     let c' = order ots fg
     printC ins ots c'
     putStrLn "==SIMP==>"
     let c2 = simp ots fg
     printC ins ots c2
     dloopsIO ins ots c2
     return ()

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
       let val  = oneof [liftM Var (choose (1,k)), liftM Not (choose (1,k))]
           gate = liftM And (listOf val) 
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
    [ Circ inps vals outs (take i netl ++ [(x,And (take j xs ++ drop (j+1) xs))] ++ drop (i+1) netl)
    | ((x,And xs),i) <- netl `zip` [0..]
    , (_,j) <- xs `zip` [0..]
    ]
    ++
    [ Circ inps vals outs (map sub (take i netl) ++ map sub (drop (i+1) netl))
    | ((x,And [y]),i) <- netl `zip` [0..]
    , let sub (z, And ps) = (z, And (map subv ps))

          subv (Var z) | x == z = y
          subv (Not z) | x == z = neg y
          subv v                = v
    ]

prop_Correct circ =
  let inp  = [ (i, And [if b then TRUE else FALSE])
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
