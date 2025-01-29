module Main where

import qualified Data.Map as M
import Data.Map( Map )
import qualified Data.Set as S
import Data.Set( Set )
import Control.Monad
import Data.List( intersperse, sort, nub )
import Test.QuickCheck
import System.Process( system )
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Scientific as N

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
  | Action [Val] String -- non-empty list of Vals
 deriving ( Ord, Eq, Show )

fvs :: Gate -> Set Var
fvs (And xs)      = S.fromList ([ v | Var v <- xs ] ++ [ v | Not v <- xs ])
fvs (Action xs _) = fvs (And xs)

eval :: Map Var Bool -> Gate -> Maybe Bool
eval mp g = gate g
 where
  is b TRUE    = b
  is b FALSE   = not b
  is b (Var x) = M.lookup x mp == Just b
  is b (Not x) = M.lookup x mp == Just (not b)

  isBool TRUE  = True
  isBool FALSE = True
  isBool _     = False

  gate (And xs)
    | any (is False) xs = Just False
    | all (is True)  xs = Just True
    | otherwise         = Nothing

  gate (Action xs _)
    | all isBool xs = Just (all (is True) xs)
    | otherwise     = Nothing

-- Circuit --------------------------------------------------------------------------

data Circuit
  = Circuit
  { maxVar   :: Int
  , inputs   :: [Var]
  , rinputs  :: [Var]
  , netlist  :: [(Var,Gate)]
  , routputs :: [Val]
  , outputs  :: [Val]
  }
 deriving ( Eq, Ord )

printC :: Circuit -> IO ()
printC c = putStr $ unlines $
  [ "[" ++ concat (intersperse ", " [ shV i | i <- inputs c ]) ++ "]"
  , "{" ++ concat (intersperse ", " [ shV i | i <- rinputs c ]) ++ "} -->"
  , "(" ++ show (length (netlist c)) ++ " gates)"
  ]
  ++
  defs (S.fromList (inputs c)) (netlist c)
  ++
  [ "--> {" ++ concat (intersperse ", " [ showV (const True) x | x <- routputs c ]) ++ "}"
  , "    [" ++ concat (intersperse ", " [ showV (const True) x | x <- outputs c ]) ++ "]"
  ]
 where
  shV x = "v" ++ show x
 
  showV seen (Var x) = (if seen x then id else mark) (shV x)
  showV seen (Not x) = "~" ++ showV seen (Var x)
  showV _    FALSE   = "FALSE"
  showV _    TRUE    = "TRUE"
  
  showG seen (And [])      = "TRUE"
  showG seen (And xs)      = concat (intersperse " & " (map (showV seen) xs))
  showG seen (Action xs a) = a ++ " <== " ++ showG seen (And xs)

  defs seen [] = []
  defs seen ((x,g):c) =
    (shV x ++ " = " ++ showG (`S.member` seen) g ++ ";")
    : defs (S.insert x seen) c

  mark s = "\027[4m" ++ s ++ "\027[0m"

draw :: FilePath -> Circuit -> IO ()
draw name c =
  do writeFile (name ++ ".dot") $ unlines $
       [ "digraph circuit {"
       ] ++
       -- inputs
       [ "node[color=pink,style=filled]"
       ] ++
       [ show i ++ " [label=" ++ show ("in:" ++ show i) ++ "];"
       | i <- inputs c
       ] ++
       -- outputs
       [ "node[color=lightblue,style=filled]"
       ] ++
       [ "o" ++ show i ++ " [label=" ++ show ("out#" ++ show i) ++ "];"
       | (i,_) <- [1..] `zip` outputs c
       ] ++
       -- registers
       [ "node[color=yellow,shape=box,style=filled]"
       ] ++
       [ show i ++ " [label=" ++ show ("REG:" ++ show i) ++ "];"
       | (i,_) <- rinputs c `zip` routputs c
       ] ++
       -- gates
       [ "node[color=black,shape=box,style=" ++ show "" ++ "]"
       ] ++
       [ arrowV y (show x)
       | (x, And ys) <- netlist c
       , y <- ys
       ] ++
       [ show x ++ " [color=green,style=filled,label=" ++ show a ++ "];"
       | (x, Action _ a) <- netlist c
       ] ++
       [ arrowV y (show x)
       | (x, Action ys _) <- netlist c
       , y <- ys
       ] ++
       -- linking outputs
       [ arrowV x ("o" ++ show i)
       | (i,x) <- [1..] `zip` outputs c
       ] ++
       -- linking registers
       [ arrowV x (show i)
       | (i,x) <- rinputs c `zip` routputs c
       ] ++
       [ "}"
       ]
     system ("dot -Tpdf -O " ++ show (name ++ ".dot"))
     return ()
 where
  arrowV TRUE    n = "FALSE -> " ++ n ++ " [arrowhead=dot]"
  arrowV FALSE   n = "FALSE -> " ++ n
  arrowV (Var v) n = show v ++ " -> " ++ n
  arrowV (Not v) n = show v ++ " -> " ++ n ++ " [arrowhead=dot]"

-- parse -----------------------------------------------------------------------

parse :: FilePath -> IO Circuit
parse file =
  do s <- B.readFile file
     return $
       case decode s :: Maybe Value of
         Nothing ->
           error "parse error in JSON file"

         Just (Object d) ->
           case KM.lookup (K.fromString "nets") d of
             Just (Array nets) -> circuit (Circuit 0 [] [] [] [] []) (V.toList nets)
 where
  circuit c [] =
    c{ maxVar  = maxV c
     , netlist = [ (-x, And [Not x])
                 | (x,_) <- netlist c
                 , x<0
                 ]
                 ++ netlist c
     }

  circuit c (Object net:nets) =
    case typ of
      "AND" ->
        circuit c{ netlist = (idf, And args):netlist c } nets

      "OR" ->
        circuit c{ netlist = (-idf, And (map neg args)):netlist c } nets

      "FALSE" ->
        circuit c{ netlist = (idf, And [FALSE]):netlist c } nets

      "TRUE" ->
        circuit c{ netlist = (idf, And []):netlist c } nets

      "REG" ->
        case fin of
          [x] -> circuit c{ rinputs  = idf:rinputs c
                          , routputs = val x : routputs c
                          } nets

      "SIG" ->
        if null fin
          then circuit c{ inputs  = idf:inputs c } nets
          else circuit c{ outputs = Var idf:outputs c
                        , netlist = (idf,And args) : netlist c
                        } nets
          
      "SIGACTION" ->
        case fin of
          [x] -> circuit c{ outputs = Var idf:outputs c
                          , netlist = (idf,Action [val x] ("SGA:" ++ show idf)):netlist c
                          } nets

      "ACTION" ->
        circuit c{ outputs = Var idf:outputs c
                 , netlist = (idf,Action args ("ACT:" ++ show idf)):netlist c
                 } nets

      t -> error ("type " ++ show t)
   where
    idf  = num (net ~> "id")
    typ  = str (net ~> "type")
    fin  = list obj (net ~> "fanin")
    fout = list obj (net ~> "fanout")
    args = [ val o | o <- fin ]

  o ~> f = case KM.lookup (K.fromString f) o of
            Just x -> x

  obj (Object o)   = o
  list a (Array v) = map a (V.toList v)
  str (String t)   = T.unpack t
  num (Number n)   = round n :: Int
  bool (Bool b)    = b

  val o = (if bool (o ~> "polarity") then Var else Not) (num (o ~> "id"))

maxV :: Circuit -> Int
maxV c = maximum (1 : inputs c ++ [ abs i | (i,_) <- netlist c ])

-- three-valued simulation
sim3 :: Map Var Bool -> Circuit -> Map Var Bool
sim3 mp c = loop mp (netlist c)
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
order :: Circuit -> Circuit
order c = c{ netlist = fst (go S.empty S.empty roots) }
 where
  roots = [ i
          | x <- outputs c ++ routputs c
          , i <- case x of
                   Var i -> [i]
                   Not i -> [i]
                   _     -> []
          ]
 
  def = M.fromList (netlist c)

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
simp' :: Int -> Circuit -> Circuit
simp' 0 _     = error "simp loops"
simp' k c
  | c == c'   = c
  | otherwise = simp' (k-1) c'
 where
  c' = order . cse . bite . prop $ c

simp = simp' 999

-- perform constant propagation
prop :: Circuit -> Circuit
prop c = c{ netlist  = [ (x, pr g) | (x,g) <- netlist c ]
          , routputs = map look (routputs c)
          , outputs  = map look (outputs c)
          }
 where
  vmp = M.fromList (netlist c)

  pr (And ys)
    | isFalse ys' = And [FALSE]
    | otherwise   = And ys'
   where
    ys' = filter (/= TRUE) $ map look ys

    isFalse []          = False
    isFalse (x:xs)
      | x == FALSE      = True
      | neg x `elem` xs = True
      | otherwise       = isFalse xs
    
    look (Var x) =
      case M.lookup x vmp of
        Just (And [])               -> TRUE
        Just (And [y]) | y /= Not x -> y
        _                           -> Var x

    look (Not x) = neg (look (Var x))
    look a       = a

  pr (Action ys a) = Action (map look ys) a

  look (Var x) =
    case M.lookup x vmp of
      Just (And [])               -> TRUE
      Just (And [y]) | y /= Not x -> y
      _                           -> Var x

  look (Not x) = neg (look (Var x))
  look a       = a

-- take bites out of gate input lists
bite :: Circuit -> Circuit
bite c = c{ netlist = go M.empty (netlist c) }
 where
  go mp [] = []
  go mp ((x, And ys):c) =
    (x, And (bt [] ys)) : go (M.insert x (foldr S.union (S.fromList ys) yss) mp) c
   where
    yss = [ zs
          | Var y <- ys
          , Just zs <- [M.lookup y mp]
          ]

    bt ys []         = ys
    bt ys (Var x:xs) = bt (Var x : ok ys) (ok xs)
     where
      ok = case M.lookup x mp of
             Just s  -> filter (not . (`S.member` s))
             Nothing -> id
    bt ys (x:xs)     = bt (x:ys) xs
  go mp ((x,g):c) = (x,g):go mp c

-- common subexpression elimination
cse :: Circuit -> Circuit
cse c = c{ netlist = [ case M.lookup g gmp of
                         Just x' | x' /= x -> (x, And [Var x'])
                         _                 -> (x, g)
                     | (x,g) <- c'
                     ] }
 where
  c'  = [ (x, case g of
                And ys -> And (nub (sort ys))
                _      -> g)
        | (x,g) <- netlist c
        ]
  gmp = M.fromList [ (And ys, x) | (x, And (ys@(_:_:_))) <- c' ]

-- Combinational Loops ---------------------------------------------------------------

-- find and remove one combinational loop
dloop :: Circuit -> Circuit
dloop c0 =
  case find S.empty (netlist c0) of
    Nothing -> c0
    Just y  -> c' 
     where
      c' = c0{ maxVar = maxV c', netlist = expand y (netlist c0) }
 where
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

    prmap = M.fromList ((S.toList zs) `zip` [maxVar c0 + 1 .. ])

    pr x = case M.lookup x prmap of
             Nothing -> x
             Just x' -> x'

    prgate (And xs)      = And (map prval xs)
    prgate (Action xs a) = Action (map prval xs) a

    prval (Var x) = Var (pr x)
    prval (Not x) = Not (pr x)
    prval v       = v

-- find and remove combinational loops, and simplify, until all are gone
dloops :: Circuit -> Circuit
dloops c
  | c == c'   = c
  | otherwise = dloops c'
 where
  c' = simp (dloop c)

-- dloops, but with nice output
dloopsIO :: Circuit -> IO Circuit
dloopsIO c
  | c == cX =
    do draw "final" c
       return c

  | otherwise =
    do putStrLn "==EXPAND-LOOP==>"
       printC cX
       putStrLn "==SIMPLIFY==>"
       let cS = simp cX
       printC cS
       dloopsIO cS
 where
  cX = dloop c

-- main ------------------------------------------------------------------------

main =
  do --c <- parse "hiphop/emit-if2/emit-if2.net.json"
     --c <- parse "hiphop/abro/abro.net.json"
     c <- parse "hiphop/causal2/causal2-.net.json"
     --c <- parse "hiphop/p15/p15.net.json"
     --c <- parse "hiphop/p13/p13.net.json"
     draw "original" c
     printC c
     putStrLn "==ORDER==>"
     let cO = order c
     printC cO
     putStrLn "==SIMP==>"
     let cS = simp cO
     printC cS
     draw "simplified" cS
     dloopsIO cS
     return ()

-- aux ------------------------------------------------------------------------

mhead :: [a] -> Maybe a
mhead []    = Nothing
mhead (x:_) = Just x

-- prop -----------------------------------------------------------------------

{-
main2 = quickCheckWith stdArgs{ maxSuccess = 999999 } prop_Correct

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
                 draw "final" (inps circ) (outs circ) (netl circ)
            ) $ [ (x,Just b) | (x,b) <- outs1 ] == outs2
-}
