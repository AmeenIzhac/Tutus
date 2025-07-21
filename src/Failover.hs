
module Failover where

import Control.Exception (evaluate)
import Core
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set
import DataStructs
import Debug.Trace
import Projection (projAllRoles)
import Utils
import ErrOr ( ErrOr(Ok, Err) )

import EffpiIR
    ( Effpi(pname, ports, cases, recvs, types, mainf),
      Effpi(..) )
import Scala (toScala)
import Effpi (effpiG)
import Control.Monad (foldM)
import DataStructs

fo :: Int -> G a -> G a
fo m g = fo' m [] 0 GEnd g 

-- tcs are the traces
fo' :: Int ->[Int] -> Int -> G a -> G a -> G a
fo' m is j t (GComm p q a cs) 
  | reliable p = (GComm p q a cs')
  | otherwise  = (GComm p q a (cs' ++ [cr]))
  where
    ts  = map (\(Choice l p' _) -> 
                extend t (GComm p q a [(Choice l p' GEnd)])) 
               cs
    cs' = zipWith3 (\(Choice l p g) n t -> 
                     Choice l p (fo' m (is ++ [n]) (j + 1) t g)) 
                    cs [0..] ts
    cr  = Choice CrashLab BUnit (newp m p is j (extend (trim p t) (GComm p q a cs)))
    -- cr    = Choice CrashLab BUnit (fo' m (is ++ [length cs]) (j + 1) GEnd (newp m p is j (extend (trim p t) (GComm p q a cs))))
fo' m is j t (GRec a g) = GRec a (fo' m is j (extend t (GRec a g)) g)
fo' m is j t (GVar n a) = GVar n a
fo' m is j t GEnd = GEnd

trim :: Role -> G a -> G a
trim r (GComm p q a [Choice l p' g]) 
  | q == r    = GComm p q a [Choice l p' (trim r g)]
  | otherwise = trim r g
trim r (GRec a g) = GRec a g
trim _ e = e

extend :: G a -> G a -> G a
extend (GComm p q a [c]) g = GComm p q a [c']
  where 
    Choice l p' g' = c
    c' = Choice l p' (extend g' g)
extend GEnd g = g
extend (GVar n a) g = error "Error 3"
extend (GRec a g) g' = GRec a g

newp :: Int -> Role -> [Int] -> Int -> G a -> G a
newp m crd is j t 
  | null is   = newp' m crd ("_" ++ show j) t
  | otherwise = newp' m crd ("_" ++ show j ++ "_" ++ concatMap show is) t

newp' :: Int -> Role -> String -> G a -> G a
newp' m crd s (GComm p q a cs) 
  | m == 2   = GComm (newr s p) (newr s q) a (map (\(Choice l p g) -> Choice l p (newp' m crd s g)) cs)
  | p == crd = GComm (newr s p) q a (map (\(Choice l p g) -> Choice l p (newp' m crd s g)) cs)
  | q == crd = GComm p (newr s q) a (map (\(Choice l p g) -> Choice l p (newp' m crd s g)) cs)
  | otherwise = GComm p q a (map (\(Choice l p g) -> Choice l p (newp' m crd s g)) cs)
newp' m crd s (GRec a g) = GRec a (newp' m crd s g)
newp' m crd _ e = e

newr :: String -> Role -> Role 
newr s (MkRole i n r) = MkRole (i + 100000) (n ++ s) R
















foe :: G () -> String
foe g = case e of
          Ok effpi -> toScala (Just crpls) effpi
          Err err -> error $ "Failed to generate Effpi: " ++ err
  where
    crpls = gcs g
    g' = rmc g
    e = effpiG "example" g'


gcs :: G a -> [(Effpi, Role, [Label], String)]
gcs g = gcs' g


gcs' :: G a -> [(Effpi, Role, [Label], String)]
gcs' start = map (\(c, r, p, l) -> (doit c, r, p, l)) crpls
  where
    crpls = trace (concatMap (\(g, r, p, l) -> showG g) (gcs'' Map.empty start)) (gcs'' Map.empty start)
    doit :: G a -> Effpi
    doit thing = case effpiG "example" (tounitG thing) of
            Ok effpi -> effpi
            Err err -> error $ "Failed to generate Effpi: " ++ err

gcs'' :: RoleLabelMap -> G a -> [(G a, Role, [Label], String)]
gcs'' rlm (GComm p q _ cs)
    | hasc      = (grmc, q, qpath, show l) : crpls
    | otherwise = crpls
    where
        hasc = or ([isCrashLabel l' | Choice l' _ _ <- cs])
        (Choice _ _ grmc) = head [c | c <- cs, isCrashLabel (label c)]
        l = head [l' | Choice l' _ _ <- cs, isCrashLabel l']
        qpath = reverse (lookupRoleLabel q rlm)
        crpls = concatMap (\(Choice l' _ g) -> gcs'' (addLabelToRoles l' [p, q] rlm) g) cs -- Merge any duplicates rather than filter them with filterc
gcs'' rlm (GRec _ g) = gcs'' rlm g
gcs'' _ _ = []

mergeG :: [G a] -> G a
mergeG []  = error "Error 1"
mergeG [g] = g
mergeG ((GComm p q a cs) : (GComm p' q' a' cs') : gs) 
  | p == p' && q == q' = mergeG ((GComm p q a (cs ++ cs')) : gs)
  | otherwise          = error "Error 2"

mergeCrpls ::[(G a, Role, [Label], String)] -> [(G a, Role, [Label], String)]
mergeCrpls [] = []
mergeCrpls [x] = [x]
mergeCrpls (x:xs) = (go (x : xs)) : mergeCrpls xs
  where
    go [(c, r, p, l)] = (c, r, p, l)
    go ((c, r, p, l) : (c', r', p', l') : rest)
      | r == r' && p == p' && l == l' = go ((mergeG [c, c'], r, p, l) : rest)
      | otherwise                     =  go ((c, r, p, l) : rest)




filterc :: [(Effpi, Role, [Label], String)] -> [(Effpi, Role, [Label], String)]
filterc xs = go xs Set.empty
  where
    go [] _ = []
    go ((g, r, l, s):rest) seen
      | s `Set.member` seen = go rest seen
      | otherwise           = (g, r, l, s) : go rest (Set.insert s seen)

-- remove crash labels
rmc :: G a -> G a
rmc (GComm p q a cs) = GComm p q a [Choice l p' (rmc g) | (Choice l p' g) <- cs, not (isCrashLabel l)]
rmc (GRec a g) = GRec a (rmc g)
rmc e = e

showCrpls :: Maybe [(Effpi, Role, [Label], String)] -> String
showCrpls Nothing = "\ncrpls is nothing"
showCrpls (Just []) = "\ncrpls is Just []"
showCrpls (Just [(_, r, p, _)]) = "\n" ++ show r ++ show p ++ "\ndone"
showCrpls (Just ((_, r, p, _) : crpls)) = "\n" ++ show r ++ show p ++ showCrpls (Just crpls)

