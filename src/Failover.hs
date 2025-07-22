
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
-- cb is the crash branch 
-- crs is the crashed roles
-- addlbls is for renaming with unique labels
fo' :: Int ->[Int] -> Int -> G a -> G a -> G a
fo' m is j t (GComm p q a cs)
  | reliable p = (GComm p q a cs')
  | otherwise  = (GComm p q a (cs' ++ [cb]))
  where
    ts  = map (\(Choice l p' _) -> 
                extend t (GComm p q a [(Choice l p' GEnd)])) 
               cs
    cs' = zipWith3 (\(Choice l p g) n t -> 
                     Choice l p (fo' m (is ++ [n]) (j + 1) t g)) 
                    cs [0..] ts
    cb  = Choice CrashLab BUnit (addlbls m p is j (extend (trim p t) (GComm p q a cs)))
    -- for nested failover use the following in place of the above line:
    -- cb  = Choice CrashLab BUnit (fo' m (is ++ [length cs]) (j + 1) GEnd (addlbls m p is j (extend (trim p t) (GComm p q a cs))))
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

addlbls :: Int -> Role -> [Int] -> Int -> G a -> G a
addlbls m crs is j t 
  | null is   = addlbls' m crs ("_" ++ show j) t
  | otherwise = addlbls' m crs ("_" ++ show j ++ "_" ++ concatMap show is) t

addlbls' :: Int -> Role -> String -> G a -> G a
addlbls' m crs s (GComm p q a cs) 
  | m == 2   = GComm (addlbl s p) (addlbl s q) a (map (\(Choice l p g) -> Choice l p (addlbls' m crs s g)) cs)
  | p == crs = GComm (addlbl s p) q a (map (\(Choice l p g) -> Choice l p (addlbls' m crs s g)) cs)
  | q == crs = GComm p (addlbl s q) a (map (\(Choice l p g) -> Choice l p (addlbls' m crs s g)) cs)
  | otherwise = GComm p q a (map (\(Choice l p g) -> Choice l p (addlbls' m crs s g)) cs)
addlbls' m crs s (GRec a g) = GRec a (addlbls' m crs s g)
addlbls' m crs _ e = e

addlbl :: String -> Role -> Role 
addlbl s (MkRole i n r) = MkRole (i + 100000) (n ++ s) R









-- refactor scribble protocol with failover pattern and generate effpi code end to end
-- cls is the crash branches acompanied by their locations. it is a list of [(Effpi, Role, [Label], String)] namely,
-- Effpi is the effpi code, 
-- Role is the role that detects the crash,
-- [Label] is the path of messages to the crashed role from the perspective of the role that receives the crash,
-- String is the label of the crash
foend2end :: G () -> String
foend2end g = case e of
          Ok effpi -> toScala (Just cls) effpi
          Err err -> error $ "Failed to generate Effpi: " ++ err
  where
    cls = gcs g
    g' = rmc g
    e = effpiG "example" g'

gcs :: G a -> [(Effpi, Role, [Label], String)]
gcs g = gcs' g

gcs' :: G a -> [(Effpi, Role, [Label], String)]
gcs' start = map (\(c, r, p, l) -> (f c, r, p, l)) cls
  where
    cls = trace (concatMap (\(g, r, p, l) -> showG g) (gcs'' Map.empty start)) (gcs'' Map.empty start)
    f :: G a -> Effpi
    f g' = case effpiG "example" (tounitG g') of
            Ok effpi -> effpi
            Err err -> error $ "Failed to generate Effpi: " ++ err

-- rlm is the role label map
-- hasc is a boolean meaning "has crash" ie there exists a crash branch in the choice
-- cb is the crash branch
-- cbp is the crash branch path
gcs'' :: RoleLabelMap -> G a -> [(G a, Role, [Label], String)]
gcs'' rlm (GComm p q _ cs)
  | hasc      = (cb, q, cbp, show l) : cls
  | otherwise = cls
    where
      hasc = or ([isCrashLabel l' | Choice l' _ _ <- cs])
      (Choice _ _ cb) = head [c | c <- cs, isCrashLabel (label c)]
      l = head [l' | Choice l' _ _ <- cs, isCrashLabel l']
      cbp = reverse (lookupRoleLabel q rlm)
       -- merge any duplicates
      cls = concatMap (\(Choice l' _ g) -> gcs'' (addLabelToRoles l' [p, q] rlm) g) cs
gcs'' rlm (GRec _ g) = gcs'' rlm g
gcs'' _ _ = []

mergeG :: [G a] -> G a
mergeG []  = error "unexpected empty list"
mergeG [g] = g
mergeG ((GComm p q a cs) : (GComm p' q' a' cs') : gs) 
  | p == p' && q == q' = mergeG ((GComm p q a (cs ++ cs')) : gs)
  | otherwise          = error "unexpected case in mergeG"

-- remove crash labels
rmc :: G a -> G a
rmc (GComm p q a cs) = GComm p q a [Choice l p' (rmc g) | (Choice l p' g) <- cs, not (isCrashLabel l)]
rmc (GRec a g) = GRec a (rmc g)
rmc e = e

showCls :: Maybe [(Effpi, Role, [Label], String)] -> String
showCls Nothing = "\ncls is Nothing"
showCls (Just []) = "\ncls is Just []"
showCls (Just [(_, r, p, _)]) = "\n" ++ show r ++ show p ++ "\nend"
showCls (Just ((_, r, p, _) : cls)) = "\n" ++ show r ++ show p ++ showCls (Just cls)

