module Utils where

import Core
import BaseUtils ( firstUpper )

import Data.List ( sort,sortBy,nub )

import System.Clock
  ( Clock (Monotonic),
    diffTimeSpec,
    getTime,
    toNanoSecs,
  )

reliable :: Role -> Bool
reliable (MkRole _ _ R) = True
reliable _ = False

participants :: G a -> [Role]
participants = sort . nub . rs where
  rs (GComm p q _ ks) = p : q : concatMap (rs . cont) ks
  rs (GRec _ k) = rs k
  rs _ = []

isCrashLabel :: Label -> Bool
isCrashLabel CrashLab = True
isCrashLabel _ = False

isCrashBranch :: Choice a b -> Bool
isCrashBranch = isCrashLabel . label

labelsAndPayloads :: G a -> [(Label, B)]
labelsAndPayloads = sortBy g . nub . lAPs where
  g x y = compare (fst x) (fst y)
  f k | isCrashBranch k = lAPs (cont k)
      | otherwise = (label k, payload k) : lAPs (cont k)

  lAPs (GComm _ _ _ ks) = concatMap f ks
  lAPs (GRec _ k) = lAPs k
  lAPs _ = []

isCustomType :: B -> Bool
isCustomType (BType _ _) = True
isCustomType _ = False

payloads :: G a -> [B]
payloads = sort . nub . payloads' where
  payloads' (GComm _ _ _ ks) =
    concatMap (\k -> payload k : payloads' (cont k)) ks
  payloads' (GRec _ k) = payloads' k
  payloads' _ = []

toCClass :: Label -> Label
toCClass = mapLabel firstUpper where
  mapLabel :: (String -> String) -> Label -> Label
  mapLabel f (MkLabel i s) = MkLabel i (f s)
  mapLabel _ CrashLab = CrashLab

rmCrashLabs :: [Label] -> [Label]
rmCrashLabs = filter (not . isCrashLabel)

setK :: Choice ty a -> ty b -> Choice ty b
setK x k' = x {cont = k'}

contM' :: Monad m => (ty a -> m (ty a)) -> Choice ty a -> m (Choice ty a)
contM' f x = fmap (setK x) (f (cont x))

showG :: G a -> String
showG g = signature ++ protocol ++ "}"
  where
    rs = showRoles (roles g)
    signature = "global protocol someProtocol(" ++ rs ++ ") {\n"
    protocol = showG' g 1

showG' :: G a -> Int -> String
showG' (GComm r1 r2 _ cs) i
  | length cs == 1 = ppsc
  | otherwise      = indentBy i ++ "choice at " ++ show r1 ++ " {\n" ++ ppc ++ indentBy i ++ "}" ++ concatMap (\ppc' -> " or {\n" ++ ppc' ++ indentBy i ++ "}") ppcs' ++ "\n"
  where
    ppcs@(ppc : ppcs') = map (\(Choice l p c) -> indentBy (i + 1) ++ showChoiceWithoutContinuation (Choice l p c) ++ showG' c (i + 1)) cs
    (ppsc : _) = map (\(Choice l p c) -> indentBy i ++ showChoiceWithoutContinuation (Choice l p c) ++ showG' c i) cs
    showChoiceWithoutContinuation (Choice l BUnit _) = show l ++ " from " ++ show r1 ++ " to " ++ show r2 ++ ";\n"
    showChoiceWithoutContinuation (Choice l p _) = show l ++ "(" ++ show p ++ ") from " ++ show r1 ++ " to " ++ show r2 ++ ";\n"
showG' (GRec _ g) i = indentBy i ++ "rec X {\n" ++ showG' g (i + 1) ++ indentBy i ++ "}\n"
showG' (GVar _ _) i = indentBy i ++ "continue X;\n"
showG' GEnd _ = ""

indentBy :: Int -> String
indentBy 0 = ""
indentBy i = "    " ++ indentBy (i - 1)


roles :: G a -> [Role]
roles g = nub (roles' g)
  where
    roles' (GComm r1 r2 _ cs) = [r1, r2] ++ concatMap (\(Choice l _ g) -> roles' g) cs
    roles' (GRec _ g) = roles' g
    roles' _ = []

showRoles :: [Role] -> String
showRoles [] = ""
showRoles [r@(MkRole _ _ R )] = "reliable role " ++ show r
showRoles [r] = "role " ++ show r
showRoles (r@(MkRole _ _ R ) : rs) = "reliable role " ++ show r ++ ", " ++ showRoles rs
showRoles (r : rs) = "role " ++ show r ++ ", " ++ showRoles rs

tounitG :: G a -> G ()
tounitG (GComm p q a cs) = GComm p q () [Choice l p' (tounitG g) | (Choice l p' g) <- cs]
tounitG (GRec a g) = GRec () (tounitG g)
tounitG (GVar n a) = GVar n ()
tounitG GEnd = GEnd