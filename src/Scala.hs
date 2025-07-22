-- module Scala ( toScala ) where

-- import BaseUtils (parens, brackets, pfxStr, inc, prefix, spacer, firstLower)
-- import Core
-- import Utils (isCrashLabel)
-- import EffpiIR
--     ( BaseName,
--       LambdaSeed,
--       Prefix,
--       Effpi(pname, ports, cases, recvs, types, mainf),
--       MainFn(..),
--       RoleTy(..),
--       TyDecl(..),
--       TyBody(..),
--       RecVar(..),
--       ChanUB(..),
--       ChanMap(..),
--       CClass(..) )

-- import Numeric.Natural (Natural)
-- import Data.List (intercalate, sort, nub)
-- import Data.Maybe (mapMaybe)

-- -- [CodeGen] ------------------------------------------------------------------
-- -- Pretty printing functions: EffpiIR -> Scala

-- interbar :: [String] -> String
-- interbar = intercalate " | "

-- intercr :: [String] -> String
-- intercr = intercalate "\n"

-- ppPackage :: String -> String
-- ppPackage n = "package " ++ n

-- ppImports :: [String] -> String
-- ppImports xs = intercr (map ("import " ++) xs)

-- ppCClasses :: [CClass] -> String
-- ppCClasses xs = intercr (map f xs) where
--   f (MkCClass n arg) = "case class " ++ n ++ parens (maybe "" ("x : " ++) arg)

-- {-
-- sealed abstract class RecT0[A]() extends RecVar[A]("RecT0")
-- case object RecT0 extends RecT0[Unit]
-- -}
-- ppRecDefs :: [RecVar] -> String
-- ppRecDefs xs = intercr (map f xs) where
--   f (MkRecVar _ t) = concat
--     ["sealed abstract class ", t, "[A]() extends RecVar[A](\"", t, "\")\n",
--      "case object ", t, " extends ", t, "[Unit]"]

-- ppChUBTys :: [ChanUB] -> String
-- ppChUBTys xs = intercalate ", " (map f (sort xs)) where
--   f (InChan i ls) =
--     "C" ++ show i ++ " <: InChannel[" ++ interbar (map show ls) ++ "]"
--   f (OutChan i ls) =
--     "C" ++ show i ++ " <: OutChannel[" ++ interbar (map show ls) ++ "]"

-- ppChUBArgs :: [ChanUB] -> String
-- ppChUBArgs xs = intercalate ", " (map f (sort xs)) where
--   f (InChan i ls) =
--     "c" ++ show i ++ " : InChannel[" ++ interbar (map show ls) ++ "]"
--   f (OutChan i ls) =
--     "c" ++ show i ++ " : OutChannel[" ++ interbar (map show ls) ++ "]"

-- ppChUBTyAssn :: [ChanUB] -> String
-- ppChUBTyAssn xs = intercalate ", " (map f (sort xs)) where
--   f (InChan i _) = "c" ++ show i ++ ".type"
--   f (OutChan i _) = "c" ++ show i ++ ".type"

-- ppTyBody :: BaseName -> LambdaSeed -> TyBody -> String
-- ppTyBody r i (Out (OutChan n _) l _ k) = concat
--   ["Out", brackets ("C" ++ show n ++ ", " ++ show l), " >>: ",
--    ppTyBody r i k]
-- ppTyBody r i (In (InChan n _) l k Nothing) = "In" ++ brackets inner
--   where
--     lamVar = "x" ++ show i ++ " : " ++ show l
--     inner = concat
--       ["C", show n, ", ", show l, ", ", parens lamVar, " => ",
--        ppTyBody r (i+1) k]
-- ppTyBody r i (In (InChan n _) l k (Just ck)) = "InErr" ++ brackets inner where
--   lamVar = "x" ++ show i ++ " : " ++ show l
--   inner = concat
--     ["C", show n, ", ", show l, ", ", parens lamVar, " => ",
--       ppTyBody r (i+1) k, ", (err : Throwable) => ", ppTyBody r (i+1) ck]
-- ppTyBody r i (Sel (OutChan n _) ks) =
--   parens (interbar (map (parens . f) ks)) where
--     f (l, _, k) =
--       "Out[C" ++ show n ++ ", " ++ show l ++ "] >>: " ++ ppTyBody r i k
-- ppTyBody r i (Bra (InChan n _) k ls cs Nothing) = "In" ++ brackets inner
--   where
--     lamVar = "x" ++ show i ++ " : " ++ interbar (map show ls)
--     args = "x" ++ show i ++ ".type" ++ if null cs then "" else
--       ", " ++ intercalate ", " (map f cs)
--     f (InChan j _) = "C" ++ show j
--     f (OutChan j _) = "C" ++ show j
--     inner = concat
--       ["C", show n, ", ", interbar (map show ls), ", ",
--        parens lamVar, " => ", r, show k, brackets args]
-- ppTyBody r i (Bra (InChan n _) k ls cs (Just ck)) = "InErr" ++ brackets inner
--   where
--     lamVar = "x" ++ show i ++ " : " ++ interbar (map show ls)
--     args = "x" ++ show i ++ ".type" ++ if null cs then "" else
--       ", " ++ intercalate ", " (map f cs)
--     f (InChan j _) = "C" ++ show j
--     f (OutChan j _) = "C" ++ show j
--     inner = concat
--       ["C", show n, ", ", interbar (map show ls), ", ",
--        parens lamVar, " => ", r, show k, brackets args,
--        ", (err : Throwable) => ", ppTyBody r (i+1) ck]
-- ppTyBody r i (Rec (MkRecVar _ t) k) =
--   "Rec" ++ brackets (t ++ ", " ++ ppTyBody r i k)
-- ppTyBody _ _ (Var (MkRecVar _ t)) = "Loop" ++ brackets t
-- ppTyBody _ _ End = "PNil"
-- ppTyBody _ _ s = error ("\nUnexpected case in ppTyBody: " ++ show s)

-- ppPayloadB :: B -> String
-- ppPayloadB BInt = "42"
-- ppPayloadB BReal = "42.0"
-- ppPayloadB BString = "\"\""
-- ppPayloadB BUnit = ""
-- ppPayloadB BBool = "true"
-- ppPayloadB (BType _ ty) = "new " ++ ty ++ "()"

-- ppRecVar :: RecVar -> String
-- ppRecVar (MkRecVar _ s) = s

-- ppPrintLn :: Prefix -> String -> String
-- ppPrintLn pfx msg = prefix pfx $
--   "println" ++ parens ("s\"-- " ++ msg ++ "\"") ++ "\n"

-- ppPrintLnPreSend :: Prefix -> BaseName -> Label -> Natural -> String
-- ppPrintLnPreSend pfx n l c = ppPrintLn pfx $
--   concat [n, " sending ", show l, " on c", show c, " ($c", show c, ")"]

-- ppPrintLnSend :: Prefix -> BaseName -> Label -> Natural -> String
-- ppPrintLnSend pfx n l c = ppPrintLn pfx $
--   concat [n, " sent ", show l, " on c", show c, " ($c", show c, ")"]

-- ppPrintLnPreRecv :: Prefix -> BaseName -> Either Label [Label] -> Natural -> String
-- ppPrintLnPreRecv pfx n (Left l) c = ppPrintLn pfx $
--   concat [n, " expecting ", show l, " on c", show c, " ($c", show c, ")"]
-- ppPrintLnPreRecv pfx n (Right ls) c = ppPrintLn pfx $
--   concat [n, " expecting ", parens (interbar (map show ls)), " on c", show c, " ($c", show c, ")"]

-- ppPrintLnRecv :: Prefix -> BaseName -> Either Label [Label] -> Natural -> String
-- ppPrintLnRecv pfx n (Left l) c = ppPrintLn pfx $
--   concat [n, " received ", show l, " on c", show c, " ($c", show c, ")"]
-- ppPrintLnRecv pfx n (Right ls) c = ppPrintLn pfx $
--   concat [n, " received ", parens (interbar (map show ls)), " on c", show c, " ($c", show c, ")"]

-- ppFnBody :: BaseName -> Prefix -> LambdaSeed -> TyBody -> String
-- ppFnBody n pfx i (Out (OutChan c _) l b k) = concat
--   [ppPrintLnPreSend pfx n l c,
--    pfxStr pfx, "send",
--    parens ("c" ++ show c ++ ", new " ++ show l ++ parens (ppPayloadB b)),
--    " >> {\n",
--    ppPrintLnSend (inc pfx) n l c,
--    ppFnBody n (inc pfx) i k,
--    "\n", pfxStr pfx, "}"]
-- ppFnBody n pfx i (In (InChan c _) l k Nothing) = concat
--   [ppPrintLnPreRecv pfx n (Left l) c,
--    pfxStr pfx, "receive", parens ("c" ++ show c), " {",
--    parens ("x" ++ show i ++ " : " ++ show l), " => \n",
--    ppPrintLnRecv (inc pfx) n (Left l) c,
--    ppFnBody n (inc pfx) (i+1) k,
--    "\n", pfxStr pfx, "}"]
-- ppFnBody n pfx i (In (InChan c _) l k (Just ck)) = concat
--   [ppPrintLnPreRecv pfx n (Left l) c,
--    pfxStr pfx, "receiveErr", "(c", show c, ")(",
--    parens ("x" ++ show i ++ " : " ++ show l), " => {\n",
--    ppPrintLnRecv (inc pfx) n (Left l) c,
--    ppFnBody n (inc pfx) (i+1) k,
--    "\n", pfxStr pfx, "}, ",
--    parens ("e" ++ show i ++ " : Throwable"), " => {\n",
--    ppPrintLn (inc pfx) (concat [n, " detected crash on channel c", show c, " ($c", show c, ")"]),
--    ppFnBody n (inc pfx) (i+1) ck,
--    "\n", pfxStr pfx, "})"]
-- ppFnBody n pfx i (Sel (OutChan c _) ((l,b,k) : ks)) = concat
--   [pfxStr pfx, "val x", show i, " = 0\n",
--    pfxStr pfx, "if ", parens ("x" ++ show i ++ " == 0"), " {\n",
--    send l b k, rest 1 ks
--   ]
--   where
--     send :: Label -> B -> TyBody -> String
--     send l' b' k' = concat
--       [pfxStr (inc pfx), "send",
--        parens ("c" ++ show c ++ ", new " ++ show l' ++ parens (ppPayloadB b')),
--        " >> {\n",
--        ppPrintLnSend (inc (inc pfx)) n l' c,
--        ppFnBody n (inc (inc pfx)) (i+1) k',
--        "\n", pfxStr (inc pfx), "}",
--        "\n", pfxStr pfx, "} "]

--     rest :: Int -> [(Label, B, TyBody)] -> String
--     rest _ [] = ""
--     rest _ [(l',b',k')] = "else {\n" ++ send l' b' k' ++ "\n"
--     rest j ((l',b',k') : ks') = concat
--       ["else if (x", show i, " == ", show j, ") {\n", send l' b' k',
--        rest (j+1) ks']

-- ppFnBody n pfx i (Bra (InChan c _) k ls cs Nothing) = concat
--   [ppPrintLnPreRecv pfx n (Right ls) c,
--    pfxStr pfx, "receive", parens ("c" ++ show c),
--   " {(x", show i, " : ", interbar (map show ls'), ") =>\n",
--   ppPrintLnRecv (inc pfx) n (Right ls) c,
--   pfxStr (inc pfx),
--   toFnName n, show k, parens args, "\n", pfxStr pfx, "}"]
--   where
--     ls' = filter (not . isCrashLabel) ls
--     args = concat ["x", show i, rest]
--     rest = if null cs then "" else
--       ", " ++ intercalate ", " (map f cs)
--     f (InChan j _) = "c" ++ show j
--     f (OutChan j _) = "c" ++ show j

-- ppFnBody n pfx i (Bra (InChan c _) k ls cs (Just ck)) = concat
--   [ppPrintLnPreRecv pfx n (Right ls) c,
--    pfxStr pfx, "receiveErr", parens ("c" ++ show c),
--    "((x", show i, " : ", interbar (map show ls'), ") => {\n", pfxStr (inc pfx),
--    toFnName n, show k, parens args, "\n", pfxStr pfx,
--    "}, (err : Throwable) => {\n",
--    ppFnBody n (inc pfx) i ck,
--    "\n", pfxStr pfx, "})"
--    ]
--   where
--     ls' = filter (not . isCrashLabel) ls
--     args = concat ["x", show i, rest]
--     rest = if null cs then "" else
--       ", " ++ intercalate ", " (map f cs)
--     f (InChan j _) = "c" ++ show j
--     f (OutChan j _) = "c" ++ show j

-- ppFnBody n pfx i (Rec t k) = concat
--   [prefix pfx ("rec" ++ parens (ppRecVar t)), " {\n",
--    pfxStr (inc pfx), "println(\"-- ", n, " entering recursion body; t = ", ppRecVar t, "\")\n",
--    ppFnBody n (inc pfx) i k, "\n", pfxStr pfx, "}"]
-- ppFnBody n pfx _ (Var t) =
--   prefix pfx ("println(\"-- " ++ n ++ " recursing; t = " ++ ppRecVar t ++ "\")\n")
--     ++ prefix pfx ("loop" ++ parens (ppRecVar t))
-- ppFnBody n pfx _ End =
--   intercr [prefix pfx (concat ["println(\"-- ", n, " exits\")"]),
--            prefix pfx "nil"]
-- ppFnBody _ _ _ s = error ("\nUnexpected case in ppFnBody: " ++ show s)

-- ppMatches :: BaseName -> [(Label, TyBody)] -> String
-- ppMatches n0 = intercr . mapMaybe ppMatch where
--   ppMatch (CrashLab,_) = Nothing
--   ppMatch (l,k) = Just $ "  case " ++ show l ++ " => " ++ ppTyBody n0 0 k

-- ppTyDecl :: BaseName -> TyDecl -> String
-- ppTyDecl n0 (Decl _ n cs ks) = concat
--   ["type ", n,
--    brackets ("X0 <: " ++ interbar (mapMaybe f ks) ++ rest),
--    " <: Process = X0 match {\n", ppMatches n0 ks, "\n}"
--    ]
--   where
--     f (CrashLab,_) = Nothing
--     f (l,_) = Just (show l)
--     rest = if null cs then "" else  ", " ++ ppChUBTys cs

-- ppRoleTys :: Maybe [(Effpi, Role, [Label], String)] -> [RoleTy] -> String
-- ppRoleTys _ xs = intercalate spacer (map f xs) where
--   f (MkRoleTy n cs body decls) = concat
--     ["type ", n, brackets (ppChUBTys cs), " = ",
--      ppTyBody n 0 body,
--      spacer, intercalate spacer (map (ppTyDecl n) decls)
--     ]

-- toFnName :: String -> String
-- toFnName = firstLower

-- ppFnMatches :: BaseName -> [(Label, TyBody)] -> String
-- ppFnMatches n0 = intercr . mapMaybe ppFnMatch where
--   ppFnMatch (CrashLab,_) = Nothing
--   ppFnMatch (l,k) = Just $ concat
--     ["  case y : ", show l, " => {\n",
--      ppPrintLn 4 (concat [n0, " received ", show l]),
--      ppFnBody n0 4 0 k,
--      "\n  }"]

-- ppFnDecl :: BaseName -> TyDecl -> String
-- ppFnDecl n0 (Decl _ n cs ks) = concat
--   ["def ", toFnName n,
--    parens ("x : " ++ interbar (mapMaybe f ks) ++ rest),
--    " : ", n, brackets ("x.type" ++ rest'),
--    " = x match {\n", ppFnMatches n0 ks, "\n}"]
--   where
--     f (CrashLab,_) = Nothing
--     f (l,_) = Just (show l)

--     rest = if null cs then "" else ", " ++ intercalate ", " (map g cs)
--     g (InChan j ls) =
--       "c" ++ show j ++ " : InChannel[" ++  interbar (map show ls) ++ "]"
--     g (OutChan j ls) =
--       "c" ++ show j ++ " : OutChannel[" ++ interbar (map show ls) ++ "]"
--     rest' = if null cs then "" else ", " ++ intercalate ", " (map h cs)
--     h (InChan j _) = "c" ++ show j ++ ".type"
--     h (OutChan j _) = "c" ++ show j ++ ".type"

-- ppRoleFns :: Maybe [(Effpi, Role, [Label], String)] -> [RoleTy] -> String
-- ppRoleFns _ xs = "implicit val timeout: Duration = Duration(\"60 seconds\")"
--             ++ spacer ++ intercalate spacer (map f xs) where
--   f (MkRoleTy n cs body decls) = concat
--     ["def ", toFnName n, parens (ppChUBArgs cs),
--      " : ", n, brackets (ppChUBTyAssn cs),
--      " = {\n",
--      ppFnBody n 2 0 body,
--      "\n}",
--      spacer, intercalate spacer (map (ppFnDecl n) decls)]

-- ppChans :: Prefix -> [ChanMap] -> String
-- ppChans pfx cs = intercr (map ppChanDef cs) where
--   ppChanDef (BaseChan i ls) =
--     prefix pfx (concat ["var c", show i, " = Channel[", interbar (map show ls), "]()"])
--   ppChanDef (MergeChan i _ ls) =
--     prefix pfx (concat ["var c", show i, " = Channel[", interbar (map show ls), "]()"])

-- ppEval :: Prefix -> [ChanMap] -> [(String,[ChanUB])] -> String
-- ppEval pfx cs rs = prefix pfx ("eval" ++ parens ("par" ++ parens ppPars)) where
--   ppPars = intercalate ", " (map ppPar rs)
--   ppPar (n,ubs) = toFnName n ++ parens ppChanArgs where
--     ppChanArgs = intercalate ", " (map ppChanArg (sort ubs))
--     ppChanArg (InChan c _) = case c `hasBeenMerged` cs of
--       Just c' -> "c" ++ show c'
--       Nothing -> "c" ++ show c
--     ppChanArg (OutChan c _) = case c `hasBeenMerged` cs of
--       Just c' -> "c" ++ show c'
--       Nothing -> "c" ++ show c

--     hasBeenMerged _ [] = Nothing
--     hasBeenMerged c (BaseChan _ _ : cs') = hasBeenMerged c cs'
--     hasBeenMerged c (MergeChan c' xs _ : cs')
--       | c `elem` xs = Just c'
--       | otherwise = hasBeenMerged c cs'

-- ppMainFn :: MainFn -> String
-- ppMainFn (MkMain cs rs) = concat
--   ["object Main {\n",
--    "  def main() : Unit = main(Array())",
--    spacer,
--    "  def main(args : Array[String]) = {\n",
--    ppChans 4 cs,
--    spacer,
--    ppEval 4 cs rs,
--    "\n  }\n",
--    "}"
--   ]

-- toScala :: Maybe [(Effpi, Role, [Label], String)] -> Effpi -> String
-- toScala cls x = intercalate spacer
--   [ppPackage ("effpi_sandbox." ++ pname x),
--    ppImports (ports x),
--    ppCClasses (nub (cases x ++ cases')),
--    ppRecDefs (recvs x),
--    ppRoleTys cls (types x ++ types'),
--    ppRoleFns cls (types x ++ types'),
--    ppMainFn (mainf x)
--   ]
--   where
--     cases' = case cls of
--       Just cls' -> concatMap (\(e, _, _, _) -> cases e) cls'
--       Nothing -> []
--     types' = case cls of
--       Just cls' -> concatMap (\(e, _, _, _) -> types e) cls'
--       Nothing -> []




module Scala ( toScala ) where

import BaseUtils (parens, brackets, pfxStr, inc, prefix, spacer, firstLower, ErrOr(Ok, Err), intercalate')
import Core ( B(..), Label(CrashLab), Role, G )
import Utils (isCrashLabel)
import EffpiIR
    ( BaseName,
      LambdaSeed,
      Prefix,
      Effpi(pname, ports, cases, recvs, types, mainf),
      MainFn(..),
      RoleTy(..),
      TyDecl(..),
      TyBody(..),
      RecVar(..),
      ChanUB(..),
      ChanMap(..),
      CClass(..) )

import Numeric.Natural (Natural)
import Data.List (intercalate, sort, nub, mapAccumL)
import Data.Maybe (mapMaybe, fromMaybe, isNothing)

import Debug.Trace (trace)

-- [CodeGen] ------------------------------------------------------------------
-- Pretty printing functions: EffpiIR -> Scala

interbar :: [String] -> String
interbar = intercalate " | "

intercr :: [String] -> String
intercr = intercalate "\n"

ppPackage :: String -> String
ppPackage n = "package " ++ n

ppImports :: [String] -> String
ppImports xs = intercr (map ("import " ++) xs)

ppCClasses :: [CClass] -> String
ppCClasses xs = intercr (map f xs) where
  f (MkCClass n arg) = "case class " ++ n ++ parens (maybe "" ("x : " ++) arg)

ppRecDefs :: [RecVar] -> String
ppRecDefs xs = intercr (map f xs) where
  f (MkRecVar _ t) = concat
    ["sealed abstract class ", t, "[A]() extends RecVar[A](\"", t, "\")\n",
     "case object ", t, " extends ", t, "[Unit]"]

ppChUBTys :: [ChanUB] -> String
ppChUBTys xs = intercalate ", " (map f (sort xs)) where
  f (InChan i ls) =
    "C" ++ show i ++ " <: InChannel[" ++ interbar (map show ls) ++ "]"
  f (OutChan i ls) =
    "C" ++ show i ++ " <: OutChannel[" ++ interbar (map show ls) ++ "]"

ppChUBArgs :: [ChanUB] -> String
ppChUBArgs xs = intercalate ", " (map f (sort xs)) where
  f (InChan i ls) =
    "c" ++ show i ++ " : InChannel[" ++ interbar (map show ls) ++ "]"
  f (OutChan i ls) =
    "c" ++ show i ++ " : OutChannel[" ++ interbar (map show ls) ++ "]"

ppChUBTyAssn :: [ChanUB] -> String
ppChUBTyAssn xs = intercalate ", " (map f (sort xs)) where
  f (InChan i _) = "c" ++ show i ++ ".type"
  f (OutChan i _) = "c" ++ show i ++ ".type"

-- this is printing the type
ppTyBody :: Maybe [(Effpi, Role, [Label], String)] -> BaseName -> LambdaSeed -> TyBody -> String
ppTyBody cls r i (Out (OutChan n _) l _ k) = concat
  ["Out", brackets ("C" ++ show n ++ ", " ++ show l), " >>: ",
   ppTyBody (takeBranch l cls) r i k]
ppTyBody Nothing r i (In (InChan n _) l k Nothing) = "In" ++ brackets inner
  where
    lamVar = "x" ++ show i ++ " : " ++ show l
    inner = concat
      ["C", show n, ", ", show l, ", ", parens lamVar, " => ",
       ppTyBody (takeBranch l Nothing) r (i+1) k]
ppTyBody (Just cls) r i (In (InChan n _) l k Nothing) = "In" ++ brackets inner
  where
    lamVar = "x" ++ show i ++ " : " ++ show l
    inner = concat
      ["C", show n, ", ", show l, ", ", parens lamVar, " => ",
       ppTyBody (takeBranch l cls') r (i+1) k]
    cls'  = Just [(c, r, p, l) | (c, r, p, l) <- cls, not (null p)]
ppTyBody cls r i (In (InChan n _) l k (Just ck)) = "InErr" ++ brackets inner where
  lamVar = "x" ++ show i ++ " : " ++ show l
  inner = concat
    ["C", show n, ", ", show l, ", ", parens lamVar, " => ",
      ppTyBody (takeBranch l cls) r (i+1) k, ", (err : Throwable) => ", ppTyBody (takeBranch l cls) r (i+1) ck]
ppTyBody cls r i (Sel (OutChan n _) ks) = parens (interbar (map (parens . f) ks)) where
    f (l, _, k) =
      "Out[C" ++ show n ++ ", " ++ show l ++ "] >>: " ++ ppTyBody (takeBranch l cls) r i k
ppTyBody cls r i (Bra (InChan n _) k ls cs Nothing) = "In" ++ brackets inner
  where
    lamVar = "x" ++ show i ++ " : " ++ interbar (map show ls)
    args = "x" ++ show i ++ ".type" ++ if null cs then "" else
      ", " ++ intercalate ", " (map f cs)
    f (InChan j _) = "C" ++ show j
    f (OutChan j _) = "C" ++ show j
    inner = concat
      ["C", show n, ", ", interbar (map show ls), ", ",
       parens lamVar, " => ", r, show k, brackets args]
ppTyBody cls r i (Bra (InChan n _) k ls cs (Just ck)) = "InErr" ++ brackets inner
  where
    lamVar = "x" ++ show i ++ " : " ++ interbar (map show ls)
    args = "x" ++ show i ++ ".type" ++ if null cs then "" else
      ", " ++ intercalate ", " (map f cs)
    f (InChan j _) = "C" ++ show j
    f (OutChan j _) = "C" ++ show j
    inner = concat
      ["C", show n, ", ", interbar (map show ls), ", ",
       parens lamVar, " => ", r, show k, brackets args,
       ", (err : Throwable) => ", ppTyBody cls r (i+1) ck]
ppTyBody cls r i (Rec (MkRecVar _ t) k) = "Rec" ++ brackets (t ++ ", " ++ ppTyBody cls r i k)
ppTyBody _ _ _ (Var (MkRecVar _ t)) = "Loop" ++ brackets t
ppTyBody _ _ _ End = "PNil"
ppTyBody _ _ _ s = error ("\nUnexpected case in ppTyBody: " ++ show s)

ppPayloadB :: B -> String
ppPayloadB BInt = "42"
ppPayloadB BReal = "42.0"
ppPayloadB BString = "\"\""
ppPayloadB BUnit = ""
ppPayloadB BBool = "true"
ppPayloadB (BType _ ty) = "new " ++ ty ++ "()"

ppRecVar :: RecVar -> String
ppRecVar (MkRecVar _ s) = s

ppPrintLn :: Prefix -> String -> String
ppPrintLn pfx msg = prefix pfx $
  "println" ++ parens ("s\"-- " ++ msg ++ "\"") ++ "\n"

ppPrintLnPreSend :: Prefix -> BaseName -> Label -> Natural -> String
ppPrintLnPreSend pfx n l c = ppPrintLn pfx $
  concat [n, " sending ", show l, " on c", show c, " ($c", show c, ")"]

ppPrintLnSend :: Prefix -> BaseName -> Label -> Natural -> String
ppPrintLnSend pfx n l c = ppPrintLn pfx $
  concat [n, " sent ", show l, " on c", show c, " ($c", show c, ")"]

ppPrintLnPreRecv :: Prefix -> BaseName -> Either Label [Label] -> Natural -> String
ppPrintLnPreRecv pfx n (Left l) c = ppPrintLn pfx $
  concat [n, " expecting ", show l, " on c", show c, " ($c", show c, ")"]
ppPrintLnPreRecv pfx n (Right ls) c = ppPrintLn pfx $
  concat [n, " expecting ", parens (interbar (map show ls)), " on c", show c, " ($c", show c, ")"]

ppPrintLnRecv :: Prefix -> BaseName -> Either Label [Label] -> Natural -> String
ppPrintLnRecv pfx n (Left l) c = ppPrintLn pfx $
  concat [n, " received ", show l, " on c", show c, " ($c", show c, ")"]
ppPrintLnRecv pfx n (Right ls) c = ppPrintLn pfx $
  concat [n, " received ", parens (interbar (map show ls)), " on c", show c, " ($c", show c, ")"]

ppFnBody :: Maybe [(Effpi, Role, [Label], String)] -> BaseName -> Prefix -> LambdaSeed -> TyBody -> String
ppFnBody cls n pfx i (Out (OutChan c _) l b k) = concat
  [ppPrintLnPreSend pfx n l c,
   pfxStr pfx, "send",
   parens ("c" ++ show c ++ ", new " ++ show l ++ parens (ppPayloadB b)),
   " >> {\n",
   ppPrintLnSend (inc pfx) n l c,
   ppFnBody (takeBranch l cls) n (inc pfx) i k,
   "\n", pfxStr pfx, "}"]
ppFnBody Nothing n pfx i (In (InChan c _) l k Nothing) = concat
  [ppPrintLnPreRecv pfx n (Left l) c,
   pfxStr pfx, "receive", parens ("c" ++ show c), " {",
   parens ("x" ++ show i ++ " : " ++ show l), " => \n",
   ppPrintLnRecv (inc pfx) n (Left l) c,
   ppFnBody Nothing n (inc pfx) (i+1) k,
   "\n", pfxStr pfx, "}"]
ppFnBody (Just cls) n pfx i (In (InChan c _) l k Nothing)
  | null fails = concat
    [ppPrintLnPreRecv pfx n (Left l) c,
     pfxStr pfx, "receive", parens ("c" ++ show c), " {",
     parens ("x" ++ show i ++ " : " ++ show l), " => \n",
     ppPrintLnRecv (inc pfx) n (Left l) c,
     ppFnBody (takeBranch l (Just cls')) n (inc pfx) (i+1) k,
     "\n", pfxStr pfx, "}"]
  | otherwise = concat
    [
     if null cs' then "" else pfxStr pfx ++ "try {\n",
     ppPrintLnPreRecv (pfx + 2) n (Left l) c,
     pfxStr (pfx + 2), "receive", "(c", show c, "){",
     parens ("x" ++ show i ++ " : " ++ show l), " => \n",
     ppPrintLnRecv (inc (pfx + 2)) n (Left l) c,
     ppFnBody (takeBranch l (Just cls')) n (inc (pfx + 2)) (i+1) k,
     "\n", pfxStr (pfx + 2), "} ",
     if null cs' then "" else concat [
      "\n", pfxStr pfx, "} finally {\n",
      intercr (map (\(MkMain cs _) -> ppChans (pfx + 4) cs) mains), "\n\n",
      intercr (map (\(MkMain cs rs) -> ppEval (pfx + 4) cs rs) mains), "\n",
      pfxStr pfx, "}"]]
   where
    fails  = [(c, r, p, l) | (c, r, p, l) <- cls, null p]
    mains  = unwrap (map (\(c, _, _, _) -> mainf c) fails)
    cls' = [(c, r, p, l) | (c, r, p, l) <- cls, not (null p)]
    cs'    = concatMap (\(MkMain cs _) -> cs) mains
ppFnBody cls n pfx i (In (InChan c _) l k (Just ck)) = concat
  [ppPrintLnPreRecv pfx n (Left l) c,
   pfxStr pfx, "receiveErr", "(c", show c, ")(",
   parens ("x" ++ show i ++ " : " ++ show l), " => {\n",
   ppPrintLnRecv (inc pfx) n (Left l) c,
   ppFnBody (takeBranch l cls) n (inc pfx) (i+1) k,
   "\n", pfxStr pfx, "}, ",
   parens ("e" ++ show i ++ " : Throwable"), " => {\n",
   ppPrintLn (inc pfx) (concat [n, " detected crash on channel c", show c, " ($c", show c, ")"]),
   ppFnBody (takeBranch l cls) n (inc pfx) (i+1) ck,
   "\n", pfxStr pfx, "})"]
ppFnBody cls n pfx i (Sel (OutChan c _) ((l,b,k) : ks)) = concat
  [pfxStr pfx, "val x", show i, " = 0\n",
   pfxStr pfx, "if ", parens ("x" ++ show i ++ " == 0"), " {\n",
   send l b k, rest 1 ks
  ]
  where
    send :: Label -> B -> TyBody -> String
    send l' b' k' = concat
      [pfxStr (inc pfx), "send",
       parens ("c" ++ show c ++ ", new " ++ show l' ++ parens (ppPayloadB b')),
       " >> {\n",
       ppPrintLnSend (inc (inc pfx)) n l' c,
       ppFnBody (takeBranch l' cls) n (inc (inc pfx)) (i+1) k',
       "\n", pfxStr (inc pfx), "}",
       "\n", pfxStr pfx, "} "]

    rest :: Int -> [(Label, B, TyBody)] -> String
    rest _ [] = ""
    rest _ [(l',b',k')] = "else {\n" ++ send l' b' k' ++ "\n"
    rest j ((l',b',k') : ks') = concat
      ["else if (x", show i, " == ", show j, ") {\n", send l' b' k',
       rest (j+1) ks']

ppFnBody Nothing n pfx i (Bra (InChan c _) k ls cs Nothing) = concat
  [ppPrintLnPreRecv pfx n (Right ls) c,
   pfxStr pfx, "receive", parens ("c" ++ show c),
  " {(x", show i, " : ", interbar (map show ls'), ") =>\n",
  ppPrintLnRecv (inc pfx) n (Right ls) c,
  pfxStr (inc pfx),
  toFnName n, show k, parens args, "\n", pfxStr pfx, "}"]
  where
    ls' = filter (not . isCrashLabel) ls
    args = concat ["x", show i, rest]
    rest = if null cs then "" else
      ", " ++ intercalate ", " (map f cs)
    f (InChan j _) = "c" ++ show j
    f (OutChan j _) = "c" ++ show j
ppFnBody (Just cls) n pfx i (Bra (InChan c _) k ls cs Nothing) 
  | null fails = concat
    [ppPrintLnPreRecv pfx n (Right ls) c,
     pfxStr pfx, "receive", parens ("c" ++ show c),
     " {(x", show i, " : ", interbar (map show ls'), ") =>\n",
     ppPrintLnRecv (inc pfx) n (Right ls) c,
     pfxStr (inc pfx),
     toFnName n, show k, parens args, "\n", pfxStr pfx, "}"]
  | otherwise = concat
    [ppPrintLnPreRecv pfx n (Right ls) c,
     if null cs' then "" else pfxStr pfx ++ "try {\n",
     pfxStr pfx, "receive", parens ("c" ++ show c),
     " {(x", show i, " : ", interbar (map show ls'), ") =>\n",
     ppPrintLnRecv (inc pfx) n (Right ls) c,
     pfxStr (inc pfx),
     toFnName n, show k, parens args, "\n", pfxStr pfx, "}",
     if null cs' then "" else concat [
     "\n", pfxStr pfx, "} finally {\n",
     intercr (map (\(MkMain cs _) -> ppChans (pfx + 4) cs) mains), "\n\n",
     intercr (map (\(MkMain cs rs) -> ppEval (pfx + 4) cs rs) mains), "\n",
     pfxStr pfx, "}"]]
  where
    ls'             = filter (not . isCrashLabel) ls
    args            = concat ["x", show i, rest]
    rest            = if null cs then "" else ", " ++ intercalate ", " (map f cs)
    f (InChan j _)  = "c" ++ show j
    f (OutChan j _) = "c" ++ show j
    fails           = [(c, r, p, l) | (c, r, p, l) <- cls, null p]
    mains           = unwrap (map (\(c, _, _, _) -> mainf c) fails)
    cs'             = concatMap (\(MkMain cs _) -> cs) mains
    
ppFnBody cls n pfx i (Bra (InChan c _) k ls cs (Just ck)) = concat
  [ppPrintLnPreRecv pfx n (Right ls) c,
   pfxStr pfx, "receiveErr", parens ("c" ++ show c),
   "((x", show i, " : ", interbar (map show ls'), ") => {\n", pfxStr (inc pfx),
   toFnName n, show k, parens args, "\n", pfxStr pfx,
   "}, (err : Throwable) => {\n",
   ppFnBody cls n (inc pfx) i ck,
   "\n", pfxStr pfx, "})"
   ]
  where
    ls' = filter (not . isCrashLabel) ls
    args = concat ["x", show i, rest]
    rest = if null cs then "" else
      ", " ++ intercalate ", " (map f cs)
    f (InChan j _) = "c" ++ show j
    f (OutChan j _) = "c" ++ show j

ppFnBody cls n pfx i (Rec t k) = concat
  [prefix pfx ("rec" ++ parens (ppRecVar t)), " {\n",
   pfxStr (inc pfx), "println(\"-- ", n, " entering recursion body; t = ", ppRecVar t, "\")\n",
   ppFnBody cls n (inc pfx) i k, "\n", pfxStr pfx, "}"]
ppFnBody _ n pfx _ (Var t) =
  prefix pfx ("println(\"-- " ++ n ++ " recursing; t = " ++ ppRecVar t ++ "\")\n")
    ++ prefix pfx ("loop" ++ parens (ppRecVar t))
ppFnBody _ n pfx _ End =
  intercr [prefix pfx (concat ["println(\"-- ", n, " exits\")"]),
           prefix pfx "nil"]
ppFnBody _ _ _ _ s = error ("\nUnexpected case in ppFnBody: " ++ show s)

ppMatches :: Maybe [(Effpi, Role, [Label], String)] -> BaseName -> [(Label, TyBody)] -> String
ppMatches cls n0 = intercr . mapMaybe ppMatch where
  ppMatch (CrashLab,_) = Nothing
  ppMatch (l,k) = Just $ "  case " ++ show l ++ " => " ++ ppTyBody cls n0 0 k

ppTyDecl :: Maybe [(Effpi, Role, [Label], String)] -> BaseName -> TyDecl -> String
ppTyDecl cls n0 (Decl _ n cs ks) = concat
  ["type ", n,
   brackets ("X0 <: " ++ interbar (mapMaybe f ks) ++ rest),
   " <: Process = X0 match {\n", ppMatches cls n0 ks, "\n}"
   ]
  where
    f (CrashLab,_) = Nothing
    f (l,_) = Just (show l)
    rest = if null cs then "" else  ", " ++ ppChUBTys cs

ppRoleTys :: Maybe [(Effpi, Role, [Label], String)] -> [RoleTy] -> String
ppRoleTys cls xs = intercalate spacer (map f xs) where
  f rt@(MkRoleTy r n cs body decls) = concat
    ["type ", n, brackets (ppChUBTys cs), " = ",
     ppTyBody cls' n 0 body,
     spacer, intercalate spacer (map (\decl@(Decl n' _ _ _) -> ppTyDecl (travRoleTy cls' rt n') n decl) decls)]
     where
      cls' = f r cls
      f :: Role -> Maybe [(Effpi, Role, [Label], String)] -> Maybe [(Effpi, Role, [Label], String)]
      f r cls = case cls of
        Just clsList -> Just [(c, r', p, l) | (c, r', p, l) <- clsList, r == r']
        Nothing -> Nothing

toFnName :: String -> String
toFnName = firstLower

ppFnMatches :: Maybe [(Effpi, Role, [Label], String)] -> BaseName -> [(Label, TyBody)] -> String
ppFnMatches cls n0 = intercr . mapMaybe ppFnMatch where
  ppFnMatch (CrashLab,_) = Nothing
  ppFnMatch (l,k) = Just $ concat
    ["  case y : ", show l, " => {\n",
     ppPrintLn 4 (concat [n0, " received ", show l]),
     ppFnBody cls n0 4 0 k,
     "\n  }"]

ppFnDecl :: Maybe [(Effpi, Role, [Label], String)] -> BaseName -> TyDecl -> String
ppFnDecl cls n0 (Decl _ n cs ks) = concat
  ["def ", toFnName n,
   parens ("x : " ++ interbar (mapMaybe f ks) ++ rest),
   " : ", n, brackets ("x.type" ++ rest'),
   " = x match {\n", ppFnMatches cls n0 ks, "\n}"]
  where
    f (CrashLab,_) = Nothing
    f (l,_) = Just (show l)

    rest = if null cs then "" else ", " ++ intercalate ", " (map g cs)
    g (InChan j ls) =
      "c" ++ show j ++ " : InChannel[" ++  interbar (map show ls) ++ "]"
    g (OutChan j ls) =
      "c" ++ show j ++ " : OutChannel[" ++ interbar (map show ls) ++ "]"
    rest' = if null cs then "" else ", " ++ intercalate ", " (map h cs)
    h (InChan j _) = "c" ++ show j ++ ".type"
    h (OutChan j _) = "c" ++ show j ++ ".type"

ppRoleFns :: Maybe [(Effpi, Role, [Label], String)] -> [RoleTy] -> String
ppRoleFns cls xs = "implicit val timeout: Duration = Duration(\"60 seconds\")"
            ++ spacer ++ intercalate spacer (map f xs) where
  f rt@(MkRoleTy r n cs body decls) = concat
    ["def ", toFnName n, parens (ppChUBArgs cs),
     " : ", n, brackets (ppChUBTyAssn cs),
     " = {\n",
     ppFnBody cls' n 2 0 body,
     "\n}",
     spacer, intercalate spacer (map (\decl@(Decl n' _ _ _) -> ppFnDecl (travRoleTy cls' rt n') n decl) decls)]
     where
      cls' = f r cls
      -- filters to paths that are specific to the role
      f :: Role -> Maybe [(Effpi, Role, [Label], String)] -> Maybe [(Effpi, Role, [Label], String)]
      f r cls = case cls of
        Just clsList -> Just [(c, r', p, l) | (c, r', p, l) <- clsList, r == r']
        Nothing -> Nothing

-- trav func to find the cls specific to each of decls in ppRoleFns
travRoleTy :: Maybe [(Effpi, Role, [Label], String)] -> RoleTy -> Natural -> Maybe [(Effpi, Role, [Label], String)]
travRoleTy cls'' (MkRoleTy _ _ _ body' decls') n = travTyBody cls'' body' n
  where
    travTyBody :: Maybe [(Effpi, Role, [Label], String)] -> TyBody -> Natural -> Maybe [(Effpi, Role, [Label], String)]
    travTyBody cls''' (Out _ l _ tb) n' = travTyBody (takeBranch l cls''') tb n'
    travTyBody cls''' (In _ l tb _) n' = travTyBody (takeBranch l cls''') tb n'
    travTyBody cls''' (Sel _ ks) n' 
      = Just $ concatMap (\(l, _, tb) ->
        Data.Maybe.fromMaybe [] (travTyBody (takeBranch l cls''') tb n')) ks
    travTyBody cls''' (Bra _ n'' ls _ _) n'
      | n' == n'' = cls'''
      | otherwise = travTyDecl cls'''' decl n''
        where
          decl      = head [d | d@(Decl n''' _ _ _) <- decls', n' == n''']
          cls'''' = fst (mapAccumL (\cls l -> (takeBranch l cls, l)) cls''' ls)
    travTyBody cls''' (Rec _ tb) n' = travTyBody cls''' tb n'
    travTyBody cls''' (Var _) _ = cls'''
    travTyBody cls''' End _ = cls'''

    travTyDecl :: Maybe [(Effpi, Role, [Label], String)] -> TyDecl -> Natural -> Maybe [(Effpi, Role, [Label], String)]
    travTyDecl cls''' (Decl _ _ _ ks) n = Just $ concatMap (\(l, tb) -> Data.Maybe.fromMaybe [] (travTyBody (takeBranch l cls''') tb n)) ks


ppChans :: Prefix -> [ChanMap] -> String
ppChans pfx cs = intercr (map ppChanDef cs) where
  ppChanDef (BaseChan i ls) =
    prefix pfx (concat ["var c", show i, " = Channel[", interbar (map show ls), "]()"])
  ppChanDef (MergeChan i _ ls) =
    prefix pfx (concat ["var c", show i, " = Channel[", interbar (map show ls), "]()"])

ppEval :: Prefix -> [ChanMap] -> [(String,[ChanUB])] -> String
ppEval pfx cs rs = prefix pfx ("eval" ++ parens ("par" ++ parens ppPars)) where
  ppPars = intercalate ", " (map ppPar rs)
  ppPar (n,ubs) = toFnName n ++ parens ppChanArgs where
    ppChanArgs = intercalate ", " (map ppChanArg (sort ubs))
    ppChanArg (InChan c _) = case c `hasBeenMerged` cs of
      Just c' -> "c" ++ show c'
      Nothing -> "c" ++ show c
    ppChanArg (OutChan c _) = case c `hasBeenMerged` cs of
      Just c' -> "c" ++ show c'
      Nothing -> "c" ++ show c

    hasBeenMerged _ [] = Nothing
    hasBeenMerged c (BaseChan _ _ : cs') = hasBeenMerged c cs'
    hasBeenMerged c (MergeChan c' xs _ : cs')
      | c `elem` xs = Just c'
      | otherwise = hasBeenMerged c cs'

ppMainFn :: MainFn -> String
ppMainFn (MkMain cs rs) = concat
  ["object Main {\n",
   "  def main() : Unit = main(Array())",
   spacer,
   "  def main(args : Array[String]) = {\n",
   ppChans 4 cs,
   spacer,
   ppEval 4 cs rs,
   "\n  }\n",
   "}"
  ]

toScala :: Maybe [(Effpi, Role, [Label], String)] -> Effpi -> String
toScala cls x = intercalate spacer
  [ppPackage ("effpi_sandbox." ++ pname x),
   ppImports (ports x),
   ppCClasses (nub (cases x ++ cases')),
   ppRecDefs (recvs x),
   ppRoleTys cls (types x ++ types'),
   ppRoleFns cls (types x ++ types'),
   ppMainFn (mainf x)
  ]
  where
    cases' = case cls of
      Just cls' -> concatMap (\(e, _, _, _) -> cases e) cls'
      Nothing -> []
    types' = case cls of
      Just cls' -> concatMap (\(e, _, _, _) -> types e) cls'
      Nothing -> []

takeBranch :: Label -> Maybe [(Effpi, Role, [Label], String)] -> Maybe [(Effpi, Role, [Label], String)]
takeBranch _ Nothing = Nothing
takeBranch targetLabel (Just tuples) = Just $ map processTuple tuples
  where
    processTuple (g, role, labels, str) =
      case labels of
        [] -> (g, role, labels, str)
        (l:ls) -> if l == targetLabel
                  then (g, role, ls, str)
                  else (g, role, l:ls, str)

showcls :: Maybe [(Effpi, Role, [Label], String)] -> String
showcls Nothing = "\ncls is nothing"
showcls (Just []) = "\ncls is Just []"
showcls (Just [(_, r, p, _)]) = "\n" ++ show r ++ show p ++ "\ndone"
showcls (Just ((_, r, p, _) : cls)) = "\n" ++ show r ++ show p ++ showcls (Just cls)

unwrap :: [MainFn] -> [MainFn]
unwrap mains = snd (mapAccumL (\acc m@(MkMain cs rs) -> (acc + (fromIntegral (maxList (idsInMain m)) + 1), shiftMain (fromIntegral acc) (MkMain cs rs))) 0 mains)
  where
    
    shiftMain :: Natural -> MainFn -> MainFn
    shiftMain n (MkMain cs rs) = MkMain (map (shiftCs n) cs) (map (shiftRs n) rs)

    shiftCs :: Natural -> ChanMap -> ChanMap
    shiftCs n (BaseChan n' ls) = BaseChan (n + n') ls
    shiftCs n (MergeChan n' ns ls) = MergeChan (n + n') (map (+ n) ns) ls

    shiftRs :: Natural -> (String, [ChanUB]) -> (String, [ChanUB])
    shiftRs n (r,ubs) = (r, map (shiftChanUB n) ubs)
    
    shiftChanUB :: Natural -> ChanUB -> ChanUB
    shiftChanUB n (InChan c ls) = InChan (c + n) ls
    shiftChanUB n (OutChan c ls) = OutChan (c + n) ls

    idsInMain :: MainFn -> [Natural]
    idsInMain (MkMain cs rs) = sort (nub (concat (map idsInCs cs) ++ concat (map idsInRs rs)))

    idsInCs :: ChanMap -> [Natural]
    idsInCs (BaseChan n _) = [n]
    idsInCs (MergeChan n ns _) = n : ns
    
    idsInRs :: (String, [ChanUB]) -> [Natural]
    idsInRs (_,ubs) = concat (map idsInUBs ubs)

    idsInUBs :: ChanUB -> [Natural]
    idsInUBs (InChan c _) = [c]
    idsInUBs (OutChan c _) = [c]

    maxList :: [Natural] -> Natural
    maxList [] = error "maxList error"
    maxList (x:xs) = foldl max x xs
    
    