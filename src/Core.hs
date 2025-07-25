{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Core where

import Numeric.Natural ( Natural )
import GHC.Generics (Generic)
import Control.DeepSeq (NFData, rnf)

-- | A role is either Reliable or Unreliable
data Reliability = R | U deriving (Show, Generic, NFData)
-- | A role has a numerical ID and a human-readable name
data Role = MkRole Natural String Reliability deriving (Generic, NFData)
-- | A label has a numerical ID and a human-readable name, or is `crash`
data Label = MkLabel Natural String | CrashLab | ExitLab deriving (Generic, NFData)

instance Eq Role where
  (==) (MkRole i _ _) (MkRole j _ _) = i == j
instance Eq Label where
  (==) (MkLabel i _) (MkLabel j _) = i == j
  (==) CrashLab CrashLab = True
  (==) ExitLab ExitLab = True
  (==) _ _ = False

instance Show Role where
  show (MkRole _ p _) = p
instance Show Label where
  show (MkLabel _ l) = l
  show CrashLab = "crash"
  show ExitLab = "exit"

instance Ord Role where
  compare (MkRole i _ _) (MkRole j _ _) = compare i j
instance Ord Label where
  compare (MkLabel i _) (MkLabel j _) = compare i j
  compare CrashLab (MkLabel _ _) = LT
  compare (MkLabel _ _) CrashLab = GT
  compare ExitLab (MkLabel _ _) = LT
  compare (MkLabel _ _) ExitLab = GT
  compare CrashLab CrashLab = EQ
  compare ExitLab ExitLab = EQ
  compare ExitLab CrashLab = GT
  compare CrashLab ExitLab = LT

-- | Ground types.
data B = BInt | BReal | BString | BUnit | BBool | BType Natural String
       deriving (Eq, Ord, Generic, NFData)

-- | A choice is a triple comprising a label, a payload, and a continuation.
data Choice ty a = Choice {
    label :: Label,
    payload :: B,
    cont :: ty a
  } deriving (Show, Generic, NFData)

-- | Global types.
data G a = GComm Role Role a [Choice G a]
         | GRec a (G a)
         | GVar Natural a
         | GEnd
         deriving Show

instance (NFData a) => NFData (G a) where
  rnf (GComm r1 r2 x choices) = rnf r1 `seq` rnf r2 `seq` rnf x `seq` rnf choices
  rnf (GRec x g) = rnf x `seq` rnf g
  rnf (GVar n x) = rnf n `seq` rnf x
  rnf GEnd = ()

-- | Local types.
data S a = SRecv Role a [Choice S a]
         | SSend Role a [Choice S a]
         | SRec a (S a)
         | SVar Natural a
         | SEnd
         deriving (Show, Generic, NFData)

instance Show B where
  show BInt = "Int"
  show BReal = "Double"
  show BString = "String"
  show BUnit = "()"
  show BBool = "Boolean"
  show (BType _ ty) = ty

