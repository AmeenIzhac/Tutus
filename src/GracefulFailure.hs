module GracefulFailure where
import Core
import Utils
import Text.Printf (printf)

gf :: G a -> G a
gf g = gf' g []

-- g is global protocol
-- crs is crashed roles
gf' :: G a -> [Role] -> G a
gf' GEnd _ = GEnd
gf' (GRec a g) crs = GRec a (gf' g crs)
gf' (GVar n a) _       = GVar n a
gf' g@(GComm p q a cs) crs
  | any (isCrashLabel . label) cs || reliable p = GComm p q a cs'
  | otherwise                                     = GComm p q a (cs' ++ [cb''])
  where
    g'                = traceFirst g
    rs                = roles g'
    cs'               = map f cs
    cb                = gcb g' (p : crs) (filter (/= p) rs) 
    cb'               = case cb of
                          GComm _ _ _ [c] -> c
                          _ -> error "Expected GComm with a single Choice"
    cb''              = f cb' 
    f (Choice l p' g'') = Choice l p' (gf' g'' (p : crs))

    gcb :: G a -> [Role] -> [Role] -> G a
    gcb _ _ [] = GEnd
    gcb (GComm p q a cs) crs urs
      | elem p crs && elem q urs       = GComm p q a [Choice CrashLab BUnit g''']
      | notElem p urs && elem q urs    = GComm p q a [Choice ExitLab BUnit g''']
      | notElem p urs && notElem q urs = gcb g'' crs urs
      where
        Choice l p' g'' = head cs
        g''' = gcb g'' crs (filter (/= q) urs)
        cont (Choice _ _ g) = g
    gcb (GRec a g) crs urs 
      | f' g urs = gcb g crs urs
      | otherwise      = GRec a (gcb g crs urs)
    gcb (GVar _ _) _ _ = error "try lgf"
    gcb GEnd _ _ = GEnd
    gcb _ _ _ = error "ambiguity found"
    
    f' _ [] = True
    f' (GComm p q a (c : _)) urs 
      | notElem p urs = f' g'' (filter (/= q) urs)
      | otherwise     = f' g'' urs
      where
        Choice _ _ g'' = c
    f' (GRec a g) urs = f' g urs
    f' _ urs = False

    traceFirst (GComm r1 r2 a (c:cs)) = GComm r1 r2 a [c']
      where 
        Choice l p' g = c
        c' = Choice l p' (traceFirst g)
    traceFirst g = g