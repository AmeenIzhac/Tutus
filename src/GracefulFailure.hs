module GracefulFailure where
import Core
import Utils
import Text.Printf (printf)

gf :: String -> G a -> G a
gf m g = gf' m g []

-- g is global protocol
-- crs is crashed roles
-- urs is unaware roles roles

gf' :: String -> G a -> [Role] -> G a
gf' _ GEnd _ = GEnd
gf' m (GRec a g) crs = GRec a (gf' m g crs)
gf' _ (GVar n a) _       = GVar n a
gf' m g@(GComm p q a cs) crs
  | any (isCrashLabel . label) cs || reliable p = GComm p q a cs'
  | otherwise                                     = GComm p q a (cs' ++ [cb''])
  where
    g'                  = traceFirst g
    rs                  = roles g'
    cs'                 = map f cs
    cb                  = gcb m g' (p : crs) (filter (/= p) rs) 
    cb'                 = case cb of
                            GComm _ _ _ [c] -> c
                            _ -> error "Expected GComm with a single Choice"
    cb''                = f cb' 
    f (Choice l p' g'') = Choice l p' (gf' m g'' (p : crs))

    gcb _ _ _ [] = GEnd
    gcb m (GComm p q a cs) crs urs
      | elem p crs && elem q urs       = GComm p q a [Choice CrashLab BUnit g''']
      | notElem p urs && elem q urs    = GComm p q a [Choice ExitLab BUnit g''']
      | notElem p urs && notElem q urs = gcb m g'' crs urs
      | m == "LGF"                     = GComm p q a (map (\(Choice l p g) -> Choice l p (gcb m g crs urs)) cs)
      | otherwise                      = error "Ambiguity found"
      where
        g''  = cont (head cs)
        g''' = gcb m g'' crs (filter (/= q) urs)
    gcb m (GRec a g) crs urs 
      | f' g urs             = gcb m g crs urs
      | otherwise            = GRec a (gcb m g crs urs)
    gcb _ (GVar n a) _ _ = GVar n a
    gcb _ (GVar _ _) _ _     = error "GF not applicable, try LGF"
    gcb _ GEnd _ _           = GEnd
    
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