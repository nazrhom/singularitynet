module Utils(
  filterPMap,
  filterPositiveValue,
  filterUnitValue,
  filterZeroValue,
  fmapPMap
) where

import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Api.V1.AssocMap (PMap(PMap))
import Plutarch.Api.V1.Value (PValue(PValue))
import Plutarch.Monadic qualified as P

filterPMap ::
  forall (a :: PType) (b :: PType) (s :: S).
  (PIsData a, PIsData b) =>
  Term s ((b :--> PBool) :--> PMap a b :--> PMap a b)
filterPMap = phoistAcyclic $ plam $
  \pred map ->
    pcon $ PMap $ pfilter #
      (plam $ \pair -> pred # pfromData (psndBuiltin # pair))
      # pto map

fmapPMap ::
  forall (a :: PType) (b :: PType) (k :: PType) (s :: S).
  (PIsData a, PIsData b, PIsData k) =>
  Term s ((a :--> b) :--> PMap k a :--> PMap k b)
fmapPMap = phoistAcyclic $ plam $
  \f map ->
    pcon $ PMap $ pmap #
      (plam $ \pair ->
        ppairDataBuiltin
          # (pfstBuiltin # pair)
          # pdata (f # pfromData (psndBuiltin # pair)))
      # pto map

filterValueByAmount ::
  forall (s :: S). Term s ((PInteger :--> PBool) :--> PValue :--> PValue)
filterValueByAmount = phoistAcyclic $ plam $ \f v -> P.do
  smap <- plet $ fmapPMap
    # (plam $ \tmap -> filterPMap # (plam $ \n -> f # n) # tmap)
    # pto v
  pcon $ PValue $ filterPMap # (plam $ \m -> pnot # (pnull # (pto m))) # smap

filterZeroValue :: forall (s :: S). Term s (PValue :--> PValue)
filterZeroValue =
  plam $ \v -> filterValueByAmount # (plam $ \n -> pnot # (0 #== n)) # v

filterPositiveValue :: forall (s :: S). Term s (PValue :--> PValue)
filterPositiveValue =
  plam $ \v -> filterValueByAmount # (plam $ \n -> 0 #< n) # v

filterUnitValue :: forall (s :: S). Term s (PValue :--> PValue)
filterUnitValue =
  plam $ \v -> filterValueByAmount # (plam $ \n -> pnot # (1 #== n)) # v
