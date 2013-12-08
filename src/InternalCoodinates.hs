{-# Language BangPatterns #-}


module InternalCoordinates (
                           angle
                          ,bond
                          ,dihedral
                          ,transform2Cart
                           ) where

import Control.Applicative                           
import Control.Lens
import Data.List as DL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- =======> Internal Modules <========
import CommonTypes


-- =================> <=======================


-- =============>  <=================
type VD = VU.Vector Double

-- =============> <==================
vecdot :: VD -> VD -> Double
vecdot v1 v2 = VU.sum $ VU.zipWith (*) v1 v2

vecnorm :: VD -> Double
vecnorm v = sqrt $ vecdot v v

vecCross :: VD -> VD -> VD  
vecCross !v1 !v2 = VU.fromList $ fmap permute [(1,2),(2,0),(0,1)]
  where permute (i,j) = (v1 VU.! i)*(v2 VU.! j) - (v2 VU.! i)*(v1 VU.! j)

vecsub :: VD -> VD -> VD
vecsub !v1 !v2 = VU.zipWith (-) v1 v2        
        
dif2 :: VD -> VD -> VD -> Double
dif2 !p1 !p2 !p3 =  (p1 `vecsub` p2) `vecdot` (p3 `vecsub` p2)

-- =============> <============                 
bond :: VD -> VD -> Double
bond !p1 !p2 = vecnorm $ p1 `vecsub` p2

angle :: VD -> VD -> VD -> Double
angle !p1 !p2 !p3 = acos $ arg
  where arg = (dif2 p1 p2 p3) / ((p1 `bond` p2) * (p2 `bond` p3))

dihedral :: VD -> VD -> VD -> VD -> Double
dihedral !p1 !p2 !p3 !p4  =
  let [xba,xca,xcb,xdb] = DL.zipWith (vecsub) [p2,p3,p3,p4] [p1,p1,p2,p2]
      [w1,w2] = DL.zipWith vecCross [xba,xcb] [xca,xdb]
      [n1,n2] = fmap vecnorm [w1,w2]
      teta = acos $ ((w1 `vecdot` w2) / (n1*n2))
  in if  (signum  $ w2 `vecdot` xba) > 0.0  then teta else -teta
  
  
calcInternals :: Connections -> Molecule -> Internals
calcInternals conex mol = undefined{- V.map fun conex 
  where vss = chunks 3 $ mol ^. getCoord to R.toUnboxed 
        fun x = case x of
                     Bond     a b     -> bond 
                     Angle    a b c   -> angle
                     Dihedral a b c d -> dihedral-}
                     
                     
  
chunks :: Int -> VD ->  V.Vector VD
chunks n v = V.map (\x -> VU.backpermute v $ VU.generate n $ \y -> n*x +y ) ixs
  where dim = (VU.lenght v) `div` n
        ixs = V.generate dim id
            
  
transform2Cart :: Internals -> VD
transform2Cart = undefined

  