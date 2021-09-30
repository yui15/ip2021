module Cbrt where

type HontaiKakaku  = Double
type ZeikomiKakaku = Double
zeikomi :: HontaiKakaku -> ZeikomiKakaku
zeikomi x = 1.1 * x

cube :: Double -> Double
cube x = x * x * x