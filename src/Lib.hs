module Lib where

-- | The volume of a d-ball
vNSphere :: Double -> Double -> Double
vNSphere r d = pi ** (d/2) * r ** d / gamma (d/2 + 1)

-- | The volume ratio of a d-ball of radius 1 to that of a d-ball of radius 1.001, as a function of d.
vRatio :: Double -> Double
vRatio d = vNSphere 1 d / vNSphere (1 + 0.01) d

-- | "Curse of dimensionality" demo
cod :: IO ()
cod = print $ map vRatio [2, 4 .. 200]


assert2 = vNSphere 1 2 ~= pi

class Eps e where
  (~=) :: e -> e -> Bool

instance Eps Double where
  x1 ~= x2 = abs (x1 - x2) <= 1e-12


-- | Gamma function (Lanczos approximation)
--
-- as described in 'Numerical Recipes in C++', the approximation is taken from [Lanczos, C. 1964 SIAM Journal on Numerical Analysis, ser. B, vol. 1, pp. 86-96]

cof :: [Double]
cof =
  [ 76.18009172947146
  , -86.50532032941677
  , 24.01409824083091
  , -1.231739572450155
  , 0.001208650973866179
  , -0.000005395239384953
  ]
 
s :: Double
s = 1.000000000190015
 
gammaln :: Double -> Double
gammaln xx =
  let t    = (xx + 5.5) - (xx + 0.5) * log (xx + 5.5)
      s'   = s + sum (zipWith (/) cof [xx + 1 ..])
  in - t + log (2.5066282746310005 * s' / xx)

gamma :: Double -> Double
gamma = exp . gammaln
