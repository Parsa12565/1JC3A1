{- Assignment 1
 - Name: Parsa Zanganeh
 - Date: 2020-10-07
 -}
module Assign_1 where

macid :: String
macid = "zanganep"


factorial :: Double -> Double
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

{- -----------------------------------------------------------------
 - cosTaylor
 - -----------------------------------------------------------------
 - Description: computes the 4th Taylor polynomial approximation of cos(x) at a
 -}
cosTaylor :: Double -> Double -> Double -> Double -> Double
cosTaylor a cos_a sin_a x = cos_a - sin_a*(x-a) - cos_a/(factorial 2)*(x-a)**2 + sin_a/(factorial 3)*(x-a)**3 + cos_a/(factorial 4)*(x-a)**4

{- -----------------------------------------------------------------
 - fmod
 - -----------------------------------------------------------------
 - Description: computes mod
 -}
fmod :: Double -> Double -> Double
fmod x y = if x < 0
            then fmod (x + y) y
            else if (x - y) < 0
                    then x
                    else fmod (x - y) y


{- -----------------------------------------------------------------
 - cosApprox
 - -----------------------------------------------------------------
 - Description: computes an approximation of cos(x) using cosTaylor
 -}
cosApprox :: Double -> Double
cosApprox x
    |0 <= fmod x (2*pi) && fmod x (2*pi) < pi/4 = cosTaylor 0 1 0 (fmod x (2*pi))
    |pi/4 <= fmod x (2*pi) && fmod x (2*pi) < 3*pi/4 = cosTaylor (pi/2) 0 1 (fmod x (2*pi))
    |3*pi/4 <= fmod x (2*pi) && fmod x (2*pi) < 5*pi/4 = cosTaylor pi (-1) 0 (fmod x (2*pi))
    |5*pi/4 <= fmod x (2*pi) && fmod x (2*pi) < 7*pi/4 = cosTaylor (3*pi/2) 0 (-1) (fmod x (2*pi))
    |7*pi/4 <= fmod x (2*pi) && fmod x (2*pi) < 2*pi = cosTaylor (2*pi) 1 0 (fmod x (2*pi))

{- -----------------------------------------------------------------
 - sinApprox
 - -----------------------------------------------------------------
 - Description: uses cosApprox to approximate the value of sin(x)
 -}
sinApprox :: Double -> Double
sinApprox x = -1*(cosApprox (x+pi/2))

{- -----------------------------------------------------------------
 - tanApprox
 - -----------------------------------------------------------------
 - Description: uses cosApprox and sinApprox to approximate the value of tan(x)
 -}
tanApprox :: Double -> Double
tanApprox x = (sinApprox x)/(cosApprox x)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}

-- TODO Add Test Cases for each of your functions below here
