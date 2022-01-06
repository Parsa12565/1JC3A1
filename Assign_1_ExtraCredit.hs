{- Assignment 1 Extra Credit
 - Name: Parsa Zanganeh
 - Date: 2020-10-07
 -}
module Assign_1_ExtraCredit where

macid :: String
macid = "zanganep"


factorial :: Double -> Double
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

evaluate :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
evaluate a cos_a sin_a x k t f = if abs ((cosTaylor a cos_a sin_a x k f) - (cos x)) < t
                                    then cosTaylor a cos_a sin_a x k f
                                    else evaluate a cos_a sin_a x (k+4) t (cosTaylor a cos_a sin_a x k f)

cosTaylor :: Double -> Double -> Double -> Double -> Double -> Double -> Double
cosTaylor a cos_a sin_a x k f = f + cos_a/(factorial k)*(x-a)**k - sin_a/(factorial (k+1))*(x-a)**(k+1) - cos_a/(factorial (k+2))*(x-a)**(k+2) + sin_a/(factorial (k+3))*(x-a)**(k+3)

fmod :: Double -> Double -> Double
fmod x y = if x < 0
            then fmod (x+y) y
            else if x - y < 0
                    then x
                    else fmod (x-y) y


{- -----------------------------------------------------------------
 - cosApprox
 - -----------------------------------------------------------------
 - Description: takes a value x and a tolerance t and returns an approximation of cos(x) with a relative error within tolerance t
 -}
cosApprox :: Double -> Double -> Double
cosApprox x t
    |0 <= fmod x (2*pi) && fmod x (2*pi) < pi/4 = evaluate 0 1 0 (fmod x (2*pi)) 0 t 0
    |pi/4 <= fmod x (2*pi) && fmod x (2*pi) < 3*pi/4 = evaluate (pi/2) 0 1 (fmod x (2*pi)) 0 t 0
    |3*pi/4 <= fmod x (2*pi) && fmod x (2*pi) < 5*pi/4 = evaluate pi (-1) 0 (fmod x (2*pi)) 0 t 0
    |5*pi/4 <= fmod x (2*pi) && fmod x (2*pi) < 7*pi/4 = evaluate (3*pi/2) 0 (-1) (fmod x (2*pi)) 0 t 0
    |7*pi/4 <= fmod x (2*pi) && fmod x (2*pi) < 2*pi = evaluate (2*pi) 1 0 (fmod x (2*pi)) 0 t 0

{- -----------------------------------------------------------------
 - sinApprox
 - -----------------------------------------------------------------
 - Description: takes a value x and a tolerance t and returns an approximation of sin(x) with a relative error within tolerance t
 -}
sinApprox :: Double -> Double -> Double
sinApprox x t = -1*(cosApprox (x+pi/2) t)

{- -----------------------------------------------------------------
 - tanApprox
 - -----------------------------------------------------------------
 - Description: takes a value x and a tolerance t and returns an approximation of tan(x) with a relative error within tolerance t
 -}
tanApprox :: Double -> Double -> Double
tanApprox x t = (sinApprox x t)/(cosApprox x t)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}

-- TODO Add Test Cases for each of your functions below here
