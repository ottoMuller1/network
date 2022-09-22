module Analysis where

------------------------differential important-----------------
-- differ ort
differ :: ( Double -> Double ) -> Double -> Double -> Double
differ f step x = ( f ( x + step ) - f x ) / step
-- f' with step 0.00001 equal to differ f 0.00001

-- integration
integ :: ( Double -> Double ) -> Double -> Double -> Int -> Double
integ _ _ _ 0 = 0
integ f x step count =
    if count < 0 || step < 0 then error "wrong count or step"
    else f x * step + integ f ( x + step ) step ( count - 1 )

-- activation
stairs :: Double -> Double
stairs x
    | x <= 0 = 0
    | otherwise = 1

linear :: Double -> Double
linear x
    | x <= - 0.5 = 0
    | x >= 0.5 = 1
    | otherwise = x + 0.5

sigmoid :: Double -> Double
sigmoid x = 1 / ( 1 + exp ( - x ) )

relu :: Double -> Double
relu = max 0
