{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Machine where

import qualified Algebraic as Alg
import qualified Analysis as Anl
import Algebraic ( ( <:> ) )
import Data.Kind ( Type )
import qualified Require.Deps as Req

---------------------------------type declarations---------------------------------
type NeuronLayer = Alg.Vector Double
type SynapseLayer = Alg.Matrix Double

-- Perceptron here is a part of Neural network, that full binded
data family Perceptron a :: Type
data instance Perceptron NeuronLayer = Only NeuronLayer | NeuronLayer :--> ( Perceptron SynapseLayer )
data instance Perceptron SynapseLayer = SynapseLayer :==> Perceptron NeuronLayer

-- networks
type PerceptronNetwork = Perceptron NeuronLayer

{-
the height of network is a count of components
layer1 :--> ( synapses1 :==> layer2 :--> ( synapses2 :==> Only layer3 ) )
here height is 3

the width of network is max length of synapses
-}

-----------------------------------instances-----------------------------------
instance Show PerceptronNetwork where
    show :: Perceptron NeuronLayer -> String
    show ( Only Alg.Ev ) = "1"
    show ( Only Alg.ZeroVector ) = "0"
    show ( Only ( Alg.Vector vector ) ) = show vector
    show ( ( Alg.Vector vector ) :--> sl ) = show vector ++ " -> " ++ show sl
    show ( Alg.ZeroVector :--> sl ) = "0 -> " ++ show sl
    show ( Alg.Ev :--> sl ) = "1 -> " ++ show sl

instance Show ( Perceptron SynapseLayer ) where
    show :: Perceptron SynapseLayer -> String
    show ( Alg.ZeroMatrix :==> nl ) = "0 => " ++ show nl
    show ( Alg.E :==> nl ) = "1 => " ++ show nl
    show ( ( Alg.Matrix columns ) :==> nl ) = show ( ( \( Alg.Vector vector ) -> vector ) <$> columns ) ++ " => " ++ show nl

instance Semigroup PerceptronNetwork where
    (<>) :: PerceptronNetwork -> PerceptronNetwork -> PerceptronNetwork
    left@( Only a ) <> ( Only b )
        | a == b = a :--> ( Alg.E :==> Only b )
        | otherwise = left
    left@( Only a ) <> right@( a' :--> sl )
        | a == a' = right
        | otherwise = left
    ( a :--> ( b :==> c ) ) <> right@( Only с' ) =
        a :--> ( b :==> ( c <> right ) )
    ( a :--> ( b :==> c ) ) <> right@( a' :--> c' ) =
        a :--> ( b :==> ( c <> right ) )

------------------------------features---------------------------------------------
-- layer closure
pancakePN :: ( Double -> Double ) -> SynapseLayer -> NeuronLayer -> PerceptronNetwork
pancakePN f sl nl = nl :--> ( sl :==> Only ( f <$> Alg.toVector ( ( f <$> nl ) <:> sl ) ) )

fromPN :: PerceptronNetwork -> NeuronLayer
fromPN ( Only a ) = a
fromPN ( a :--> ( a' :==> b ) ) = fromPN b

buildPN :: ( Double -> Double ) -> PerceptronNetwork -> [ SynapseLayer ] -> PerceptronNetwork
buildPN f = foldl ( \pn sl -> pn <> pancakePN f sl ( fromPN pn ) )

-- network instruments
heightPN :: PerceptronNetwork -> Int
heightPN ( Only _ ) = 1
heightPN ( _ :--> ( _ :==> pn ) ) = 1 + heightPN pn

widthPN :: PerceptronNetwork -> Int
widthPN ( Only Alg.ZeroVector ) = 1
widthPN ( Only Alg.Ev ) = 1
widthPN ( Only ( Alg.Vector vector ) ) = length vector
widthPN ( Alg.Ev :--> ( _ :==> pn ) ) = max 1 ( widthPN pn )
widthPN ( Alg.ZeroVector :--> ( _ :==> pn ) ) = max 1 ( widthPN pn )
widthPN ( Alg.Vector vector :--> ( _ :==> pn ) ) = max ( length vector ) ( widthPN pn )

-- back propogation
{-
type Await = Double
type Result = Double
educationPNSimple :: Double -> ( Await -> Result -> Double ) -> PerceptronNetwork -> PerceptronNetwork
educationPNSimple step wrong pn =
    let d'dx = Anl.differ step
    in pn
-}
