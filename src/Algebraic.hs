{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Algebraic where

---------------------- type vector, matrix--------------------------
data Vector a = Vector [ a ] | ZeroVector | Ev deriving ( Show, Eq )

-- Vectors in context of Matrix are columns of Matrix
data Matrix a = Matrix [ Vector a ] | ZeroMatrix | E deriving ( Show, Eq ) -- columns!

----------------------------important--------------------------------
allSame :: Eq a => [ a ] -> Bool
allSame [] = True
allSame [ x ] = True
allSame ( x : x' : xs ) = ( x == x' ) && allSame xs

--------------------------features-----------------------------------
-- scale prod
scale :: ( Eq a, Ring a ) => Vector a -> Vector a -> a
scale Ev Ev = error "a lot of evals"
scale Ev ( Vector a ) = mconcat a
scale a Ev = scale Ev a
scale ZeroVector _ = e
scale _ ZeroVector = e
scale a b = 
    let Vector vector = a |> b
    in mconcat vector

-- matrix line to vector conversion
toVector :: Matrix a -> Vector a
toVector E = Ev
toVector ZeroMatrix = ZeroVector
toVector ( Matrix columns )
    | and $ ( \( Vector vector ) -> length vector /= 1 ) <$> columns = error "wrong structure"
    | otherwise = Vector $ ( \( Vector ( x : xs ) ) -> x ) <$> columns

-- vector to matrics columns
toMatrix :: Vector a -> Matrix a
toMatrix Ev = E
toMatrix ZeroVector = ZeroMatrix
toMatrix ( Vector vector ) = Matrix $ ( \a -> Vector [ a ] ) <$> vector

---------------------- important classes----------------------------
class Monoid a => Group a where
    e :: a
    e = mempty
    
    neg :: a -> a

class Group a => Ring a where
    e' :: a
    ( |> ) :: a -> a -> a
    infixr 7 |>

    -- more than <>
    ( <|> ) :: a -> a -> a
    a <|> b = b <|> a

-- an action of group on a set
class Group g => Action g s | s -> g where
    ( <:> ) :: g -> s -> s

---------------------instances------------------------------------------
instance Functor Vector where
    fmap :: (a -> b) -> Vector a -> Vector b
    fmap f Ev = Ev
    fmap f ZeroVector = ZeroVector
    fmap f ( Vector vector ) = Vector $ f <$> vector

instance Functor Matrix where
    fmap :: ( a -> b ) -> Matrix a -> Matrix b
    fmap f E = E
    fmap f ZeroMatrix = ZeroMatrix
    fmap f ( Matrix matrix ) = Matrix $ fmap ( fmap f )  matrix

-- note: vector space is a group, because for all v from V have v' | v * v' = 0v
-- note: vector space with B ( V ) = measure of V is a Semigroup with partial summing
instance ( Ring a, Eq a ) => Semigroup ( Vector a ) where
    ( <> ) :: Group a => Vector a -> Vector a -> Vector a
    Ev <> Ev = Vector $ repeat $ e' <|> e'
    Vector a <> Ev = Vector $ ( e' <|> ) <$> a
    Ev <> Vector a = Vector a <> Ev
    a <> ZeroVector = a
    ZeroVector <> a = a
    Vector a <> Vector b
        | length a == length b || a == repeat ( e' <> e' ) || b == repeat ( e' <> e' ) = Vector $ zipWith ( <> ) a b
        | otherwise = error "unequal vectors"

instance ( Ring a, Eq a ) => Monoid ( Vector a ) where
    mempty :: Group a => Vector a
    mempty = ZeroVector

instance ( Ring a, Eq a ) => Group ( Vector a ) where
    e :: ( Ring a, Eq a ) => Vector a
    e = mempty

    neg :: Group a => Vector a -> Vector a
    neg ZeroVector = ZeroVector
    neg Ev = Vector $ repeat $ neg e'
    neg ( Vector vector ) = Vector $ neg <$> vector

instance ( Ring a, Eq a ) => Ring ( Vector a ) where
    e' :: Ring a => Vector a
    e' = Ev

    (<|>) :: Ring a => Vector a -> Vector a -> Vector a
    ( <|> ) = ( <> )

    (|>) :: Ring a => Vector a -> Vector a -> Vector a
    _ |> ZeroVector = ZeroVector
    ZeroVector |> _ = ZeroVector
    Ev |> a = a
    a |> Ev = a
    Vector a |> Vector b
        | length a == length b || a == repeat ( e' <> e' ) || b == repeat ( e' <> e' ) = Vector $ zipWith ( |> ) a b
        | otherwise = error "unequal vectors"

-- we can define action of group Vector on set of Matrix
-- we do not have a Matrix space like a semigroup here
instance ( Ring a, Eq a ) => Action ( Vector a ) ( Matrix a ) where
    (<:>) :: ( Ring a, Eq a ) => Vector a -> Matrix a -> Matrix a
    Ev <:> E = E
    ZeroVector <:> _ = ZeroMatrix
    Ev <:> Matrix columns
        | allSame $ ( \( Vector column ) -> length column ) <$> columns =
            Matrix $ ( \(Vector column) -> Vector [ mconcat column ] ) <$> columns
        | otherwise = error "unequal columns"
    v@( Vector vector ) <:> Matrix columns
        | and $ ( == length vector ) <$>
            ( ( \( Vector column ) -> length column ) <$> columns ) = 
                Matrix $ ( \column -> Vector [ scale v column ] ) <$> columns
        | otherwise = error "unequal vectors"
    _ <:> ZeroMatrix = ZeroMatrix
    Vector vector <:> E = Matrix $ ( \o -> Vector [ o ] ) <$> vector

-- let's define Double like a Ring
instance Semigroup Double where
    (<>) :: Double -> Double -> Double
    ( <> ) = ( + )

instance Monoid Double where
    mempty :: Double
    mempty = 0

instance Group Double where
    neg :: Double -> Double
    neg = ( e - )

instance Ring Double where
    e' :: Double
    e' = 1

    (|>) :: Double -> Double -> Double
    ( |> ) = ( * )

    (<|>) :: Double -> Double -> Double
    ( <|> ) = ( <> )
