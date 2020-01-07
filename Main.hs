newtype Vector
  = Vector [Double]
    deriving Eq
newtype Matrix
  = Matrix [Vector]
    deriving Eq

instance Show Vector where
  show (Vector [])
    = "[-]"
  show (Vector xs)
    = unlines $ pad $ map show xs
      where
        pad :: [String] -> [String]
        pad items
          = map (take width . (++ repeat ' ')) items
            where
              width = maximum $ map length items

instance Show Matrix where
  show (Matrix [])
    = "[[-]]"
  show matrix@(Matrix columns)
    = unlines $ map unwords rows
      where
        showed = map (lines . show) columns
        (m, _) = dimensions matrix
        rows   = [map (!! x) showed | x <- [0..m - 1]]

v1 = Vector [1..4]
v2 = Vector [4,3..1]
m1 = Matrix [v1, v2]
m2 = transpose m1
m3 = Matrix [ Vector [10, 2]
            , Vector [4, 2] ]
m4 = Matrix [v2, v1]
m5 = Matrix [v1,v2,v2,v1]
m6 = Matrix [ Vector [1,2,4,6]
            , Vector [2,3,4,5]
            , Vector [7,4,3,1]
            , Vector [9,3,7,4]
            ]

vecAdd :: Vector -> Vector -> Vector
vecAdd (Vector xs) (Vector ys)
  = Vector $ zipWith (+) xs ys

vecSub :: Vector -> Vector -> Vector
vecSub (Vector xs) (Vector ys)
  = Vector $ zipWith (-) xs ys

vecDot :: Vector -> Vector -> Double
vecDot (Vector xs) (Vector ys)
  = sum $ zipWith (*) xs ys

vecScale :: Vector -> Double -> Vector
vecScale (Vector xs) scalar
  = Vector (map (* scalar) xs)

vecIndex :: Vector -> Int -> Double
vecIndex (Vector xs) i
  = xs !! i

vecExtend :: Vector -> Vector
vecExtend (Vector xs)
  = Vector (0 : xs)

vecShrink :: Vector -> Vector
vecShrink vector@(Vector [_])
  = vector
vecShrink (Vector (_ : xs))
  = Vector xs

dimension :: Vector -> Int
dimension (Vector xs)
  = length xs

transpose :: Matrix -> Matrix
transpose (Matrix columns)
  = Matrix [getRow n | n <- [0..pred $ dimension (columns !! 0)]]
    where
      getRow :: Int -> Vector
      getRow n
        = Vector [column !! n | (Vector column) <- columns]

matAdd :: Matrix -> Matrix -> Matrix
matAdd (Matrix columns1) (Matrix columns2)
  = Matrix $ map (uncurry vecAdd) $ zip columns1 columns2

matSub :: Matrix -> Matrix -> Matrix
matSub (Matrix columns1) (Matrix columns2)
  = Matrix $ map (uncurry vecSub) $ zip columns1 columns2

matMulVec :: Matrix -> Vector -> Vector
matMulVec matrix vector
  = Vector $ map (vecDot vector) rows
    where
      (Matrix rows) = transpose matrix

matMul :: Matrix -> Matrix -> Matrix
matMul matrix (Matrix columns)
  = Matrix $ map (matMulVec matrix) columns

identity :: Int -> Matrix
identity size
  = Matrix $ map createColumn [0..size - 1]
    where
      createColumn :: Int -> Vector
      createColumn pos
        = Vector $ (replicate pos 0) ++ 1 : replicate (size - pos - 1) 0

dimensions :: Matrix -> (Int, Int)
dimensions (Matrix columns@(column : _))
  = (dimension column, length columns)

-- sorts column vectors of a matrix by sorting them as lists
sortColumns :: Matrix -> Matrix
sortColumns (Matrix [])
  = Matrix []
sortColumns (Matrix columns)
  = Matrix $ sortedColumn : remaining
    where
      sortedColumn        = Vector $ minimum $ map (\(Vector xs) -> xs) columns
      (before, _ : after) = span (/= sortedColumn) columns
      (Matrix remaining)  = sortColumns $ Matrix $ before ++ after

-- to Row Echelon Form
toREF :: Matrix -> Matrix
toREF
  = transpose . toREF' . transpose
    where
      toREF' :: Matrix -> Matrix
      toREF' (Matrix [])
        = Matrix []
      toREF' matrix@(Matrix [_])
        = matrix
      toREF' matrix
        = Matrix $ row : map vecExtend remainingRows
          where
            (Matrix (row : rows))
              = reduce matrix
            (Matrix remainingRows)
              = toREF' $ Matrix $ map vecShrink rows
            reduce :: Matrix -> Matrix
            reduce (Matrix (row : rows))
              = Matrix $ row : map (reduceRow row) rows
            reduceRow :: Vector -> Vector -> Vector
            reduceRow subtrahend@(Vector (x : _)) minuend@(Vector (y : _))
              = (vecScale minuend x) `vecSub` (vecScale subtrahend y)

-- to Reduced Row Echelon Form
toRREF :: Matrix -> Matrix
toRREF
  = undefined
