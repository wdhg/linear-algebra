newtype Vector
  = Vector [Double]
    deriving Eq
newtype Matrix
  = Matrix [Vector]
    deriving Eq

instance Show Vector where
  show (Vector xs)
    = unlines $ pad $ map show xs
      where
        pad :: [String] -> [String]
        pad items
          = map (take width . (++ repeat ' ')) items
            where
              width = maximum $ map length items

instance Show Matrix where
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

vecAdd :: Vector -> Vector -> Vector
vecAdd (Vector xs) (Vector ys)
  = Vector $ zipWith (+) xs ys

vecSub :: Vector -> Vector -> Vector
vecSub (Vector xs) (Vector ys)
  = Vector $ zipWith (-) xs ys

vecDot :: Vector -> Vector -> Double
vecDot (Vector xs) (Vector ys)
  = sum $ zipWith (*) xs ys

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

sortRows :: Matrix -> Matrix
sortRows matrix
  = transpose $ sortRows' $ transpose matrix
    where
      sortRows' :: Matrix -> Matrix
      sortRows' (Matrix [])
        = Matrix []
      sortRows' (Matrix rows)
        = Matrix (row : rows')
          where
            row            = Vector $ minimum $ map (\(Vector xs) -> xs) rows
            (Matrix rows') = sortRows' $ Matrix $ filter (/= row) rows

-- to Row Echelon Form
toREF :: Matrix -> Matrix
toREF matrix
  = undefined
    where
      matrixT = transpose matrix
      reduceRow :: Vector -> Int -> Vector -> Vector
      reduceRow
        = undefined


-- to Reduced Row Echelon Form
toRREF :: Matrix -> Matrix
toRREF
  = undefined
