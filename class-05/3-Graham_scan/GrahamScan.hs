{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point (Double, Double) deriving(Show, Ord, Eq)
  
{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = GoLeft | GoRight | OneLine deriving (Show)


ugol :: (Floating a, Ord a) => a -> a -> a
ugol x y 
  | (x>0)&&(y>=0) = atan(y/x)
  | (x>0)&&(y<0) = atan(y/x) + 2*pi
  | (x<0) = atan(y/x) + pi
  | (x==0)&&(y>0) = pi/2
  | (x==0)&&(y<0) = -pi/2
  | (x==0)&&(y==0) = 0


direct :: Point -> Point -> Point -> Direction
direct (Point (ax, ay)) (Point (bx,by)) (Point (cx,cy))
  | vb > vc = GoRight
  | vb < vc = GoLeft
  | otherwise = OneLine
  where
	vb = ugol (bx-ax) (by-ay)
	vc = ugol (cx-ax) (cy-ay)

{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

directions :: [Point] -> [Direction]
directions (a:b:[]) = []
directions (a:b:c:xs) = [direct a b c] ++ directions (b:c:xs)

{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

graham_scan :: [Point] -> [Point]
graham_scan = undefined

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}
