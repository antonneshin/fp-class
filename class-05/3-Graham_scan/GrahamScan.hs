{-# LANGUAGE EmptyDataDecls #-}

import Data.List

--module GrahamScan where

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point (Double, Double) deriving(Show, Ord, Eq)

minus :: Point -> Point -> Point
Point (ax, ay) `minus` Point (bx, by) = Point (ax-bx, ay-by)


plus :: Point -> Point -> Point
Point (ax, ay) `plus` Point (bx, by) = Point (ax+bx, ay+by)

  
{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = GoLeft | GoRight | OneLine deriving (Show, Eq, Ord)


ugol :: (Floating a, Ord a) => a -> a -> a
ugol x y 
  | (x>0)&&(y>=0) = atan(y/x)
  | (x>0)&&(y<0) = atan(y/x) + 2*pi
  | (x<0) = atan(y/x) + pi
  | (x==0)&&(y>0) = pi/2
  | (x==0)&&(y<0) = -pi/2
  | (x==0)&&(y==0) = 0


getx :: Point -> Double
getx (Point (a, _)) = a

gety :: Point -> Double
gety (Point (_, b)) = b

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

min_p :: [Point] -> Point
min_p (h:xs) = foldl(\ m b -> if (gety m) > (gety b) then b else ( if (gety m)==(gety b) then ( if (getx m) > (getx b) then b else m ) else m ) ) h xs


polyar_u :: [Point] -> [(Double, Point)]
polyar_u xs = map (\ h -> (ugol (getx (h)) (gety (h)), h) ) xs



sortPoint :: [Point] -> [Point]
sortPoint xs = map (snd) $ sort $ polyar_u xs


onestep :: ([Point], (Point, Point)) -> Point -> ([Point], (Point, Point))
onestep (ls, (pr, t) ) x
  | (direct pr t x) == GoLeft = (ls++[x],(t,x))
  | ((direct pr t x) == GoRight) || ((direct pr t x) == OneLine) = (ch_ls++[x], (pr, x))
  where
    ch_ls = take ((length ls)-1) ls

graham_scan :: [Point] -> [Point]
graham_scan xs = map (`plus` min_point) $ fst $ foldl (onestep) ([Point (0,0),golova],(Point (0,0),golova)) (spisok)
  where
    min_point = min_p xs 
    sp =map (`minus` min_point) $ delete min_point xs
    golova = head sp
    spisok = delete golova sp




{-
  5. Приведите несколько примеров работы функции graham_scan.
-}

test1 = graham_scan [Point (-2,-2), Point (-2,2), Point (2,-2), Point (2,2), Point (0,-3)]

test2 = drop 1 $ graham_scan $ zipWith (\x y -> Point (x, y)) [0, 0, 0, 1, 2, 2, 3, 5, 5, 3, 0, 2, 2, 3] [0, 0, 1, 0, 0, 0, 0, 3, 3, 5, 2, 2, 3, 2] 
