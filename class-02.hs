-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms s = (s `div` 3600, (s `mod` 3600) `div` 60, (s `mod` 3600) `mod` 60) 

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h*3600+m*60+s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = h*3600+m*60+s

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]
--получилось True!!

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

triangle :: Point -> Point -> Point -> (Double, Double)
triangle (x1, y1) (x2, y2) (x3, y3) = (a+b+c, 1/2*a*b)
  where
	a=distance (x1, y1) (x2, y2)
	b=distance (x2, y2) (x3, y3)
	c=distance (x1, y1) (x3, y3)

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) = if ((x `mod` 2) == 0) then ((nEven xs) +1) else (nEven xs)

	

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = [2*x] ++ (doubleElems xs)

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) = if ((x `mod` 2 ) /= 0) then ([x] ++ fltOdd xs) else fltOdd xs  

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
del_otr :: Integral a => [a] -> [a]
del_otr [] = []
del_otr (x:xs) = if (x >= 0) then ([x] ++ del_otr xs) else del_otr xs

-- б) увеличить элементы с чётными значениями в два раза;
d_ch :: Integral a => [a] -> [a]
d_ch [] = []
d_ch (x:xs) = if ((x `mod` 2 ) == 0) then ([2*x] ++ d_ch xs) else [x] ++ d_ch xs

-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
obmen :: Integral a => [a] -> [a]
obmen [] = []
obmen (x:[]) = []
obmen (x:y:xs) = [y, x] ++ obmen xs

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] [] = []
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = [x+y] ++ combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
double_list :: [Integer] -> [Integer] -> [ ( Integer, Integer ) ]
double_list [] [] = []
double_list [] ys = []
double_list xs [] = []
double_list (x:xs) (y:ys) = [(x,y)] ++ double_list xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
create_list_u:: Integer -> [Integer]
create_list_u 0 = []
create_list_u n = [n] ++ create_list_u (n-1)

-- б) в порядке возрастания.
create_list_v:: Integer -> [Integer]
create_list_v 0 = []
create_list_v n = create_list_v (n-1) ++ [n]

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insert_in_list:: Integer -> [Integer] -> [Integer]
insert_in_list a [] = []
insert_in_list a (x:[]) = [x]
insert_in_list a (x:xs) = [x,a] ++ insert_in_list a xs 

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).

rav :: [Integer] -> [Integer]
rav [] = []
rav (x:[]) = [x]
rav (x:xs) = rav' x xs
rav':: Integer -> [Integer] -> [Integer]
rav' x [] = [x]
rav' x (y:xs) 
	| (x == y) = [y] ++ rav' x xs
	| otherwise = [x]

nerav :: [Integer] -> [Integer]
nerav [] = []
nerav (x:[]) = []
nerav (x:xs) = nerav' x xs
nerav':: Integer -> [Integer] -> [Integer]
nerav' x [] = []
nerav' x (y:xs) 
	| (x == y) = nerav' x xs
	| otherwise = [y]++xs

divide_list:: [Integer] -> ([Integer], [Integer])
divide_list [] = ([], [])
divide_list xs = (rav xs , nerav xs)

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
list_e:: [a] -> Int -> a
list_e [] _ = error "pustoy spisok"
list_e (x:xs) n
	| n==0 = x
	| n > length xs = error "N bolshe dlini spiska! "
	| otherwise = list_e xs (n-1)


-- б) Eq a => [a] -> a -> Bool
srav:: Eq a => [a] -> a -> Bool
srav [] _ = False
srav (x:[]) a
	| x==a = True
	| otherwise = False
srav (x:xs) a  
	| x==a = srav xs a
	| otherwise = False

-- в) [a] -> Int -> [a]
list_n:: [a] -> Int -> [a]
list_n [] n = []
list_n xs 0 = []
list_n (x:[]) _ = [x]
list_n (x:xs) n = [x] ++ list_n xs (n-1) 

-- г) a -> Int -> [a]
create_list:: a -> Int -> [a]
create_list a 0 = []
create_list a n = [a] ++ create_list a (n-1)

-- д) [a] -> [a] -> [a]
skleyka:: [a] -> [a] -> [a]
skleyka [] [] = []
skleyka xs [] = xs
skleyka [] ys = ys
skleyka (x:xs) (y:ys) = [x, y] ++ skleyka xs ys

-- е) Eq a => [a] -> [[a]]
list_list:: Eq a => [a] -> [[a]]
list_list [] = []
list_list (x:[]) = [[x]]
list_list xs = [xrav xs] ++ list_list (drop (length (xrav xs)) xs)
xrav :: Eq a => [a] -> [a]
xrav (x:[]) = [x]
xrav (x:xs) = xrav' x xs
xrav':: Eq a => a -> [a] -> [a]
xrav' x [] = [x]
xrav' x (y:xs) 
	| (x == y) = [y] ++ xrav' x xs
	| otherwise = [x]

-- ж) [a] -> [(Int, a)]
cnt_e:: Eq a => a -> [a] -> Int
cnt_e a [] = 0
cnt_e a (x:xs)
	| (a==x) = 1 + cnt_e a xs
	| otherwise = cnt_e a xs

del_e:: Eq a => a -> [a] -> [a]
del_e a [] = []
del_e a (x:xs)
	| (a==x)= del_e a xs
	| otherwise = [x] ++ del_e a xs

count_elem:: Eq a => [a] -> [(Int, a)]
count_elem [] = []
count_elem (x:xs) = [ ( z, x ) ] ++ count_elem (del_e x xs)
	where 
		z=(cnt_e x (x:xs)) 

-- з) Eq a => [a] -> [a]
short_list:: Eq a => [a] -> [a]
short_list [] = []
short_list (x:[]) = [x]
short_list (x:xs) = [x] ++ short_list (del_e x xs)
