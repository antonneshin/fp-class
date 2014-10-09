{-
  Все задачи в этом задании должны решаться исключительно с помощью свёрток.
  Явная рекурсия не допускается. Если в решении в качестве вспомогательной
  требуется стандартная функция обработки списков (помимо fold*, scan*), она
  также должна реализовываться свёрткой.

  Каждое решение должно сопровождаться тремя различными тестовыми примерами, которые при запуске
  возвращают True, например:

  f = undefined -- решение 
  f_test1 = f undefined == undefined -- тест 1
  f_test2 = f undefined == undefined -- тест 2
  f_test3 = f undefined == undefined -- тест 3
-}

{-
 1. Простейшие функции обработки списков
  a) Найти сумму чётных элементов списка с целочисленными элементами.
  b) Найти сумму и произведение элементов списка вещественных чисел.
  с) Найти среднее арифметическое элементов списка вещественных чисел (функцией length пользоваться нельзя,
     решение должно выполняться в один проход).
  d) Найти минимальный элемент списка.
  e) Найти наименьший нечётный элемент списка с целочисленными значениями (дополнительным параметром
     функции должно быть значение, возвращаемое по умолчанию).
-}

f1a :: Integral a => [a] -> a
f1a [] = 0
f1a xs = foldl (\x y -> x + if (y `mod` 2) == 0 then y else 0) 0 xs

f1a_test1 = f1a [] == 0
f1a_test2 = f1a [2] == 2
f1a_test3 = f1a [1..10] == 30


f1b :: [Integer] -> (Integer, Integer)
f1b [] = (0, 0)
f1b xs = foldl (\(x,y) z -> (x+z, y*z)) (0,1) xs

f1b_test1 = f1b [] == (0, 0)
f1b_test2 = f1b [1,2] == (3, 2)
f1b_test3 = f1b [1..10] == (55,3628800)


f1c :: Fractional b => [b] -> b
f1c [] = 0
f1c xs = (foldl (+) 0 xs)/(foldl (\x y -> x + 1  ) 0 xs)

f1c_test1 = f1c [] == 0
f1c_test2 = f1c [2] == 2.0
f1c_test3 = f1c [1,2,3] == 2.0


f1d :: Ord a => [a] -> a
f1d xs = foldl (\ x y -> if (x>y) then x else y) (head xs) xs

f1d_test1 = f1d [4] == 4
f1d_test2 = f1d [4, 5] == 5
f1d_test3 = f1d [6, 4, 5] == 6


f1e :: Integral a => [a] -> a
f1e xs = foldl (\ x y -> if (x<y) then x else y) (head $ filter (\x -> (x `mod` 2) /= 0) xs) $ filter (\x -> (x `mod` 2) /= 0) xs

f1e_test1 = f1e [1] == 1
f1e_test2 = f1e [2, 1] == 1
f1e_test3 = f1e [1,2,3,-1,5,6] == -1


{-
 2. Свёртки, формирующие списки
  a) Сформировать список, содержащий каждый второй элемент исходного.
  b) Сформировать список, содержащий первые n элементов исходного.
  c) Сформировать список, содержащий последние n элементов исходного.
  d) Сформировать список, содержащий все элементы исходного списка, большие левого соседа.
  e) Сформировать список, содержащий все локальные минимумы исходного списка.
  f) Дана строка, содержащая слова, разделённые одним или несколькими пробелами. Сформировать
     список слов этой строки.
  g) Разбить список на непересекающиеся подсписки длиной n элементов.
  h) Разбить список на подсписки длиной n элементов с перекрывающейся частью в k элементов (k < n).
  k) Сформировать список, содержащий все начальные элементы списка, удовлетворяющие заданному предикату.
  l) Повторить каждый элемент списка заданное количество раз.
  m) Удалить из списка повторяющиеся подряд идущие элементы.
  n) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
     заданной функции двух аргументов к соответствующим элементам исходных списков.
-}

--a) Сформировать список, содержащий каждый второй элемент исходного.
f2a :: [a] -> [a]
f2a xs = fst $ foldl ( \(list, cnt) x -> if (cnt `mod` 2)/=0 then ((list++[x]), cnt + 1) else (list, cnt + 1)) ([],0) xs

f2a_test1 = f2a [2] == []
f2a_test2 = f2a [1,2] == [2]
f2a_test3 = f2a [1..10] == [2,4,6,8,10]


--  b) Сформировать список, содержащий первые n элементов исходного.
f2b :: (Eq a1, Num a1) => a1 -> [a] -> [a]
f2b n xs = fst $ foldl(\(list, cnt) x -> if (cnt/=0) then ((list++[x]), cnt -1) else (list, 0)) ([], n) xs 

f2b_test1 = f2b 1 [1] == [1]
f2b_test2 = f2b 2 [1,3,2] == [1,3]
f2b_test3 = f2b 22 [1..10] == [1..10]


-- c) Сформировать список, содержащий последние n элементов исходного.
reverse' :: [a] -> [a]
reverse' = foldl(\list x -> (x:list)) []

f2c :: (Eq a1, Num a1) => a1 -> [a] -> [a]
f2c n xs = reverse' $ f2b n $ reverse' xs

f2c_test1 = f2c 1 [1] == [1]
f2c_test2 = f2c 2 [1,3,2] == [3,2]
f2c_test3 = f2c 22 [1..10] == [1..10]


--  d) Сформировать список, содержащий все элементы исходного списка, большие левого соседа.
f2d :: Ord a => [a] -> [a]
f2d (h:xs) =fst $ foldl (\(list, a) x-> if x>a then (list++[x], x) else (list, x) ) ([h], h) xs

f2d_test1 = f2d [1,2,3] == [1,2,3]
f2d_test2 = f2d [2,1,3] == [2,3]
f2d_test3 = f2d [3,2,1] == [3]


--  e) Сформировать список, содержащий все локальные минимумы исходного списка.
f2e :: Ord a => [a] -> [a]
f2e (a:[]) = [a]
f2e (a:b:xs) = fst $ foldl (\ (list, (a, b)) x -> (list ++ [min x (min a b)], (b, x)) ) ([min a b], (a, b)) xs

f2e_test1 = f2e [1,2] == [1]
f2e_test2 = f2e [1,2,1] == [1,1]
f2e_test3 = f2e [1,2,3,4] == [1,1,2]


--f) Дана строка, содержащая слова, разделённые одним или несколькими пробелами. Сформировать список слов этой строки.  
f_s :: ([[Char]], ([Char], Char)) -> Char -> ([[Char]], ([Char], Char))
f_s (list, (slovo, simvol)) x 
	| x/=' '  = (list, (slovo++[x],x))
	| x==' ' && simvol/=' ' = (list ++ [slovo], ([],x))
	| x==' ' && simvol==' ' = (list, ([],x))

f2f :: [Char] -> [[Char]]
f2f xs = fst $ foldl f_s ([],([], head xs)) (xs++[' '])

f2f_test1 = f2f "q" == ["q"]
f2f_test2 = f2f "     qqef    " == ["qqef"]
f2f_test3 = f2f "qwe     qqef   rew z" == ["qwe","qqef","rew","z"]


--g) Разбить список на непересекающиеся подсписки длиной n элементов.
take_list :: (Eq t, Num t) => ([[a]], ([a], t, t)) -> a -> ([[a]], ([a], t, t))
take_list (list,(word,cnt,cnt_const)) x
	| cnt/=0 = (list, (word++[x],cnt-1,cnt_const))
	| otherwise = (list++[word],([x],cnt_const-1,cnt_const))

f2g :: (Eq t, Num t) => t -> [b] -> [[b]]
f2g n xs = list ++ [ostatok]
  where
   (list,(ostatok,c,cc)) = foldl take_list ([], ([], n, n)) xs

f2g_test1 = f2g 30 [1]==[[1]]
f2g_test2 = f2g 2 [1,2,3]==[[1,2],[3]]
f2g_test3 = f2g 2 [1..10]==[[1,2],[3,4],[5,6],[7,8],[9,10]]

--h) Разбить список на подсписки длиной n элементов с перекрывающейся частью в k элементов (k < n).
drop' :: (Eq a, Num a) => a -> [a1] -> [a1]
drop' n xs = fst $ foldl (\ (list, nfl) x -> if (nfl==0) then (list++[x],0) else (list, nfl-1)) ([],n) xs

take_list' :: (Eq t, Num t) => ([[a]], ([a], t, t, t)) -> a -> ([[a]], ([a], t, t, t))
take_list' (list,(word,cnt,cnt_const,k)) x
	| cnt/=0 = (list, (word++[x],cnt-1,cnt_const,k))
	| otherwise = (list++[word],((drop' (cnt_const-k) word)++[x],cnt_const-1-k,cnt_const,k))

f2h :: (Eq t, Num t) => t -> t -> [b] -> [[b]] 	 
f2h n k xs = list ++ [ostatok]
  where
   (list,(ostatok,c,cc,ccc)) = foldl take_list' ([], ([], n, n,k)) xs

f2h_test1 = f2h 2 1 [1..10] == [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7],[7,8],[8,9],[9,10]]
f2h_test2 = f2h 4 3 [1..10] == [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7],[5,6,7,8],[6,7,8,9],[7,8,9,10]]
f2h_test3 = f2h 4 0 [1..10] == [[1,2,3,4],[5,6,7,8],[9,10]]


--  k) Сформировать список, содержащий все начальные элементы списка, удовлетворяющие заданному предикату.
f2k :: (a -> Bool) -> [a] -> [a]
f2k f xs = foldl (\ lst x -> if (f x) then (lst++[x]) else (lst)) [] xs 

f2k_test1 = f2k (<2) [-5..5] == [-5,-4,-3,-2,-1,0,1]
f2k_test2 = f2k (odd) [1..10] == [1,3,5,7,9]
f2k_test3 = f2k (`elem` [-5..5]) [-10..10] == [-5,-4,-3,-2,-1,0,1,2,3,4,5]


-- l) Повторить каждый элемент списка заданное количество раз.
f2l :: Int -> [a] -> [a]
f2l n xs = foldl (\ list x -> list ++ (replicate n x)) [] xs

f2l_test1 = f2l 1 [1..5] == [1,2,3,4,5]
f2l_test2 = f2l 0 [1..5] == []
f2l_test3 = f2l 3 [1..5] == [1,1,1,2,2,2,3,3,3,4,4,4,5,5,5]


--  m) Удалить из списка повторяющиеся подряд идущие элементы.
f2m :: Eq a => [a] -> [a]
f2m (g:xs) = fst $ foldl (\ (list, a) x -> if a==x then (list, a) else (list++[x],x)) ([g], g) xs 

f2m_test1 = f2m [1] == [1]
f2m_test2 = f2m [1,1,1] == [1]
f2m_test3 = f2m [1,1,1,2,2,1,1] == [1,2,1]


--  n) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
--     заданной функции двух аргументов к соответствующим элементам исходных списков.
f2n :: (a1 -> b -> a) -> [b] -> [a1] -> [a]
f2n f _ [] = []
f2n f xs ys = fst $ foldl (\ (list, cnt) x -> (list++[f (ys!!cnt) x ], cnt+1)) ([],0) xs

f2n_test1 = f2n (+) [1,2] [1,2] == [2,4]
f2n_test2 = f2n (-) [1,2] [1,2] == [0,0]
f2n_test3 = f2n (*) [1,2] [1,2] == [1,4]

{-
 3. Использование свёртки как носителя рекурсии (для запуска свёртки можно использовать список типа [1..n]).
  a) Найти сумму чисел от a до b.
  b) Найти сумму факториалов чисел от a до b (повторные вычисления факториалов не допускаются).
  с) Сформировать список из первых n чисел Фибоначчи.
  d) Пользуясь рядом Тейлора, вычислить значение синуса заданного числа x (использовать
     n слагаемых).
  e) Проверить, является ли заданное целое число простым.
-}

--a) Найти сумму чисел от a до b.
f3a :: (Enum b, Num b) => b -> b -> b
f3a a b = foldl (+) 0 [a..b]

f3a_test1 = f3a 1 1 == 1
f3a_test2 = f3a 1 3 == 6
f3a_test3 = f3a (-1) 1 == 0


--b) Найти сумму факториалов чисел от a до b (повторные вычисления факториалов не допускаются).
fact :: (Enum b, Eq b, Num b) => b -> b
fact 0 = 1
fact 1 = 1
fact a = foldl (*) 1 [1..a]

f3b :: (Enum a, Eq a, Num a) => a -> a -> a
f3b a b = fst $ foldl(\(sum_fact, fc) x -> (sum_fact+fc*x, fc*x)) (fact a, fact a) [a+1..b]

f3b_test1 = f3b 0 0 == 1
f3b_test2 = f3b 0 1 == 2
f3b_test3 = f3b 0 3 == 10


--с) Сформировать список из первых n чисел Фибоначчи.
f3c :: (Enum b, Eq b, Num b, Num a) => b -> [a]
f3c 0 = []
f3c 1 = [1]
f3c n = fst $ foldl (\(fibs,(pp,p)) x -> (fibs++[pp+p],(p, pp+p))) ([1,1],(1,1)) [1..n-2]

f3c_test1 = f3c 1 == [1]
f3c_test2 = f3c 3 == [1,1,2]
f3c_test3 = f3c 10 == [1,1,2,3,5,8,13,21,34,55]


--d) Пользуясь рядом Тейлора, вычислить значение синуса заданного числа x (использовать n слагаемых).
f3d :: (Enum a, Eq a, Fractional a) => a -> a -> a
f3d x n = fst $ foldl(\(r_t, (pp,param)) n -> (r_t + pp*param/(fact (2*n+1)),(pp*param, param))) (x, (x,-x*x)) [1..n]

f3d_test1 = f3d (pi/2) 40 == 1.0000000000000002
f3d_test2 = f3d (pi/2) 1 == 0.9248322292886504
f3d_test3 = f3d (0) 40 == 0.0


--e) Проверить, является ли заданное целое число простым.
f3e 1 = True
f3e n = (==2).snd $ foldl (\(chislo, cnt) a -> if (chislo `mod` a) == 0 then (chislo, cnt + 1) else (chislo, cnt)) (n, 0) [1..n]

f3e_test1 = f3e 1 == True
f3e_test2 = f3e 3 == True
f3e_test3 = f3e 128 == False


{-
 4. Решить задачу о поиске пути с максимальной суммой в треугольнике (см. лекцию 3) при условии,
   что необходимо дополнительно найти сам путь (к примеру, в виде закодированных направлений спуска:
   0 - влево, 1 - вправо). В решении допускается использование любых стандартных функций.
-}
{-
max' (x,xs) (y,ys) = if y>x then (y, xs++[1]) else (x, ys++[0])

downstep :: [Int] -> [Int] -> [Int]
downstep upper lower = zipWith (+) lower $ zipWith max (0:upper) (upper ++ [0])

answer :: [[Int]] -> Int
answer = maximum . foldl1 downstep
-}


{-
 5. Пусть числовые матрицы представлены списками строк. Реализовать следующие функции:
  1) транспонирование матрицы;
  2) сумма двух матриц;
  3) произведение двух матриц.
-}
{- начиная с этого места, решал эти задания, основываясь на работах других студентов -}
f51 :: [[a]] -> [[a]]
f51 matr = map (reverse') $ foldl (\ x y ->  zipWith (:) y x) (repeat []) matr


f51_test1 = f51 [[1]] == [[1]]
f51_test2 = f51 [[1,2],[3,4]] == [[1,3],[2,4]]
f51_test3 = f51 [[1,2],[3,4],[5,6],[7,8]] == [[1,3,5,7],[2,4,6,8]]



f52 :: [[Integer]] -> [[Integer]] -> [[Integer]]
f52 = zipWith (zipWith (+))

f52_test1 = f52 [[0]] [[1]] == [[1]]
f52_test2 = f52 [[1,2],[3,4]] [[1,2],[3,4]] == [[2,4],[6,8]]
f52_test3 = f52 [[1,2,3],[3,4,5]] [[1,2,3],[3,4,5]] == [[2,4,6],[6,8,10]]


f53 m1 m2 = foldr (\x matr -> (foldr (\y row -> (sum $ zipWith (*) x y) : row) [] (f51 m2)) : matr) [] m1
f53_test = f53 [[1,2],[3,4]] [[1,2],[3,4]] == [[7,10],[15,22]]
{-
 6. Реализовать левую свёртку, пользуясь правой. Проанализировать поведение собственной реализации
  на бесконечных списках и сравнить его с поведением оригинальной foldl.
-}

foldl' f c xs = foldr (flip f) c (reverse xs) 
