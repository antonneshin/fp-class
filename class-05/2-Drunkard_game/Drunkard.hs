{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades | Clubs | Diamonds | Hearts 
		deriving (Show, Eq, Ord)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
		| Jack | Queen | King | Ace 
		deriving (Show, Eq, Ord) 

data Card = Card Value Suit
		deriving (Show, Eq, Ord)

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ a ) (Card _ b) = a==b

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

beats :: Card -> Card -> Ordering
c1 `beats` c2 = compare c1 c2

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round (xs, ys) = foldl(\ (a, b) _ -> my_round (a, b) ) (xs, ys) [1..n]
 where
  n = length xs


my_round :: Ord a => ([a], [a]) -> ([a], [a])
my_round (xs, ys)
  | (compare h1 h2) == LT = (drop 1 xs, (drop 1 ys) ++ [h2] ++ [h1] )
  | (compare h1 h2) == GT = ((drop 1 xs) ++ [h1] ++ [h2], drop 1 ys  )
  | otherwise = (drop 1 xs, drop 1 ys)
  where 
    h1 = head xs	
    h2 = head ys

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second deriving (Show)

game :: ([Card], [Card]) -> (Winner, Int)
game (xs, ys) 
  | flag == 1 = (First, res)
  | otherwise = (Second, res)
  where
    ls = game_cnt(xs, ys)
    res = (length ls) - 1 
    flag = head $ reverse ls

game_cnt :: Num t => ([Card], [Card]) -> [t]
game_cnt ([], ys) = [-1]
game_cnt (xs, []) = [1]
game_cnt (xs, ys) = [0] ++ game_cnt (game_round (xs, ys)) 

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}
--Spades | Clubs | Diamonds | Hearts
game_test1 = game ([Card Ten Diamonds,Card Seven Clubs, Card Queen Spades, Card Five Hearts, Card King Spades, Card Four Clubs, Card Two Diamonds, Card Three Hearts, Card Jack Spades],[Card Eight Clubs,Card Ace Clubs, Card Nine Diamonds, Card Queen Hearts, Card King Clubs, Card Two Clubs, Card Three Spades, Card Jack Hearts, Card Six Spades, Card Jack Clubs])

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
