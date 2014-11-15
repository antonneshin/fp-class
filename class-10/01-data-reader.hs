{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}
import System.IO
import Control.Monad
import System.Environment

type Name = String
type Age = Int
type Groupe = String

data Stud = Stud Name Age Groupe deriving (Show)

main = do
  [name] <- getArgs
  cont <- readFile name
  print $ foldl (\ ls a -> ls++(show a)++"\n") [] $ foldl (\ ls a -> let [n,a,g] = words a in ls++[Stud n (read a::Int) g]) [] (lines cont)
