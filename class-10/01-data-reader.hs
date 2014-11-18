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
import System.Environment
import Control.Applicative

type Name = String
type Age = Int
type Ngr = String
data Stud = Stud Name Age Ngr deriving (Show)

makeList :: FilePath -> IO [Stud]
makeList fname = readFile fname >>= (\ cont -> return $ foldl (\ ls a -> let [n,age,g]=(words a) in ls++[Stud n (read age::Int) g] ) [] (lines cont))

myAction :: [FilePath] -> IO [Stud]
myAction ls = (++) <$> (makeList $ head ls) <*> (makeList $ last ls)



main = getArgs>>=(\ ls -> myAction ls)

