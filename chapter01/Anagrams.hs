import Pipe
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set

type Dictionary = String
type Wort = String
type Text = String
type Label = String

getWords :: Int -> Text -> [Wort]
getWords n text =  [word | word <- words (map Char.toLower text 
    $> filter (\ x -> Char.isLetter x || Char.isSeparator x)), length word == n]

addLabel :: Wort -> (Label, Wort)
addLabel word = (List.sort word, word)

sortLabels :: [(Label, Wort)] -> [(Label, Wort)]
sortLabels = List.sort

groupByLabel :: [(Label, Wort)] -> [(Label, [Wort])]
groupByLabel = groupByHelp Set.empty

groupByHelp :: Set.Set Label -> [(Label, Wort)] -> [(Label, [Wort])]
groupByHelp _ [] = []
groupByHelp set (x:xs) 
  | not (Set.member label set) =
      (label, wort:worte) : groupByHelp (Set.insert label set) xs
  | otherwise = groupByHelp set xs
  where 
      label = fst x
      wort  = snd x
      worte = filter (\ tuple -> fst tuple == label) xs $> map snd $> filter (/= wort)

showEntry :: [(Label, [Wort])] -> Dictionary
showEntry [] = []
showEntry (x:xs) = fst x ++ ": " ++ unwords (snd x) ++ "\n" ++ showEntry xs

anagrams :: Int -> Text -> Dictionary
anagrams n text = getWords n text
                $> map addLabel
                $> sortLabels
                $> groupByLabel
                $> showEntry

printAnagrams :: Int -> FilePath -> FilePath -> IO()
printAnagrams n infile outfile = do
  text <- readFile infile
  writeFile outfile (anagrams n text)
  putStrLn "anagrams done"



