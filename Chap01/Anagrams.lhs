\documentclass{article}
\usepackage{amssymb,amsmath}
%include polycode.fmt

\title{Converting Anagrams}
\author{Oliver Krischer}
\date{September 2020}

\begin{document}
\maketitle

\section{Problem Description}
Write a function that creates a printout of anagrams of all the words
with a given length in a text with the following specification:

\begin{spec}
anagrams :: Int -> Text -> Dictionary
\end{spec}

\section{solution}

\begin{code}
module Anagrams where
import Data.Char (toLower)
import Data.List (sort)
import qualified Data.Set as Set
import Flow
\end{code}

\begin{code}
type Dictionary = String
type Wort = String
type Text = String
type Label = String
\end{code}

\begin{code}
anagrams :: Int -> Text -> Dictionary
anagrams n text = getWords n text
                |> map addLabel
                |> sortLabels
                |> groupByLabel
                |> showEntry
\end{code}

extract the words of length n
\begin{code}
getWords :: Int -> Text -> [Wort]
getWords n text =  [word | word <- words (map toLower text), length word == n]
\end{code}

take each word and add a label to it
\begin{code}
addLabel :: Wort -> (Label, Wort)
addLabel word = (sort word, word)
\end{code}

sort list of label-word-tuples in aplphabetical order
\begin{code}
sortLabels :: [(Label, Wort)] -> [(Label, Wort)]
sortLabels xs = sort xs
\end{code}

replace each group of labelled words with the same label
with a single entry using an accumulator (Set.empty) and a helper function
\begin{code}
groupByLabel :: [(Label, Wort)] -> [(Label, [Wort])]
groupByLabel xs = groupByHelp Set.empty xs

groupByHelp :: Set.Set Label -> [(Label, Wort)] -> [(Label, [Wort])]
groupByHelp _ [] = []
groupByHelp set xs 
  | not (Set.member label set) = 
      (label, [word | (label', word) <- xs, label == label'])
      : groupByHelp (Set.insert label set) (tail xs)
  | otherwise = groupByHelp set (tail xs)
  where label = fst (head xs)
\end{code}

replace each enty by a string and concatenate the results
\begin{code}
showEntry :: [(Label, [Wort])] -> Dictionary
showEntry [] = []
showEntry (x:xs) = fst x ++ ": " ++ unwords (snd x) ++ "\n" ++ showEntry xs
\end{code}

\end{document}