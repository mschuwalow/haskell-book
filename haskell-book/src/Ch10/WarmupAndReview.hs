module Ch10.WarmupAndReview where

stops = "pbtdkg"
vowels = "aeiou"

-- 1a
stopVowelStopCombos =
  [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

-- 1b
stopVowelStopCombos' =
  filter f stopVowelStopCombos
  where
    f ('p', _, _) = True
    f _ = False

-- 1c
nouns = ["people", "history", "way", "art", "world"]
verbs = ["be", "have", "do", "say", "make", "go", "take", "come", "see"]

sentences = [(n1,v,n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

--3
secFunc x =
  fromIntegral (sum (map length (words x))) / (fromIntegral (length (words x)))
