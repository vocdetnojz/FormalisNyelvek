module Trie

import StdEnv, StdLib, GenEq

derive gEq Trie, Maybe

:: Trie v = T (Maybe v) [(Char, Trie v)]

emptyTrie :: Trie v
emptyTrie = T Nothing []

insertTrie :: String v (Trie v) -> (Trie v)
insertTrie "" v _ = T (Just v) []
insertTrie s v (T v1 l) = T v1 (f l)
  where
    f [] = [((hd (fromString s)), (T (Just v) []))]
    f [(c,t):cts] | c == key = [(c,insertTrie new_str v t):cts]
                  | c < key = [(c,t): f cts]
                  | c > key = [((hd (fromString s)), insertTrie new_str v t)]
    ss :: [Char]
    ss = tl (fromString s)
	new_str = toString ss
	key = hd (fromString s)


buildTrie :: [(String, v)] -> Trie v
buildTrie xs = abort "reference?"
buildTrie xs = foldr insertTrie emptyTrie xs


lookupTrie :: String (Trie v) -> Maybe v
lookupTrie a b = abort "undefined"

class Functor f where
  fmap :: (a -> b) (f a) -> (f b)

instance Functor Trie where
  fmap a b = abort "undefined"



test_insertTrie :: [Bool]
test_insertTrie =
    [ 
    insertTrie ""    0 emptyTrie	=== T (Just 0) []
    , insertTrie "a"   1 emptyTrie	=== T Nothing [('a', T (Just 1) [])]
    , insertTrie "b"   2 (insertTrie "a"   1 emptyTrie)	=== T Nothing [('a', T (Just 1) []), ('b', T (Just 2) [])]
    , insertTrie "a"   1 (insertTrie "b"   2 emptyTrie)	=== T Nothing [('a', T (Just 1) []), ('b', T (Just 2) [])]
    , insertTrie "ab"  3 (insertTrie "a"   1 emptyTrie)	=== T Nothing [('a', T (Just 1) [('b', T (Just 3) [])])]
    , insertTrie "abc" 4 emptyTrie	=== T Nothing [('a', T Nothing [('b', T Nothing [('c', T (Just 4) [])])])]
    ]

Start = insertTrie "a" 1 (insertTrie "b" 2 emptyTrie)// === T Nothing [('a', T (Just 1) []), ('b', T (Just 2) [])]
//Start = "N5XGDH"