module Data.List.Zip
    ( zipDefWith
    , zipDef
    , zipDefWith3
    , zipDef3
    , zipDefWith4
    , zipDef4
    , zipDefWith5
    , zipDef5
    , zipDefWith6
    , zipDef6
    , zipDefWith7
    , zipDef7
    ) where

-- | Combines all lists by applying the combining function using the given
-- defaults whenever a list is exhausted until the last list is empty.
--
-- For finite lists the following always holds:
--
-- > length (zipDefWith defX defY f xs ys) == max (length xs) (length ys)
--
-- and the missing tail will always be extended with the default value:
--
-- > drop (length xs) (zipDefWith defX defY f xs ys) == map (f defX) (drop (length xs) ys)
--
--
zipDefWith :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipDefWith ea eb comb = go
  where
    go as bs = case (as, bs) of
        ([], _)            -> map (comb ea) bs
        (_, [])            -> map (\a -> comb a eb) as
        (a : as', b : bs') -> comb a b : go as' bs'

prop_maxLength :: a -> b -> (a -> b -> c) -> [a] -> [b] -> Bool
prop_maxLength defX defY f xs ys = length (zipDefWith defX defY f xs ys) == max (length xs) (length ys)

prop_extend :: (Eq c) => a -> b -> (a -> b -> c) -> [a] -> [b] -> Bool
prop_extend defX defY f xs ys = drop (length xs) (zipDefWith defX defY f xs ys) == map (f defX) (drop (length xs) ys)

-- | Analogous to 'zip':
--
-- > zipDef defX defY = zipDefWith defX defY (,)
zipDef :: a -> b -> [a] -> [b] -> [(a, b)]
zipDef ea eb = zipDefWith ea eb (,)


zipDefWith3 :: a -> b -> c -> (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipDefWith3 ea eb ec comb = go
  where
    go as bs cs = case (as, bs, cs) of
        ([], _, _)                  -> zipDefWith eb ec (comb ea) bs cs
        (_, [], _)                  -> zipDefWith ea ec (\a -> comb a eb) as cs
        (_, _, [])                  -> zipDefWith ea eb (\a b -> comb a b ec) as bs
        (a : as', b : bs', c : cs') -> comb a b c : go as' bs' cs'

zipDef3 :: a -> b -> c -> [a] -> [b] -> [c] -> [(a, b, c)]
zipDef3 ea eb ec = zipDefWith3 ea eb ec (,,)

zipDefWith4 :: a -> b -> c -> d -> (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipDefWith4 ea eb ec ed comb = go
  where
    go as bs cs ds = case (as, bs, cs, ds) of
        ([], _, _, _)                        -> zipDefWith3 eb ec ed (comb ea) bs cs ds
        (_, [], _, _)                        -> zipDefWith3 ea ec ed (\a -> comb a eb) as cs ds
        (_, _, [], _)                        -> zipDefWith3 ea eb ed (\a b -> comb a b ec) as bs ds
        (_, _, _, [])                        -> zipDefWith3 ea eb ec (\a b c -> comb a b c ed) as bs cs
        (a : as', b : bs', c : cs', d : ds') -> comb a b c d : go as' bs' cs' ds'

zipDef4 :: a -> b -> c -> d -> [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zipDef4 ea eb ec ed = zipDefWith4 ea eb ec ed (,,,)

zipDefWith5 :: a -> b -> c -> d -> e
            -> (a -> b -> c -> d -> e -> f)
            -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipDefWith5 ea eb ec ed ee comb = go
  where
    go as bs cs ds es = case (as, bs, cs, ds, es) of
        ([], _, _, _, _)                              -> zipDefWith4 eb ec ed ee (comb ea) bs cs ds es
        (_, [], _, _, _)                              -> zipDefWith4 ea ec ed ee (\a -> comb a eb) as cs ds es
        (_, _, [], _, _)                              -> zipDefWith4 ea eb ed ee (\a b -> comb a b ec) as bs ds es
        (_, _, _, [], _)                              -> zipDefWith4 ea eb ec ee (\a b c -> comb a b c ed) as bs cs es
        (_, _, _, _, [])                              -> zipDefWith4 ea eb ec ed (\a b c d -> comb a b c d ee) as bs cs ds
        (a : as', b : bs', c : cs', d : ds', e : es') -> comb a b c d e : go as' bs' cs' ds' es'

zipDef5 :: a -> b -> c -> d -> e
        -> [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zipDef5 ea eb ec ed ee = zipDefWith5 ea eb ec ed ee (,,,,)

zipDefWith6 :: a -> b -> c -> d -> e -> f
            -> (a -> b -> c -> d -> e -> f -> g)
            -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipDefWith6 ea eb ec ed ee ef comb = go
  where
    go as bs cs ds es fs = case (as, bs, cs, ds, es, fs) of
        ([], _, _, _, _, _)                                    -> zipDefWith5 eb ec ed ee ef (comb ea) bs cs ds es fs
        (_, [], _, _, _, _)                                    -> zipDefWith5 ea ec ed ee ef (\a -> comb a eb) as cs ds es fs
        (_, _, [], _, _, _)                                    -> zipDefWith5 ea eb ed ee ef (\a b -> comb a b ec) as bs ds es fs
        (_, _, _, [], _, _)                                    -> zipDefWith5 ea eb ec ee ef (\a b c -> comb a b c ed) as bs cs es fs
        (_, _, _, _, [], _)                                    -> zipDefWith5 ea eb ec ed ef (\a b c d -> comb a b c d ee) as bs cs ds fs
        (_, _, _, _, _, [])                                    -> zipDefWith5 ea eb ec ed ee (\a b c d e -> comb a b c d e ef) as bs cs ds es
        (a : as', b : bs', c : cs', d : ds', e : es', f : fs') -> comb a b c d e f : go as' bs' cs' ds' es' fs'

zipDef6 :: a -> b -> c -> d -> e -> f
        -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zipDef6 ea eb ec ed ee ef = zipDefWith6 ea eb ec ed ee ef (,,,,,)

zipDefWith7 :: a -> b -> c -> d -> e -> f -> g
            -> (a -> b -> c -> d -> e -> f -> g -> h)
            -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
zipDefWith7 ea eb ec ed ee ef eg comb = go
  where
    go as bs cs ds es fs gs = case (as, bs, cs, ds, es, fs, gs) of
        ([], _, _, _, _, _, _)                                          -> zipDefWith6 eb ec ed ee ef eg (comb ea) bs cs ds es fs gs
        (_, [], _, _, _, _, _)                                          -> zipDefWith6 ea ec ed ee ef eg (\a -> comb a eb) as cs ds es fs gs
        (_, _, [], _, _, _, _)                                          -> zipDefWith6 ea eb ed ee ef eg (\a b -> comb a b ec) as bs ds es fs gs
        (_, _, _, [], _, _, _)                                          -> zipDefWith6 ea eb ec ee ef eg (\a b c -> comb a b c ed) as bs cs es fs gs
        (_, _, _, _, [], _, _)                                          -> zipDefWith6 ea eb ec ed ef eg (\a b c d -> comb a b c d ee) as bs cs ds fs gs
        (_, _, _, _, _, [], _)                                          -> zipDefWith6 ea eb ec ed ee eg (\a b c d e -> comb a b c d e ef) as bs cs ds es gs
        (_, _, _, _, _, _, [])                                          -> zipDefWith6 ea eb ec ed ee ef (\a b c d e f -> comb a b c d e f eg) as bs cs ds es fs
        (a : as', b : bs', c : cs', d : ds', e : es', f : fs', g : gs') -> comb a b c d e f g : go as' bs' cs' ds' es' fs' gs'

zipDef7 :: a -> b -> c -> d -> e -> f -> g
        -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
zipDef7 ea eb ec ed ee ef eg = zipDefWith7 ea eb ec ed ee ef eg (,,,,,,)

