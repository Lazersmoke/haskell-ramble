-- Data decls could be in their own module for qualified import? 
-- Only thing to update to add fields is the accessor functions
-- In library

type Access part whole = (part -> part) -> whole -> (part, whole)

-- (a :: Access p w) (f :: p -> p) (w :: w) yields a pair of a certain "part" :: p of w changed by f and w, with that "part" changed by f

head' :: Access a [a]
head' f (x:xs) = (f x, f x : xs)

tail' :: Access [a] [a]
tail' f (x:xs) = (f xs, x : f xs)

grab :: Access part whole -> whole -> part
grab a w = fst $ a id w

change :: Access part whole -> (part -> part) -> whole -> whole
change a f w = snd $ a f w

changeMap :: Functor a => Access (a part) whole -> (part -> part) -> whole -> whole
changeMap a f w = snd $ a (fmap f) w

set :: Access part whole -> part -> whole -> whole
set a n w = snd $ a (const n) w

(~>>) :: Access p w -> w -> p
(~>>) = grab

(<<~) :: w -> Access p w -> p
(<<~) = flip (~>>)

(>@>) :: Access p w -> p -> w -> w
(>@>) = set

(>&>) :: Access p w -> (p -> p) -> w -> w
(>&>) = change
