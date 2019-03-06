distributivity
    :: Either a (b, c)
    -> (Either a b, Either a c)
distributivity = undefined

associator
    :: (a, (b, c))
    -> ((a, b), c)
associator = undefined

{-# LANGUAGE TypeOperators #-}

type (<->) a b = (a -> b, b -> a)

eitherAssoc
    :: Either a (Either b c)
    <-> Either (Either a b) c
eitherAssoc = undefined
