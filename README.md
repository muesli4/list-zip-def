# list-zip-def

Provides zip functions that use a given default value, whenever one list is exhausted, but not all.

    Prelude> import Data.List.Zip
    Prelude Data.List.Zip> zipDef 0 'x' [1, 2, 3] "a"
    [(1,'a')(2,'x')(3,'x')]

The resulting list spine is also lazy:

    Prelude Data.List.Zip> let xs = zipDef 0 'x' [1, 2, 3 :: Int] "a"
    Prelude Data.List.Zip> :sprint xs
    xs = _
    Prelude Data.List.Zip> import Data.Maybe
    Prelude Data.List.Zip Data.Maybe> listToMaybe xs 
    Just (1,'a')
    Prelude Data.List.Zip Data.Maybe> :sprint xs
    xs = (1,'a') : _
