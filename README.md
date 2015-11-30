# list-zip-def

Provides zip functions that use a given default value, whenever one list is exhausted, but not all.

    Prelude> import Data.List.Zip
    Prelude Data.List.Zip> zipDef 0 0 [1, 2, 3] [1]
    [(1,1)(2,0)(3,0)]

