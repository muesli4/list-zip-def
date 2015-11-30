# list-zip-def

Provides zip functions that use a given default value, whenever one list is exhausted, but not all.

    Prelude> import Data.List.Zip
    Prelude Data.List.Zip> zipDef 0 'x' [1, 2, 3] "a"
    [(1,'a')(2,'x')(3,'x')]

