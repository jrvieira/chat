module Config where

import Data.Text ( Text )

-- CONFIG

address :: String
address = "0.0.0.0"

port :: Int
port = 3000

anon :: Text
anon = "_"

commchar :: Char
commchar = ':'

freechar :: Char
freechar = ' '

privchar :: Char
privchar = '@'

chanchar :: Char
chanchar = '#'

