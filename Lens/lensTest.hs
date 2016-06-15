{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Kitten = Kitten {_kittenName :: String, _kittenAge :: Int}

makeLenses ''Kitten

main = return ()
