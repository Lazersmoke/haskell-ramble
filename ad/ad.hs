
--f(x + h) - f(x)
----------------
--      h
derivate :: (Double -> Double) -> Double -> Double
derivate func x = limit (\h -> (func (x + h) - func x) / h) 0

limit :: (Double -> Double) -> Double -> Double
limit func to = map (func . (+to)) limitVals !! 100

limitVals :: [Double]
limitVals = map (\x -> 2**(-x)) [0..]

