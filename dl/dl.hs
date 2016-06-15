type Operation = MonoVal -> MonoVal
type MonoVal = Double
data MonoExp = MonoExp {base :: MonoVal, opList :: [Operation]}

evalMono :: MonoExp -> MonoVal
evalMono (MonoExp a []) = id a
evalMono (MonoExp base (op:ops)) = evalMono $ MonoExp (op base) ops

-- "a\b"
monoLog :: MonoVal -> MonoVal -> MonoVal
a `monoLog` b = logBase b a

-- "a/b"
monoPow :: MonoVal -> MonoVal -> MonoVal
a `monoPow` b = a ** b

monoAdd :: MonoVal -> MonoVal -> MonoVal
a `monoAdd` b = a + b

monoMul :: MonoVal -> MonoVal -> MonoVal
x `monoMul` y = x * y

-- 1101
-- 0101

-- 1101
