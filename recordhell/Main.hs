

data Test = Test
  Int
  String

onFirst :: Test -> (Int -> Int) -> (Int, Test)
onFirst test f = (f (firstField test), test {firstField = f . firstField . test})

onFirst :: Test -> (Int -> Int) -> (Int, Test)
onFirst (Test old _) f = f old
