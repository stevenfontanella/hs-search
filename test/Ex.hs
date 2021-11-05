data A = B Int | C String

data Rec = Rec {
  aField :: Int
}

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n - 2)
