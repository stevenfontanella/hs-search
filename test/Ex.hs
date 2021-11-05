data A = B Int | C String

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n - 2)
