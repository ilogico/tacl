fun real sum(int x, int y) [
  ^ x + y
]

fun int factorial(int n) = if (n < 2) 1 else n*factorial(n-1);

fun int fibo(int n) [
  var int n0 = 0;
  var int n1 = 1;
  var int temp;
  var int i = 0;
  while (i < n)[
    temp = n1;
    n1 = n1 + n0;
    n0 = temp;
  ]
  ^ n1
]

var int x = factorial(3);

fun real g(int n) [
  var real s = 0;
  while (n >= 1) [
    s = s + n;
    n = n - 1;
  ]
  ^ s
]

fun int sub(int i, int j) = i - j;
fun int fibonacci(int n)
[
  var int r;
  if (n == 0 || n == 1)
    r = n;
  else
    r = fibonacci(n - 1) + fibonacci(sub(n, 2));
  ^ r
]

var int n = 26;
fun int main()
[
  var int fib = fibonacci(n);
  # all three should print the same
  print fib;
  print fibonacci(14 + 3 * 4);
  print 121393;
  ^ 0 # return 0 to the system
]
