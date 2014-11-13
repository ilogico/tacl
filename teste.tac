fun real sum(int x, int y) [
  ^ x + y;
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
  ^ n1;
]
proc main() [

]
var int x = factorial(3);
