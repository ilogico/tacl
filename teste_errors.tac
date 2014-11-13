fun int main(int x)[
  var int y = 59;
  var int i = 0;
  while(i < 30)[
    print coisas(96, 98, y);
    i = i + 5;
  ]
  ^ cenas();
]

proc print_stuff(int n)[
  while (n > 0)[
    print n;
    n = n - 1;
  ]
]
fun int coisas(int b)[
  ^ if (b == 3) 2 else 5;
]

fun int fact(int n) = if (n < 2) 1 else n * fact(n-1);
