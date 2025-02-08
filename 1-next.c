int
fib(int x) {
  if (x<=1)
    return 1;
  return
      fib(x-1)
      +
      fib(x-2);
}

int ASSERT(int a, int b) {
  return a == b ;
}
int g1, g2[4];
int main() {

    fib(3);
    ASSERT(3, ({
                int x=3;
                *&x;
    }));
    return 0;
}
