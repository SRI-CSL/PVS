static int mod (unsigned int a, unsigned int b)
{
  while (a >= b) a -= b;
  return (int) a;
}

main ()
{
  unsigned int k, i;
  int u;

  k = 500000;

  for (i = 1; i <= 1000000; i++)
    u = (int) (k % i) /*mod (k,i)*/;
}
