int y;
int *g = &y;

void f(int *p) {
  *p = *g;
}

int main() {
  y = 16;
  y *= 10;
  f(&y);
  y = y / 4;
  return 0;
}
