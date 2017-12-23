#include <stdio.h>

#define DEBUG 1
#define PROBLEM2 1

int is_prime(int n) {
  if(n == 0 || n == 1) return 0;
  if(n % 2 == 0) return 0;
  int half = n / 2;
  for(int i = 3; i < half; i += 2) {
    if(n % i == 0) return 0;
  }
  return 1;
}

int main() {
  // int a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0;
  int a = 1, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0;
  int mulcount = 0;
  int pc = 0;
  void *jump_table[] = {
    &&L0,
    &&L1,
    &&L2,
    &&L3,
    &&L4,
    &&L5,
    &&L6,
    &&L7,
    &&L8,
    &&L9,
    &&L10,
    &&L11,
    &&L12,
    &&L13,
    &&L14,
    &&L15,
    &&L16,
    &&L17,
    &&L18,
    &&L19,
    &&L20,
    &&L21,
    &&L22,
    &&L23,
    &&L24,
    &&L25,
    &&L26,
    &&L27,
    &&L28,
    &&L29,
    &&L30,
    &&L31
  };
  printf("starting...\n");
begin:
#if DEBUG
  printf("pc = %i, a = %i, b = %i, c = %i, d = %i, e = %i, f = %i, g = %i, h = %i\n", pc, a, b, c, d, e, f, g, h);
#endif
  if (pc <= 31 && pc >= 0) {
    goto *jump_table[pc];
  } else {
    printf("pc = %i, a = %i, b = %i, c = %i, d = %i, e = %i, f = %i, g = %i, h = %i\n", pc, a, b, c, d, e, f, g, h);
    printf("mulcount = %i\n", mulcount);
    printf("complete\n");
    return 0;
  }
L0:
  b = 93;
  goto next;
L1:
  c = b;
  goto next;
L2:
  if(0 != a) {
    pc += 2;
    goto begin;
  } else {
    goto next;
  }
L3:
  if(0 != 1) {
    pc += 5;
    goto begin;
  } else {
    goto next;
  }
L4:
  b *= 100;
  mulcount += 1;
  goto next;
L5:
  b -= -100000;
  goto next;
L6:
  c = b;
  goto next;
L7:
  c -= -17000;
  goto next;
L8:
  f = 1;
  goto next;
L9:
  d = 2;
  goto next;
L10:
  e = 2;
  goto next;
L11:
#if PROBLEM2
  if(!is_prime(b)) {
    f = 0;
  }
  pc = 24;
  goto begin;
#endif
  g = d;
  goto next;
L12:
  g *= e;
  mulcount += 1;
  goto next;
L13:
  g -= b;
  goto next;
L14:
  if(0 != g) {
    pc += 2;
    goto begin;
  } else {
    goto next;
  }
L15:
  f = 0;
  goto next;
L16:
  e -= -1;
  goto next;
L17:
  g = e;
  goto next;
L18:
  g -= b;
  goto next;
L19:
  if(0 != g) {
    pc += -8;
    goto begin;
  } else {
    goto next;
  }
L20:
  d -= -1;
  goto next;
L21:
  g = d;
  goto next;
L22:
  g -= b;
  goto next;
L23:
  if(0 != g) {
    pc += -13;
    goto begin;
  } else {
    goto next;
  }
L24:
  if(0 != f) {
    pc += 2;
    goto begin;
  } else {
    goto next;
  }
L25:
  h -= -1;
  goto next;
L26:
  g = b;
  goto next;
L27:
  g -= c;
  goto next;
L28:
  if(0 != g) {
    pc += 2;
    goto begin;
  } else {
    goto next;
  }
L29:
  if(0 != 1) {
    pc += 3;
    goto begin;
  } else {
    goto next;
  }
L30:
  b -= -17;
  goto next;
L31:
  if(0 != 1) {
    pc -= 23;
    goto begin;
  } else {
    goto next;
  }
next:
  pc++;
  goto begin;
}
