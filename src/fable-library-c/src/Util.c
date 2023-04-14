#include "./rc.c"
#include <stdbool.h>

#ifndef Util_C
#define Util_C

bool equals(struct Rc a, struct Rc b) {
    return a.data == b.data;
}

#endif