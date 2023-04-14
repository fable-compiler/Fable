#include "./rc.c"

#ifndef Closure_C
#define Closure_C

// Currently not directly used for creation, only as casting templates
typedef struct Rc(*fn1)(struct Rc self, struct Rc p_0);
struct FnClosure1 {
    fn1 fn;
};

#endif