#include <stdlib.h>

#ifndef Rc_C
#define Rc_C

struct Rc {
    void *data;
    int (*dispose) (void *data);
    int *count;
};

struct Rc Rc_New(int size, void *data, void *dispose(void *data)) {
    struct Rc rc;
    rc.count = malloc(sizeof(int));
    *rc.count = 1;
    rc.data = malloc(size);
    rc.dispose = dispose;
    memcpy(rc.data, data, size);
    // rc.Data =
    return rc;
};

struct Rc Rc_Clone(struct Rc value) {
    //struct Rc* rc = (struct Rc*) value;
    *value.count = *value.count + 1;
    struct Rc next;
    next.count = value.count;
    next.data = value.data;
    return next;
}

int Rc_Dispose(struct Rc value) {
    *value.count = *value.count - 1;
    if(value.dispose != NULL)
        value.dispose(value.data);
    if(*value.count == 0){
        free(value.data);
        free(value.count);
    }
    return *value.count;
}

// how to use

// This is a .NET reference type
struct __Example_Use_Rc_Struct {
    int X;
};

int __example_use_rc() {
    //Create a new instance
    struct __Example_Use_Rc_Struct test;
    test.X = 1;
    struct Rc rc = Rc_New(sizeof(test), &test, NULL);

    //Leave context, ownership
    struct Rc rc2 = Rc_Clone(rc);

    //dereference
    int outVal = ((struct __Example_Use_Rc_Struct *)(rc2.data))->X;
    // (Test*)

    //Go out of scope, clean up
    Rc_Dispose(rc);
    Rc_Dispose(rc2);

    return 1;
}

struct __Example_Use_Rc_Struct_Complex {
    struct Rc a; //__Example_Use_Rc_Struct
};

#endif