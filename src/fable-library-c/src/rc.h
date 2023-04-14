#include <stdlib.h>

#ifndef Rc_H
#define Rc_H

struct Rc {
    void *data;
    int (*dispose) (void *data);
    int *count;
};

struct Rc Rc_New(int size, void *data, void *dispose(void *data));

struct Rc Rc_Clone(struct Rc value);

int Rc_Dispose(struct Rc value);

#endif