#include <string.h>
#include "./rc.c"

struct Rc String_New(char *inStr) {
    return Rc_New(strlen(inStr), inStr, NULL);
}

//get field pattern
char* String_Get_Char(struct Rc rc) {
    char* data = rc.data;
    return data;
}

struct Rc String_Concat(struct Rc left, struct Rc right) {
    int newSz = strlen(left.data) + strlen(right.data);
    char* next = malloc(newSz);
    strcpy(next, left.data);
    strcat(next, right.data);
    Rc_Dispose(left);
    Rc_Dispose(right);
    return String_New(next);
}

// pattern to wrap a struct in an Rc
struct W_String {
    char *data;
};

struct Rc W_String_New(char *inStr) {
    struct W_String str;
    str.data = malloc(strlen(inStr));
    return Rc_New(sizeof(str), &str, W_String_Dispose);
}

//get field pattern
char* W_String_Get_Char(struct Rc rc) {
    char* data = ((struct W_String *)rc.data)->data;
    Rc_Dispose(rc);
    return data;
}

struct Rc W_String_Concat(struct Rc left, struct Rc right) {
    int newSz = strlen(((struct W_String *)left.data)) + strlen(((struct W_String *)right.data));
    char* next = malloc(newSz);
    strcpy(next, (struct W_String*)left.data);
    strcat(next, ((struct W_String*)right.data)->data);
    Rc_Dispose(left);
    Rc_Dispose(right);
    return W_String_New(next);
}

void W_String_Dispose(void *data){
    free(((struct W_String *)(data))->data);
}