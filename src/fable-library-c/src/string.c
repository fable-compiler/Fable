#include <string.h>
#include "./rc.c"

struct String {
    char *data;
};

struct Rc String_New(char *inStr) {
    struct String str;
    str.data = malloc(strlen(inStr));
    return Rc_New(sizeof(str), &str);
}

//get field pattern
char* String_Get_Char(struct Rc* rc) {
    char* data = ((struct String *)rc->data)->data;
    String_Dispose(rc);
    return data;
}

struct Rc String_Concat(struct Rc* left, struct Rc* right) {
    int newSz = strlen(((struct String *)left->data)) + strlen(((struct String *)right->data));
    char* next = malloc(newSz);
    strcpy(next, (struct String*)left->data);
    strcat(next, ((struct String*)right->data)->data);
    String_Dispose(left);
    String_Dispose(right);
    return String_New(next);
}

void String_Dispose(struct Rc* rc){
    if(Rc_Dispose(*rc) == 0){
        free(((struct String *)(rc->data))->data);
    }
}