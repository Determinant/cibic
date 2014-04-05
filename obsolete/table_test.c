#include <stdlib.h>
#include <stdio.h>
#include "semantics.h"
#define PV(str) \
    do \
    { \
        if (cscope_push_var(scope, newvar(str))) \
            fprintf(stderr, "Successfully pushed var: %s\n", str); \
        else \
            fprintf(stderr, "Naming conflicts deteced: %s\n", str); \
    } while(0)

#define PT(str) \
    do \
    { \
        if (cscope_push_type(scope, newtype(str))) \
            fprintf(stderr, "Successfully pushed type: %s\n", str); \
        else \
            fprintf(stderr, "Naming conflicts deteced: %s\n", str); \
    } while(0)

CVar_t newvar(const char *name) {
    return cvar_create(name, NULL, NULL);
}

CType_t newtype(const char *name) {
    return ctype_create(name, 0, NULL);
}

void manual() {
    CScope_t scope = cscope_create();
    PV("a");
    PV("b");
    PV("asdf");
    PV("fdsa");
    PV("hello");
    cscope_debug_print(scope);
    cscope_enter(scope);
    PV("a");
    PV("hello");
    PT("CType");
    cscope_debug_print(scope);
    cscope_enter(scope);
    PV("a");
    PV("yay");
    PV("world");
    PT("CType");
    PV("a");
    cscope_debug_print(scope);
    cscope_exit(scope);
    cscope_debug_print(scope);
    cscope_exit(scope);
    cscope_debug_print(scope);
}

char *str_gen(int len) {
    int i;
    char *str = malloc(len);
    for (i = 0; i < len; i++)
        str[i] = rand() % 2 + 'a';
    return str;
}

void autoforce() {
    static const int max_lvl = 100,
                    max_push = 10;
    int i, j;
    CScope_t scope = cscope_create();
    for (i = 0; i < max_lvl; i++)
    {
        cscope_enter(scope);
        int push = rand() % max_push;
        for (j = 0; j < push; j++)
        {
            int len = rand() % 3 + 1;
            int opt = rand() & 1;
            if (opt) PV(str_gen(len));
            else PT(str_gen(len));
        }
    }
    for (i = 0; i < max_lvl; i++)
    {
        cscope_debug_print(scope);
        cscope_exit(scope);
    }
}

int main() {
/*    manual(); */
    autoforce();
    return 0;
}
