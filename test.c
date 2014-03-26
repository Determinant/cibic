#include <stdlib.h>
#include "semantics.h"
#define PV(str) cscope_push_var(scope, newvar(str))
#define PT(str) cscope_push_type(scope, newtype(str))

CVar_t newvar(const char *name) {
    return cvar_create(name, NULL);
}

CType_t newtype(const char *name) {
    return ctype_create(name, 0);
}

int main() {
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
    cscope_debug_print(scope);
    cscope_exit(scope);
    cscope_debug_print(scope);
    cscope_exit(scope);
    cscope_debug_print(scope);
    return 0;
}
