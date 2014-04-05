int;
char;
struct {int x;};
/* useless declarations are ok */

int a(int a);
int a(int d);
/* duplicate function declarations are ok */

struct A {int x; int y;} b;

/* struct definitions in parameters is in a local scope
 * subsequent parameter can make use of previous defined struct */

int foo(struct A {int x;} a, struct A b) {
    /* function declaration in local scope is ok */
    int f(char *x);
    /* A is defined in parameters */
    struct A c;
    c.x = 43;
}

void bar() {
    /* struct definition is ok inside a function */
    struct A {int x;};
    struct A a;
    a.x = 1;
    {
        /* the following `A' is different from previous one */
        struct A {int y;};
        struct A b;
        b.y = 2;
    }
}

struct C c;
struct C {
    struct D {
        int x, y;
    } b;
    int c;
    struct D d;
};
struct D d; /* feasible here */

void nonsense() {
    char q;
    (void)q;
    return "yay";
}

int assign() {
    int *a;
    struct {int *x;} b;
    a = b.x;
}

void incomplete() {
    struct E {struct F *f;} e;
}

void delay() {
    struct G *g;
    struct G {int x; };
    g->x = 1;
}

void comma() {
    int a;
    int *b;
    (b++, a++) * 3;
}

int complex_pointer() {
    int (*f(int ***e[10]))();
}

struct Node n;
struct Node {int x, y;} n;
/* global forward declaration is ok */
int main() {
    n.x = 1;
    n.y = 2;
}
