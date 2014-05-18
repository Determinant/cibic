#include <cstdio>
typedef void (*Func_t)();
void f(Func_t func, int step) {
    if (!step) return;
    printf("i'm f\n");
    func(func, step - 1);
}
void g(void (*func)(), int step) {
    if (!step) return;
    printf("i'm g\n");
    func(func, step - 1);
}
int main() {
    void (*func)(void (*ifunc)(), int step);
    int x = 1;
    if (x) func = f;
    else func = g;
    func(func, 5);
    return 0;
}
