struct D {
    int a, b;
};
struct A {
    int a[100];
    struct B {
        struct C {
            int x, y;
        } c;
        int z;
        struct D *p;
    } b;
} s;
int main() {
    struct D d;
    int n = 0;
    s.a[1] = 1;
    s.a[2] = 2;
    s.b.z = 4;
    s.b.c.x = 5;
    s.b.c.y = 6;
    s.b.p = &d;
    s.b.p->a = 7;
    s.b.p->b = 8;
    printf("%d %d %d %d %d %d %d\n", s.a[1], s.a[2], s.b.z, s.b.c.x, s.b.c.y, s.b.p->a, s.b.p->b);
}
