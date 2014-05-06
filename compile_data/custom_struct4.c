struct A {
    struct B {
        int x, y;
        struct C {
            int w;
        } c;
    } b;
    int z;
};

struct B f(struct A a) {
    printf("z: %d\n", a.z);
    return a.b;
}

struct C g(struct B b) {
    printf("x: %d\ny: %d\n", b.x, b.y);
    return b.c;
}

int main() {
    struct A a;
    a.z = 1;
    a.b.x = 2;
    a.b.y = 3;
    a.b.c.w = 4;
    printf("w: %d\n", g(f(a)).w);
}
