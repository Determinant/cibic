int main() {
    struct A {int x, y;} b;
    int a[(1 + 1 ==  2) * 2] = {1, b};
}
