int N = 0;
void f() { N = 2; }
int main() {
    int a, b;
    a = N + 1;
    f();
    b = N + 1;
    printf("%d\n", b);
}
