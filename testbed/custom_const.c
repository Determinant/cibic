int sum;
void f() {
    sum = 3;
}
int main() {
    sum = 1;
    f();
    printf("%d\n", sum);
}
