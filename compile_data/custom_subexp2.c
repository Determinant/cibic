int flag = 0;
int check(int x, int y) {
    return x > 0 && y > 0 && (flag ^= 1);
}
int main() {
    int x = 1, y = 2;
    printf("%d\n", check(x, y));
    printf("%d\n", check(x, y));
    printf("%d\n", check(x, y));
}
