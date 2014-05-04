struct A {
    int x, y;
} sa;
struct A print(struct A a, struct A b) {
    a.x++;
    a.y++;
    b.x--;
    b.y--;
    printf("args: %d %d\n", a.x, a.y);
    printf("args: %d %d\n", b.x, b.y);
    return a;
}
int main() {
    int i;
    int t;
    int *a, *b;
    struct A sb, sc;
    a = malloc(sizeof(int) * 100);
    for (i = 0; i < 100; i++)
        a[i] = i;
    b = malloc(sizeof(int) * 100);
    memcpy(b, a, sizeof(int) * 100);
    for (i = 0; i < 100; i++)
        printf("%d ", b[i]);
    sb.x = 1;
    sb.y = 2;
    sa = sb;
    sc = sa;
    printf("\n%d %d\n", sa.x, sa.y);
    printf("%d %d\n", sc.x, sc.y);
    sa.x = 1;
    sa.y = 2;
    sb.x = 1;
    sb.y = 2;
    sa = print(sa, sb);
    sb = print(sa, sb);
    sa = print(sa, sb);
    sb = print(sa, sb);
    printf("%d %d\n", sa.x, sa.y);
    printf("%d %d\n", sb.x, sb.y);
}
