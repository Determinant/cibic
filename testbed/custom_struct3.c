struct A {
    int x, y;
} a[10];
struct A f(struct A a) {
    a.x++;
    a.y++;
    return a;
}
int main(){
    int i;
    for (i = 1; i < 10; i++)
        a[i] = f(a[i - 1]);
    for (i = 0; i < 10; i++)
        printf("%d %d\n", a[i].x, a[i].y);
}
