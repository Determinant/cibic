int main() {
    struct {int x, y;} a;
    struct {int x, y;} b, c;
    a = b;
    b = c;
    c = a;
}
