int main() {
    struct {int x, y;} a, b, c;
    a = b;
    b = c;
    c = a;
}
