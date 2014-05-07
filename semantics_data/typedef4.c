typedef struct I I;
int incomp(I a);
struct I { int i, j; };
int incomp(I a) {}
typedef int b;
int main() {
    I i;
    b b;
    incomp(i);
}
