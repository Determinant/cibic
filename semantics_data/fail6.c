int main() {
    struct C {int x;} *c;
    /* fail because no member called `y' */
    c->y;
}
