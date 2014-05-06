int main() {
    /* fail because of incomplete field */
    struct C {struct B b;} *c;
}
