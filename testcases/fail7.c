int *main(int a,int b) {
    struct {int x;} c;
    /* fail because of wrong argument type */
    main(2, c);
    return &a;
}
