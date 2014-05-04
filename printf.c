int printf(char *fmt) {
    char *arg, ch;
    arg = (int)&fmt + sizeof(char*);
    while ((ch = *fmt))
    {
        if (ch == '%')
        {
            ch = *(++fmt);
            if (ch == 'd')
                __print_int(*(int *)arg);
            else if (ch == 'c')
                __print_char(*(char *)arg);
            else if (ch == 's')
                __print_string(*(char **)arg);
            else
            {
                int len, x = *(int *)arg;
                if (!x) len = 1;
                else
                    for (len = 0; x; x /= 10, len++);
                len = 4 - len;
                while (len) __print_char('0'), len--;
                __print_int(*(int *)arg);
                fmt += 2;
            }
            arg += 4;
        }
        else __print_char(ch);
        fmt++;
    }
}
