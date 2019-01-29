int printf(char*,...);
int scanf(char*,...);
void exit(int);
void *malloc(int);
int getchar();
int strlen(const char *s);
char *strcat(char *s1, const char *s2);
char *strcpy(char *s1, const char *s2);

void printInt(int a) {
    printf("%d\n", a);
}

void printString(char *s) {
    printf("%s\n", s);
}

void error() {
    printf("runtime error\n");
    exit(1);
}

int readInt() {
    int a;
    scanf("%d\n", &a);
    return a;
}

char *readString() {
    int len = 2048;
    char *line = malloc(len * sizeof(line));
    scanf(" %2048[^\n]",line);
    getchar();
    return line;
}

char *_concatenate(char *str1, char *str2) {
    char *res;
    int len = strlen(str1) + strlen(str2) + 1;
    res = malloc(len * sizeof(*res));
    strcpy(res, str1);
    strcat(res, str2);
    return res;
}
