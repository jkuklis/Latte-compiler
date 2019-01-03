#include <stdio.h>
#include <stdlib.h>

void printInt(int a) {
    printf("%d\n", a);
}

void printString(char *s) {
    printf("%s\n", s);
}

void error() {
    printf("runtime error\n");
    int a = 0;
    int b = 1 / a;
}

int readInt() {
    int a;
    scanf("%d", &a);
}

char *readString() {
    char *line;
    size_t len = 0;
    getline(&line, &len, stdin);
    return line;
}
