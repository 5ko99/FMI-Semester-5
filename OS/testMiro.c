#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc,char* argv[]) {
    int f = fork();
    if(f) {
        printf("if %i",&f);
    } else {
        printf("else");
    }
    return 0;
}
