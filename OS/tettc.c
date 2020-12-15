#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc, char* argv[]){
    printf("%s %s \n",argv[1],argv[2]);
    return 0;
}
