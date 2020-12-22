#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc, char* argv[]) {
    char buf[1];
    int from_file = open(argv[1], O_WRONLY | O_APPEND | O_CREAT ,0666);
    if (from_file==-1) {
        perror(argv[1]);
        return -1;
    }
    char buf[1];
    
    return 0;
}
