#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc, char* argv[]) {
    char buf[1];
    int to_write = open(argv[1], O_WRONLY | O_TRUNC | O_CREAT,0666);
    if(to_write== - 1) {
        perror("File cannot open!");
        return -1;
    }
    while(read(0,buf,1)>0) {
        write(1,buf,1);
        write(to_write,buf,1);
    }
    close(to_write);
    return 0;
}
