#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc,char* argv[]) {
    int fd_from = open(argv[1],O_RDONLY);
    if(fd_from==-1){
        perror("File not opened!");
        return -1;
    }
    int fd_to = open(argv[2],O_WRONLY | O_CREAT | O_TRUNC | O_EXCL,0640);
    if(fd_to==-1){
        fd_to = open(argv[2],O_WRONLY | O_TRUNC);
        if(fd_to==-1) {
            perror("File not opened for write!");
            return -1;
        }
    }
    char buf[1];
    while(read(fd_from,buf,1)>0) {
        write(fd_to,buf,1);
    }
    close(fd_from);
    close(fd_to);
    return 0;
}
