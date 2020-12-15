#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc, char* argv[]){
    char c,file1[50], file2[50];
    scanf("%s %s",&file1,&file2);
    int fd_from = open(file1, O_RDONLY);
    int fd_to = open(file2, O_WRONLY | O_CREAT | O_TRUNC,0664);
    char buf[1];
    while(read(fd_from,buf,1)!=0){
        write(fd_to,buf,1);
    }
    close(fd_from);
    close(fd_to);
    return 0;
}
