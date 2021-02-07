#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc,char* argv[]) {
    int fd = open(argv[1],O_RDONLY);
    if(fd==-1) return -1;
    int chars=0, words=0, lines=0;
    char buf[1];
    while(read(fd,buf,1)>0) {
        if(buf[0]=='\n'){
            lines++;
            words++;
        }
        if(buf[0]==' ') words++;
        chars++;
    }
    printf(" %i %i %i %s\n",lines,words,chars,argv[1]);
    close(fd);
    return 0;
}
