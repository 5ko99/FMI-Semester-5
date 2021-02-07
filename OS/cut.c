#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc,char* argv[]) {
    int fd = open(argv[1],O_RDONLY);
    if(fd==-1) return -1;
    int n,m;
    scanf("%i %i",&m,&n);
    char buf[1];
    int pnt = 1;
    while(read(fd,buf,1)>0) {
       if(buf[0]=='\n'){
           pnt=0;
           printf("\n");
       }
       if(pnt<m || pnt>n){
           pnt++;
           continue;
       }
       printf("%c",buf[0]);
       pnt++;
    }
    close(fd);
    return 0;
}
