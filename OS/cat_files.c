#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc, char* argv[]) {
    char buf[1];
    for(int i=1; i < argc; ++i) {
        int open_file = open(argv[i],O_RDONLY);
        if(open_file == -1) {
            perror(argv[i]);
        } else {
            while(read(open_file,buf,1)>0) {
                write(1, buf,1);
            }
            close(open_file);
        }
    }
    return 0;
}
