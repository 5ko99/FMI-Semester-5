#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
int main(int argc, char* argv[]) {
    printf("This is process id:%i \n", getpid());
    printf("This is parent id:%i \n", getppid());
    printf("This is the result:%i \n", execlp("ps", "ps",NULL));
    perror("Error! \n");
    return 0;
}
