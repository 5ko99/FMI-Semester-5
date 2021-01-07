#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
int main(int argc, char *argv[])
{
    printf("This is process id:%i \n", getpid());
    printf("This is parent id:%i \n", getppid());
    printf("\nForkResult:%i \n", fork());
    printf("This is process id after fork:%i \n", getpid());
    printf("This is parent id after fork:%i \n", getppid());
    return 0;
}