#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
int main(int argc, char *argv[])
{
    int stat;
    printf("This is process id:%i \n", getpid());
    printf("This is parent id:%i \n", getppid());
    int forkId = fork();
    if (forkId == 0) //child
    {
        printf("This is process id in child after fork:%i \n", getpid());
        printf("This is parent id in child after fork:%i \n", getppid());
        sleep(4);
        exit(0);
    }
    else //parent
    {
        printf("\nForkResult:%i \n", forkId);
        printf("This is process id after fork:%i \n", getpid());
        printf("This is parent id after fork:%i \n", getppid());
        int cpid = wait(&stat);
        printf("Child code after wait:%i \nWith status:%i \n", cpid, stat);
    }
    return 0;
}
