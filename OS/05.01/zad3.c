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
    if (forkId == 0)
    {
        printf("This is process id in child after fork:%i \n", getpid());
        printf("This is parent id in child after fork:%i \n", getppid());
        //in child
        for (int i = 0; i < 1000000; ++i)
        {
        }
        exit(1);
    }
    else
    {
        printf("\nForkResult:%i \n", forkId);
        printf("This is process id after fork:%i \n", getpid());
        printf("This is parent id after fork:%i \n", getppid());
        int cpid = wait(&stat);
        printf("Child code after wait:%i \n With status:%i \n", cpid, WEXITSTATUS(stat));
    }
    return 0;
}