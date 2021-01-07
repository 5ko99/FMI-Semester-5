#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
int main(int argc, char *argv[])
{
    printf("This is process id:%i \n", getpid());
    printf("This is parent id:%i \n", getppid());
    int forkId = fork();
    if (forkId == 0)
    {
        printf("This is result of fork in child:%i \n", forkId);
        printf("This is process id in child after fork:%i \n", getpid());
        printf("This is parent id in child after fork:%i \n", getppid());
        printf("This is the result of execlp with ls in child:%i \n", execlp("ls", "ls", NULL));
        //If command is not successfully replaced, then exit code of a child will be 1, if is then 0
        exit(-1);
    }
    else
    {
        printf("Fork result in parent:%i \n", forkId);
        printf("This is process id in parent after fork:%i \n", getpid());
        printf("This is parent id in parent after fork:%i \n", getppid());
        int stat;
        int childIdAfterWait = wait(&stat);
        printf("Child id after wait: %i \nChild status:%i \n", childIdAfterWait, WEXITSTATUS(stat));
    }
    return 0;
}