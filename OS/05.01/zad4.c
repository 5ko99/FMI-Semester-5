#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
int main(int argc, char *argv[])
{
    char *args[] = {"ls","-l",NULL};
    printf("This is process id:%i \n", getpid());
    printf("This is parent id:%i \n", getppid());
    int forkId = fork();
    if (forkId == 0)
    {
        printf("This is result of fork in child:%i \n", forkId);
        printf("This is process id in child after fork:%i \n", getpid());
        printf("This is parent id in child after fork:%i \n", getppid());
        execvp(args[0],args);
        //If command is not successfully replaced, then exit code of a child will be 1, if is then 0
        exit(1);
    }
    else
    {
        printf("Fork result in parent:%i \n", forkId);
        printf("This is process id in parent after fork:%i \n", getpid());
        printf("This is parent id in parent after fork:%i \n", getppid());
        int stat;
        wait(&stat);
        printf("Child id after wait: %i \nChild status:%i \n", forkId, stat);
    }
    return 0;
}
