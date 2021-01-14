#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
int main(int argc, char *argv[])
{
    int stat;
    char *path = argv[1];
    char *params[10];
    for (int i = 1; i < argc; ++i)
    {
        params[i - 1] = argv[i];
    }
    params[argc - 1] = 0;
    int forkId = fork();
    if (forkId == 0)
    {
        execvp(path, params);
        perror("error!");
        exit(1);
    }
    else
    {
        int cpid = wait(&stat);
        printf("\nChild code after wait:%i\nWith status:%i \n",
               cpid, stat);
    }
    return 0;
}