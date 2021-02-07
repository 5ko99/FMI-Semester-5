#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
int main(int argc, char *argv[])
{
    int stat;
    char *args[30];
    for (int i = 0; i < argc-1; ++i)
    {
        args[i] = argv[i+1];
    }
    args[argc-1] = NULL;
    int forkId = fork();
    if (forkId == 0)
    {
        execvp(args[0],args);
        perror("error!");
        exit(1);
    }
    else
    {
        wait(&stat);
        printf("\nChild pid after wait:%i\nWith status:%i \n",
               forkId, stat);
    }
    return 0;
}
