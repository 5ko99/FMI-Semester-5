#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
int main(int argc, char *argv[])
{
    int forkID = fork();
    char *args1[] = {argv[1],NULL};
    char *args2[] = {argv[2],NULL};
    if(forkID == 0) {
        execvp(args1[0],args1);
        perror("Command 1 not found\n");
        return -1;
    } else {
        int stat;
        wait(&stat);
        if(stat!=0) {
            execvp(args2[0],args2);
            perror("Command 2 not found\n");
            return -1;
        }
    }
    return 0;
} 
 
