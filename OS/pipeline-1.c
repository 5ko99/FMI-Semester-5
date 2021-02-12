/*

Как работи shell?
Пример на системните извиквания при работа на конвейр.

Имитира команда за shell "ps -a | grep gnome".

(c) народна песен на C, стил POSIX, по лиценза GNU GPL

*/

#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
// #include <sys/stat.h>
// #include <fcntl.h>
#include <stdio.h>

int main(void)
{
 int pid, pid1, fd[2];
 char buf[1];
 char* args1[] = { "ps", "-x", (char*)0 };
 char* args2[] = { "grep", "gnome", (char*)0 };
 pid=fork();
 if (pid>0) wait(NULL); // Waits the child termination
 if (pid==0) {   /* Child shell */
  pipe(fd);      /* Create pipe */
  pid1=fork(); 
  if (pid1==0) { /* process for cmd1 */
   close(fd[0]);  /* Close unused read end */
   dup2(fd[1],STDOUT_FILENO);
   close(fd[1]);  /* Close duplicated write end */
   //execvp(args1[0], args1);
   read(0,buf,1);
   printf("%c",buf[0]);
  }
  if (pid1>0) { /* process for cmd2 */
   close(fd[1]);  /* Close unused write end */
   dup2(fd[0],STDIN_FILENO);
   close(fd[0]);  /* Close duplicated read end */
   read(fd[2],buf,1);
   printf("%c",buf[1]);
   buf[0]='a';
   write(1,buf,1);
   //execvp(args2[0], args2);
  }
 }
 if (pid<0) perror("fork");
}
