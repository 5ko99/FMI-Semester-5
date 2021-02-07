#include<stdio.h>
#include<sys/types.h>
#include<sys/stat.h>
#include<fcntl.h>
#include<unistd.h>
int main(int argc, char* argv[]) {
 int to_file = open(argv[1], O_WRONLY | O_APPEND | O_CREAT, 0666);
 if (to_file == -1) {
  perror(argv[1]);
  return -1;	 
 }
 char buf[1];
 while(read(0, buf, 1) > 0) {
  write(to_file, buf, 1);
 } 
 close(to_file);
 return 0;
} 
