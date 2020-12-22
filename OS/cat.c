#include<stdio.h>
#include<sys/types.h>
#include<sys/stat.h>
#include<fcntl.h>
#include<unistd.h>
int main(int argc, char* argv[]) {
 char buf[1];
 while(read(0, buf, 1) > 0) {
  write(1, buf, 1);
 }
 return 0;
}
