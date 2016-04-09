#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

const int READ_SIZE = 1024 * 5;

int main(int argc, char* argv[]) {
  if (3 != argc) {
    return 1;
  }

  const char* file_name = argv[1];
  const char* query_body = argv[2];

  char response_buf[READ_SIZE];
  
  // make request
  int requester = open("/home/quasilyte/CODE/GIT/getcodd/getcodd-in", O_WRONLY);
  if (-1 == requester) {
    return 2;
  }
  write(requester, query_body, strlen(query_body));
  close(requester);
  
  // wait for response
  // remove(file_name);
  // mkfifo(file_name, 0666);
  int responder = open(file_name, O_RDONLY);
  if (-1 == responder) {
    return 3;
  }
  read(responder, &response_buf, READ_SIZE);
  puts(response_buf);

  close(responder);
  
  return 0;
}
