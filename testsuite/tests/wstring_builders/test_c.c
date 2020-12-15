#include <string.h>

int c_strlen (char *message) {
  int length = 0;
  char *cursor = message;

  while (cursor[0] != '\0' || cursor[1] != '\0') {
     cursor = cursor + 2;
     length++;
  }
  return length;
}
