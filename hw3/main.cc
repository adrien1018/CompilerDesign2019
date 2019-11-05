#include <iostream>
#include "driver.h"
#include "gv.h"

int main(int argc, char *argv[]) {
  int res = 0;
  Driver drv;
  for (int i = 1; i < argc; ++i) {
    if (!drv.parse(argv[i])) {
      PrintGV(drv.prog);
      std::cout << drv.result << '\n';
    } else {
      res = 1;
    }
  }
  return res;
}
