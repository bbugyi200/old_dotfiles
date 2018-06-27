#include "CppUTest/CommandLineTestRunner.h"

int main(int argc, char** argv) {
  MemoryLeakWarningPlugin::turnOffNewDeleteOverloads();
  return CommandLineTestRunner::RunAllTests(argc, argv);
}
