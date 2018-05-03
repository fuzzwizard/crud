#include "alltypes.hpp"

Buffer read_file_into_buffer(const char * path) {
  auto f = fopen(path, "r");
  assert(f);
  // defer { fclose(f); };

  fseek(f, 0, SEEK_END);
  int file_size = ftell(f);
  fseek(f, 0, SEEK_SET);

  Buffer result;
  result.initialize(file_size);
  result.count = fread(result.data, sizeof(u8), file_size, f);

  fclose(f);
  return result;
}