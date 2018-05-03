#include "crud.hpp"

int main(int argc, char* argv[]) {
	auto b = read_file_into_buffer("data/test_program.crud");

	Scanner s(&b);

	auto tokens = s.scan_tokens();
	s.print_tokens();

	s.free();
	b.free();
}
