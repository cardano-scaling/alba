#include "blake2b.h"

enum Blake2_Size {
  BLAKE2B_224 = 28,
  BLAKE2B_256 = 32
};

int blake2b256_hash(const unsigned char *input, const size_t input_length, uint8_t *output) {
  return crypto_generichash_blake2b(output, BLAKE2B_256, (const unsigned char*) input, input_length, NULL, 0);
}

int blake2b224_hash(const unsigned char *input, const size_t input_length, uint8_t *output) {
  return crypto_generichash_blake2b(output, BLAKE2B_224, (const unsigned char*) input, input_length, NULL, 0);
}
