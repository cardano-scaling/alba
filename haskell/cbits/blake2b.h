#ifndef BLAKE2B_H
#define BLAKE2B_H

#include <stdio.h>
#include <string.h>
#include <sodium/crypto_generichash_blake2b.h>

int blake2b256_hash(const unsigned char *input, const size_t input_length, uint8_t *output);

int blake2b224_hash(const unsigned char *input, const size_t input_length, uint8_t *output);


#endif
