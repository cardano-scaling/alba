# Caledonia
**This is a work in progress** 🛠

Approximate Lower Bound Arguments ([ALBA](https://eprint.iacr.org/2023/1655.pdf)).

Caledonia is a Rust implementation of ALBA's prehashed scheme using `Blake2b` as hash function.

* Test to measure running times of the `setup`, `prove`, and `verify` is provided.


## Toy-size Benchmarks
Here we give the running times of the `setup`, `prove`, and `verify`. We run the test on macOS 14.5 on an Apple M1 Pro machine with 16 GB of RAM.

```shell
l:100 s:10 u:9: 	 setup 0mus 	 prove:12ms 	 verify:0ms
l:100 s:20 u:15: 	 setup 0mus 	 prove:2ms 	 verify:0ms
l:100 s:40 u:25: 	 setup 0mus 	 prove:18ms 	 verify:0ms
l:100 s:80 u:46: 	 setup 0mus 	 prove:246ms 	 verify:0ms
l:150 s:10 u:9: 	 setup 0mus 	 prove:1ms 	 verify:0ms
l:150 s:20 u:15: 	 setup 0mus 	 prove:12ms 	 verify:0ms
l:150 s:40 u:25: 	 setup 0mus 	 prove:31ms 	 verify:0ms
l:150 s:80 u:46: 	 setup 0mus 	 prove:8ms 	 verify:0ms
```
