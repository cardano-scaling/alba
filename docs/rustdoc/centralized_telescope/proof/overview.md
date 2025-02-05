## Centralized Telescope Proof
The `Proof` structure represents a Centralized Telescope proof generated using a depth-first search (DFS).
It consists of an element sequence, a retry counter (`retry_counter`), and a search index (`search_counter`).

**Proof generation:** The `new` function initializes proof generation by calling `prove_routine`, which attempts multiple retries using `prove_index` to organize elements and start a DFS search.
The `dfs` function recursively explores sequences, selecting elements based on predefined rules.
A valid proof is returned immediately; otherwise, retries continue until the limit is reached.

**Proof verification:** The `verify` function checks proof correctness by ensuring `search_counter` and `retry_counter` are within bounds and `element_sequence` has the expected length.
It reconstructs the round, verifies element mapping, and updates the round state.
If any check fails, the proof is rejected.
A final probabilistic check determines validity, returning `true` if successful, otherwise `false`.
