## Overview
The proof generation process relies on two core functions: `prove_index` and `dfs`, which work together to explore potential proof sequences efficiently.

The `prove_index` function organizes the prover's dataset into bins and performs multiple search attempts using depth-first search (DFS). 
- It first assigns elements to bins using the `bin_hash` function and then iterates over `search_width` attempts to find a valid proof. 
- In each search attempt, it generates a candidate round using the `round_hash` function and invokes `dfs` to explore possible proof sequences. 
- If `dfs` finds a valid proof, it is returned immediately; otherwise, the function continues until all search attempts are exhausted. 
- The total number of DFS calls is returned alongside the proof (or an empty proof if no valid sequence is found).

The `dfs` function performs a depth-first search to construct a valid proof by iterating through elements in bins.
Its process is as follows:
- If the sequence reaches `proof_size` $u$, it checks validity using `proof_hash` $\mathsf{H_2}$.
    - If valid, returns the proof; otherwise, returns total DFS calls and an empty proof.
- If incomplete, iterates over elements in the bin corresponding to the current round identifier $id$.
    - If DFS calls reach `dfs_bound` $b$, terminates early.
    - Adds elements to the sequence and computes a new round identifier using `round_hash` $\mathsf{H_1}$.
- If the new identifier is valid, recursively calls `dfs` on the updated round.
    - If a proof is found, returns it immediately.
    - Otherwise, updates the step counter and continues searching.
- If all elements are exhausted without finding a proof, returns total DFS calls and an empty proof.
