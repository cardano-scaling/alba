This module implements the **proof generation** and **verification** functionalities in the **Centralized Telescope** scheme.
The `proof` structure represents a cryptographic proof generated using a depth-first search (DFS) algorithm. 
It contains three key fields: 
- Retry counter to track the number of retries made to find the proof, 
- Search counter indicating the index of the searched subtree,
- Element sequence, storing the sequence of elements forming the proof.

The proof system relies on internal algorithms, hash functions, and key parameters to efficiently construct and verify Alba proofs.
