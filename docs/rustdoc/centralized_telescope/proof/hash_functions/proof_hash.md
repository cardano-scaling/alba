## Proof hash
- It is used to determine whether a proof is valid based on a probabilistic check.
- Computes a hash from the round's hash value.
- Uses the hash and probability threshold $q$ to perform a Bernoulli trial.
- Returns `true` with probability in $\[q - \varepsilon, q\]$ for a negligible $\varepsilon$, otherwise returns `false`.

---
- $\mathsf{H_2}(q,~ $ [$round$](variables#round) $) \rightarrow (true ~ /~ false):$
---
- $hash \leftarrow \mathsf{digest}($ [$round.hash$](variables#round-digest) $)$
- **return** $~~ \mathsf{sample\\_bernoulli}(hash,~ q).$
---
