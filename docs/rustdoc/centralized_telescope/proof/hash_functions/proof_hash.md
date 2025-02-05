## Proof hash
- $\mathsf{H_2}(q,~ $ [$round$](variables#round) $) \rightarrow (true ~ /~ false):$
---
- $hash \leftarrow \mathsf{digest}($ [$round.hash$](variables#round-digest) $)$
- **return** $~~ \mathsf{sample\\_bernoulli}(hash,~ q).$
---
