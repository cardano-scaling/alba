<a id="proof-hash">$\mathsf{H_2}$</a> $(q,~ $ [$round$](variables#round) $) \rightarrow (true ~ /~ false):$
---
- $hash \leftarrow \mathsf{digest}($ [$round.hash$](variables#round-digest) $)$
- **return** $~~ \mathsf{sample\\_bernoulli}(hash,~ q).$
---
