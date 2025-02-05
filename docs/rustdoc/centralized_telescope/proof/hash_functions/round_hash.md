<a id="round-hash">$\mathsf{H_1}$</a> $(input_1,~ input_2,~ n_p) \rightarrow (hash,~ value ~ /~ \bot):$
---
- $hash \leftarrow \mathsf{digest}(input_1 ~ || ~ input_2)$
- $value \leftarrow \mathsf{sample\\_uniform}(hash,~ n_p)$
- **return** $~~ (hash,~ value).$
---
