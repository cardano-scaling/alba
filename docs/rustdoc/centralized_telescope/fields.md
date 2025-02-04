## Proof
- <a id="proof-v">$v$</a> : Numbers of retries done to find the proof $\implies$ `retry_counter`
- <a id="proof-t">$t$</a> : Index of the searched subtree to find the proof $\implies$ `search_counter`
- <a id="proof-slist">$s_1, \ldots, s_u$</a> : Sequence of elements from prover's set $\implies$ `element_sequence`

## Round
- <a id="round-v">$v$</a> : Numbers of retries done to find the proof $\implies$ `retry_counter`
- <a id="round-t">$t$</a> : Index of the searched subtree to find the proof $\implies$ `search_counter`
- <a id="round-slist">$s_1, \ldots, s_k$</a> : Sequence of elements from prover's set $\implies$ `element_sequence`
- <a id="round-digest">$hash$</a> : Candidate round hash
- <a id="round-id">$id$</a> : Candidate round id, i.e. round hash mapped to $\[1, n_p\]$
- <a id="round-np">$n_p$</a> : Approximate size of prover set to lower bound $\implies$ `set_size`

## Parameters
- <a id="params-u">$u$</a> : Proof size $\implies$ `proof_size`
- <a id="params-d">$d$</a> : Maximum number of subtrees to search to find a proof $\implies$ `search_width`
- <a id="params-q">$q$</a> : Probability that a tuple of element is a valid proof $\implies$ `valid_proof_probability`
- <a id="params-r">$r$</a> : Maximum number of retries to find a proof $\implies$ `max_retries`
- <a id="params-b">$b$</a> : Maximum number of DFS calls permitted to find a proof $\implies$ `dfs_bound`
