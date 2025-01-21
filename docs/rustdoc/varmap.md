Variable name mapping.

In the [paper](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/), numerous variables are represented by various letters. To enhance the simplicity and readability of our code, we have opted for descriptive names. A mapping between the variable names used in the paper and those in the code is provided for reference.


| Paper              | Code                      |
|--------------------|---------------------------|
| $\lambda_{sec}$    | `soundness_param`         |
| $\lambda_{rel}$    | `completeness_param`      |
| $S_p$              | `prover_set`              |
| $n_p$              | `set_size`                |
| $n_f$              | `lower_bound`             |
| $u$                | `proof_size`              |
| $r$                | `max_retries`             |
| $d$                | `search_width`            |
| $q$                | `valid_proof_probability` |
| $b$                | `dfs_bound`               |
| $v$                | `retry_counter`           |
| $t$                | `search_counter`          |
| $H_0$              | `bin_hash`                |
| $H_1$              | `round_hash`              |
| $H_2$              | `proof_hash`              |
| $s_i$              | `Element`                 |
| $s_1, \ldots, s_u$ | `element_sequence`        |
| $p$                | `lottery_probability`     |
| $limit$            | `step`                    |

