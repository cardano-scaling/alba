In the [paper](https://iohk.io/en/research/library/papers/approximate-lower-bound-arguments/), numerous variables are represented by various letters. To enhance the simplicity and readability of our code, we have opted for descriptive names. A mapping between the variable names used in the paper and those in the code is provided for reference.


| Paper              | Code                                                                                             |
|--------------------|--------------------------------------------------------------------------------------------------|
| $\lambda_{sec}$    | [`soundness_param`][crate::centralized_telescope::params::Params::soundness_param]               |
| $\lambda_{rel}$    | [`completeness_param`][crate::centralized_telescope::params::Params::completeness_param]         |
| $S_p$              | `prover_set`                                                                                     |
| $n_p$              | [`set_size`][crate::centralized_telescope::params::Params::set_size]                             |
| $n_f$              | [`lower_bound`][crate::centralized_telescope::params::Params::lower_bound]                       |
| $u$                | [`proof_size`][crate::centralized_telescope::setup::Setup::proof_size]                           |
| $r$                | [`max_retries`][crate::centralized_telescope::setup::Setup::max_retries]                         |
| $d$                | [`search_width`][crate::centralized_telescope::setup::Setup::search_width]                       |
| $q$                | [`valid_proof_probability`][crate::centralized_telescope::setup::Setup::valid_proof_probability] |
| $b$                | [`dfs_bound`][crate::centralized_telescope::setup::Setup::dfs_bound]                             |
| $v$                | `retry_counter`                                                                                  |
| $t$                | `search_counter`                                                                                 |
| $H_0$              | `bin_hash`                                                                                       |
| $H_1$              | `round_hash`                                                                                     |
| $H_2$              | `proof_hash`                                                                                     |
| $s_i$              | `Element`                                                                                        |
| $s_1, \ldots, s_u$ | `element_sequence`                                                                               |
| $p$                | [`lottery_probability`][crate::simple_lottery::setup::Setup::lottery_probability]                |

