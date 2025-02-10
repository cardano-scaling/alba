# Telescope - Construction with Bounded DFS

In scenarios where $n_p$ is small, the parameters given for prehashed construction (relative to the security parameter, $\lambda$) are not optimal, resulting in a reduced probability of constructing a valid proof in a single attempt.
For large $n_p$, the rapid growth in potential proof tuples ensures valid ones can be found efficiently, allowing the prehashed construction to work seamlessly.
In contrast, small $n_p$ limits the search space, making the previous parameters inadequate.
To address this, the **construction with bounded DFS** expands on the prehashed version with *retries*, *prehashing randomization*, and *bounding* the DFS.
