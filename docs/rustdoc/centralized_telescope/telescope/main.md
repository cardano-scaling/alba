

---
> 📖 **See the documentation: [Telescope - Construction with Bounded DFS][crate::docs::centralized]** for further details.
---

## Creating a Telescope

Constructing an ALBA proof within a **centralized Telescope-bounded DFS setup** requires careful parameter selection to ensure both efficiency and security.
- The **soundness parameter** ($\lambda_{\text{sec}}$) controls how difficult it is for an adversary to forge a valid proof.
  - A higher $\lambda_{\text{sec}}$ increases the difficulty of false proofs but may require more computational resources.
- The **reliability parameter** ($\lambda_{\text{rel}}$) determines the probability that an honest prover can generate a valid proof within the given constraints.
  - If $\lambda_{\text{rel}}$ is too low, legitimate proof generation may fail, necessitating retries or adjustments in other parameters.
- The **DFS bound** is essential for computational feasibility, limiting the number of explored vertices during proof construction.
  - A sufficiently high DFS bound ensures a valid proof can be found, while an overly restrictive DFS bound may lead to proof generation failure.
- The **prover set size** ($n_p$) affects the likelihood of successful proof generation. 
  - With smaller values requiring additional proof attempts to compensate for reduced availability of valid elements.
- The **random oracle model** plays a key role in ensuring unpredictability and cryptographic robustness.

The proof construction process involves performing a bounded DFS over the prover set, selecting elements that maintain prefix validity while ensuring that the traversal remains within predefined limits. 
If a valid proof is not found within the DFS bound, the prover may retry or adjust parameters to optimize success probability. 

The verification process then checks proof validity against the bounded DFS constraints, rejecting invalid proofs while maintaining a balance between proof size and security guarantees. 

Properly tuning these settings ensures that ALBA proofs remain compact, verifiable, and secure within the centralized Telescope framework.

The goal is to ensure that honest provers can reliably generate proofs while preventing adversaries from exploiting weaknesses. 
Adjusting these parameters dynamically based on $n_p$ ensures that the system remains both secure and efficient.

### Methods for creating a Telescope
We provide two methods for creating a `Telescope`, each serving different purposes based on the level of control and responsibility required by the user.

#### Standard Creation 
- The [`create`][crate::centralized_telescope::Telescope#method.create] method is the recommended way to instantiate a `Telescope`, as it ensures correctness by automatically generating internal parameters through fine-tuning. 
- The user provides the **security parameters**, **set size**, and **lower bound**, and the function derives the optimal internal configurations for efficient proof generation and verification. 
- This approach guarantees that the `Telescope` instance adheres to expected security and performance constraints, making it the safest and most reliable option.

---
> 📖 **See full documentation: [Parameter Setup - Centralized Telescope][crate::docs::centralized::params]** for further details.
---

#### Manual Creation
- The [`setup_unsafe`][crate::centralized_telescope::Telescope#method.setup_unsafe] method allows the user to create a `Telescope` manually by directly specifying internal parameters. 
- Unlike `create`, this method does **not** verify the consistency or correctness of the provided parameters. 
- This gives the user full control but also places the responsibility on them to ensure that the chosen parameters do not compromise security or efficiency. 
- The `setup_unsafe` function exists to provide flexibility for advanced users who need custom configurations or create a `Telescope` object with trusted, already checked, parameters. 
- Some users may have precomputed or externally derived parameters that they wish to use without the restrictions imposed by automatic fine-tuning. 
- However, since there is no internal validation, improper usage can lead to weaker security guarantees, incorrect proofs, or unintended protocol behavior.

#### Which one should you use?
- Use **`create`** if you want a **safe and optimized** `Telescope` with parameters tuned automatically.
- Use **`setup_unsafe`** **only** if you fully understand the implications and need custom parameter configurations.

## Proof generation
- The goal is to ensure that a valid proof can be constructed efficiently even when the prover set is limited. 
- A bounded depth-first search approach is used to explore the prover set and identify a valid proof within a controlled number of steps. 
- The proof generation process follows a structured search strategy where the prover attempts to construct a proof by selecting elements that satisfy predefined constraints. 
- If a valid proof is not found within the DFS bound, the prover retries multiple times, leveraging probabilistic guarantees to increase the chance of success.

A centralized Telescope proof is generated using the [`prove`][crate::centralized_telescope::Telescope#method.prove] method. 
This initiates the proof generation process for the provided prover set based on the current [Telescope structure][crate::docs::centralized::proof::variables#telescope].

---
> 📖 **See the documentation of proof generation: [$\mathsf{prove}$][crate::docs::centralized::proof::prove].**
---

## Proof verification
- The verification process ensures that a proof is valid while maintaining efficiency and robustness against adversarial attempts. 
- Given that the proof is constructed using a bounded depth-first search (DFS), the verifier must confirm that the proof adheres to the constraints defined by the ALBA protocol. 
- The verification algorithm ensures that the proof elements were correctly selected according to the DFS traversal
- If any element does not meet the expected hash conditions, the proof is rejected.

A centralized Telescope proof is verified using the [`verify`][crate::centralized_telescope::Telescope#method.verify] method.
This initiates the proof verification process for the provided proof based on the current [Telescope structure][crate::docs::centralized::proof::variables#telescope].

---
> 📖 **See the documentation of proof verification: [$\mathsf{verify}$][crate::docs::centralized::proof::verify].**
---
