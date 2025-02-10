# Centralized Telescope
The `Telescope` is the core cryptographic component designed for generating and verifying ALBA proofs in the centralized setting.
It provides the core methods `prove` for constructing proofs and `verify` for validating them, ensuring efficient and reliable proof management.
A `Telescope` instance is created using the `create` method, which initializes it with protocol-specific parameters such as the prover set size.
An alternative `setup_unsafe` method allows manual parameter assignment and should be used with caution.
Additionally, helper functions like `get_set_size` and `get_params` provide access to internal values, supporting easier management of proof parameters.
