//! ALBA's bounded DFS scheme using Blake2b as hash function.
//! (c.f. Section 3.2.2 of Alba paper)

// Defines the internal parameters and provide functions for their generation
pub mod params;

// Defines different scenarios of parameter generation
mod cases;

// Defines the proof structure and provide high level functions, i.e. the depth
// first search algorithm (DFS) and indexed proving function, as well as helper
// hash functions
pub mod proof;

// Defines the internal round structure to facilitate the updating and checking
// of a proof candidate
mod round;

// Defines the user facing telescope structure, that is instantiated to
// generate an Alba proof out of a set and verify it
pub mod telescope;

// Modules defining internal types
mod types;
