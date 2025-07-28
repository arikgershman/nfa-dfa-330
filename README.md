# Regular Expression Engine ⚙️

This repository contains my implementation for Project 3, a Regular Expression Engine, from my CMSC320: Organization of Programming Languages at UMD. This project delves into the fundamental algorithms behind regular expressions, Non-deterministic Finite Automata (NFAs), and Deterministic Finite Automata (DFAs).

The core objective was to implement key transformations and simulations that form the basis of a regular expression interpreter, demonstrating proficiency in functional programming and theoretical computer science concepts.

## Project Overview

This project is structured into three interconnected parts, each focusing on a critical component of regular expression processing:

### Part 1: NFAs (Non-deterministic Finite Automata)

In this section, I implemented functions to simulate the behavior of NFAs. This involved understanding and translating formal definitions into OCaml code.

* **`move nfa qs s`**: Computes the set of states reachable from a given set of states `qs` by making a single transition on a symbol `s` (or an epsilon transition).

* **`e_closure nfa qs`**: Determines the set of states reachable from a given set of states `qs` by following zero or more epsilon transitions.

* **`accept nfa s`**: Checks if a given string `s` is accepted by the NFA `nfa`. This function leverages the `move` and `e_closure` functions to simulate the NFA's behavior on the input string.

### Part 2: Subset Construction (NFA to DFA Conversion)

This part focuses on the subset construction algorithm, a crucial process for converting an NFA into an equivalent DFA. The challenge here was to manage the state explosion that can occur during this conversion.

* **`new_states nfa qs`**: Identifies all new DFA states (sets of NFA states) reachable from a current DFA state `qs` by consuming a single input symbol.

* **`new_trans nfa qs`**: Generates the transitions for a new DFA state `qs` based on the NFA's transitions and epsilon closures.

* **`new_finals nfa qs`**: Determines if a new DFA state `qs` should be an accepting state based on whether it contains any of the NFA's final states.

* **`nfa_to_dfa nfa`**: The main function that orchestrates the subset construction, taking an NFA and returning an equivalent DFA.

### Part 3: Regular Expressions to NFAs

The final part involves building an NFA directly from a given regular expression. This demonstrates an understanding of how regular expression operators (concatenation, union, Kleene star, empty string, character) translate into NFA structures.

* **`regexp_to_nfa regexp`**: Converts a `regexp_t` (OCaml datatype representing a regular expression) into an equivalent `nfa_t`. This function required careful construction of NFAs for each regular expression operation, ensuring the resulting NFA accepts the same language.

## Key Concepts & Challenges

* **Functional Programming in OCaml:** All implementations strictly adhere to functional programming principles, avoiding imperative features like loops and mutable references (except for the provided `fresh` function for unique state generation).

* **Finite Automata Theory:** Deepened understanding of NFAs, DFAs, epsilon transitions, and the subset construction algorithm.

* **Recursive Data Types:** Extensive use of OCaml's powerful pattern matching and recursive data types for representing regular expressions and NFAs.

* **Algorithm Efficiency:** Particular attention was paid to the efficiency of `nfa_to_dfa` to avoid timeouts on large inputs, requiring careful design to minimize redundant computations.

* **Parser Integration:** While the parser (`string_to_regexp`) was provided, understanding its output and how to build NFAs from the `regexp_t` datatype was essential.

## Repository Structure

* `src/nfa.ml`: Contains implementations for NFA operations (`move`, `e_closure`, `accept`) and the NFA to DFA conversion (`nfa_to_dfa` and its helpers).

* `src/regexp.ml`: Contains the `regexp_to_nfa` function. (Parser code for `string_to_regexp` was provided).

* `src/sets.ml`: (Provided) Module for set operations, crucial for managing state sets in NFAs/DFAs.

* `test/`: Contains testing infrastructure and student-written tests.

## How to Run / Test the Project

To run and test this OCaml project, ensure you have OCaml version 4.13.0 or newer installed. The project uses `dune` as its build system.

1.  **Build the Project:**
    Compile your code:

    ```bash
    dune build
    ```

2.  **Run All Tests (Public and Student):**
    Execute all available tests:

    ```bash
    dune runtest -f
    ```

3.  **Run Specific Test File (e.g., public tests):**
    To run tests from a particular file:

    ```bash
    dune runtest -f test/public
    ```

    (Replace `test/public` with the path to your desired test file.)

4.  **Interactive Testing with Utop:**
    For an interactive OCaml top-level environment with your project functions loaded:

    ```bash
    dune utop src
    ```

    In `utop`, all commands must end with `;;`. Exit `utop` by typing `#quit;;` or pressing `Ctrl-D` / `Cmd-D`.
    
