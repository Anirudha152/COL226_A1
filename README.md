# COL226 Assignment 1: OCaml module for vectors

**Authors:** Anirudha Saraf

## Project Description
This project implements a functional vector algebra library in OCaml. The module defines a vector as a list of floats and provides a comprehensive set of mathematical operations, error handling for dimensional mismatches, and geometric calculations.

In addition to the implementation, the source code includes formal proofs of correctness for various vector properties (such as commutativity, associativity, and distributivity) using induction.

## Implementation Details

### Type Definition
The core data structure is defined as:
```ocaml
type vector = float list

```

### Core Functions

The module implements the following operations:

* **Initialization**
* `create`: Generates a vector of dimension `n` initialized with value `x`.
* `unit`: Creates a unit vector of dimension `n` with a 1.0 at index `j`.


* **Properties**
* `dim`: Returns the dimension (length) of the vector.
* `is_zero`: Checks if the vector is a zero vector.
* `length`: Calculates the Euclidean norm (magnitude) of the vector.


* **Algebraic Operations**
* `scale`: Performs scalar multiplication.
* `inv`: Returns the additive inverse of the vector.
* `addv`: Performs vector addition. Raises `DimensionError` if dimensions do not match.
* `dot_prod`: Calculates the dot product of two vectors.


* **Geometric Operations**
* `angle`: Computes the angle between two vectors in radians using the dot product and magnitudes.



### Error Handling

The module defines a custom exception `DimensionError of string` to handle invalid operations, such as adding vectors of different dimensions or requesting invalid indices for unit vectors.

## Verification

### Automated Testing

The file includes a suite of test cases that run automatically when the module is executed. These tests cover:

* Edge cases (zero dimensions, negative indices).
* Floating point special values (Infinity, NaN).
* Standard arithmetic correctness.
* Geometric property verification.

### Mathematical Proofs

The source code contains detailed comments providing formal proofs for the correctness of the implemented operations. These proofs utilize induction over the vector length to verify properties including:

1. Commutativity of addition.
2. Associativity of addition.
3. Identity of addition and scalar multiplication.
4. Scalar distribution over vector sums.
5. Relationship between length and dot product.
6. Orthogonality conditions.

## Usage

To run the module and execute the embedded test suite, use the OCaml toplevel or compiler:

```bash
ocaml vector.ml

```

## Disclaimer
This code was developed as part of an academic assignment for the COL226 course at IIT Delhi. It is intended for educational purposes and was completed in January 2025, it is no longer being actively updated or maintained, please reach out to me over email or linkedin for any queries. Please cite appropriately if used in research or projects.