There are currently two simplification methods implemented:
* A closed-form simplification
* A complexity minimizing simplification

# Simplify

The 'Simplify' class implements a closed-form simplication. The most common simplifications are implemented as hardcoded rules, which are then executed on a single code-path, leading to the simplified expression.

# SimplifyMin

The 'SimplifyMin' class uses a numerical minimization approach to help simplify expressions. This method starts by giving each mathematical a complexity score (implemented in the 'complexity' method). In addition, this simplification method has access to a list of valid conversions, that can manipulate expressions (for instance, shuffling terms around, or replacing terms in a sum with equivalent terms).

The complexity of expressions is then minimized using a very simple brute-force search algorithm, which searches for the conversion leading to the simplest expression (with the lowest complexity score).

