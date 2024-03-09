# stella-hs
Stella type checker written on haskell. Itmo languages and type systems course

## How to use
- Tested version of toolchain
  * Cabal – versions 3.10.2.1 and 3.8.10, but there should be no problems with other versions of the cabal. Stack has not been tested
  * GHC - version 9.2.5, 9.4.8,
- Tests
    * To test all programs use
        ```bash
        cabal test
        ```
    * To test correct programs use
        ```bash
        cabal test --test-options='-p Ok'
        ```
    * To tests invalid programs use
        ```bash
        cabal test --test-options='-p Bad'
        ```
- Running type checker
    * With cabal
    ```bash
    cabal run stella-hs -- {path_to_stella_file}
    ```
    Examples
    ```bash
    $ cabal run stella-hs -- test/stella-tests/bad/ERROR_NONEXHAUSTIVE_MATCH_PATTERNS/ne_bool.st

    $ stella-hs -- test/stella-tests/ok/fixpoint.st
    ```
