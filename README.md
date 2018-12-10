# Latte language compiler

This project contains only parsing, static type check and error handling so far.
The application prints the analysis output to the standard out.

## Compiler

    ./latc

## Tests

    ./tester # runs tests from gTest/
    ./tester <tests directory>

The analyser is not working correctly yet with regard to returning in other
places than the outermost block of the function.
This will change in the next iteration.
