# Latte

    Latte is an imperative language, almost a subset of Java and can be easily translated to it.
    This project is a simple Latte compiler that generates LLVM Assembly output

# Project sturcture and used libraries
    The project has a Makefile for building, src folder for source files, res folder for runtime.bc and concat.bc external files and tests folder for test files. 
    Project also uses Haskell packages missingh and split. Missingh is used for 'repl' function, which replaces occurencies of one string with another. Split is used for 'splitOn' function which splits a string with a given separator and returns an array of strings.

# How to run
    To run the project use Makefile commands
    'make' command will install Haskell packages used in the project using 'cabal install', compile source files and output latc_llvm executable
    'make compile' will only compile sources without installing packages
    'make testGood' will run latc_llvm with all good test files and output .ll and .bc files
    'make testBad' will run latc_llvm with all bad test files and print the error message
