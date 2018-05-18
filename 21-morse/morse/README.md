# Morse

# Project Creation Steps

```
stack new morse simple
cd morse
stack setup
```

## Usage

#### Examples

```
echo "hi" | stack exec morse to
echo ".... .." | stack exec morse from
```

#### Build and Run

```
> stack build
> stack ghci
*Main>  main
```

Or, to run, from the project root directory:

```
stack exec -- morse
```

#### Test

```
$ stack ghci morse:tests 
...
*Main> main
+++ OK, passed 100 tests.
*Main>

```


