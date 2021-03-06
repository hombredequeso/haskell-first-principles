Exercises from the book:
Haskell Programming from first principles
by Christopher Allen and Julie Moronuki

Use at own risk! Maybe it works, maybe it doesn't...

## Miscellaneous Hints

### Holes
The wildcard character (underscore) is a helpful tool in the development process.
Most simply, it is possible to write the general form of a function, but put underscores in places where the type might be unclear, or even where it is just unclear what precisely should be done at any point. Compiling will then provide a helpful error about the type required to be filled in.
Beyond simple use of the underscore, underscore and some text also works, making it possible to have more than one hole (unknown) in the file.
For example:
---
sum :: (Foldable t, Num a) => t a -> a
sum = \_sumIt
---

Results in:
Prelude> :l 20.library-functions.2.hs
[1 of 1] Compiling LibraryFunctions20 ( 20.library-functions.2.hs, interpreted )

20.library-functions.2.hs:9:8: error:
    * Found hole: _sumIt :: t a -> a
    ... blah blah blah, potentially more useful error information here ...
    * Relevant bindings include
        sum' :: t a -> a (bound at 20.library-functions.2.hs:9:1)
  |
9 | sum' = _sumIt
  |        ^^^^^^
Failed, no modules loaded.
Prelude>

### Pointfree

[ Pointfree ](https://wiki.haskell.org/Pointfree) code does not explicitly mention function arguments. It is commonly used in functional languages. The [pointfree.io](http://pointfree.io/) site is a useful tool in learning Haskell, allowing you to type in your non-pointfree code and see what its most succinct pointfree form would be.

## System related

To get QuickCheck.Checkers and QuickCheck.Classes to work:

---
> cabal update
> cabal install checkers

It is possible to check the packages that are available to ghci using:
> cabal list --installed
Since that is likely to be a very long list, you may want to:
> cabal list --installed | grep checkers
And if you need information about a module, try:
> cabal info checkers
---
 
If 'cabal update' gives a 'failed to download error', try:

```
cabal --http-transport=plain-http update

```

Note that if using cabal to install packages it is typically required to exit ghci and restart it, as the cabal packages seem to be read in once, at startup time.

## Prerequisites
The main prerequisite is the ghci compiler.
Probably the simplest way to get started is to install "Haskell Platform"
See [Haskell Downloads]( https://www.haskell.org/downloads )

Currently, management of libraries is done manually using [Cabal](https://www.haskell.org/cabal/).

Helpful cabal commands include:
- List packages downloaded locally:
> cabal list --installed
- Install 'packageX'
> cabal install packageX

If having problems figuring out the name of the package to install, check out (How are Hackage package names mapped to 'cabal install' names?)[https://stackoverflow.com/a/7422549] for a very quick, helpful overview.


## Running Exercises

Generally each of the exercise files stands-alone. For instance

---
> cd exercises
> ghci
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
Prelude> :l 20.library-functions.hs
[1 of 1] Compiling LibraryFunctions20 ( 20.library-functions.hs, interpreted )
Ok, one module loaded.
\*LibraryFunctions20> sum' [1,2,3]
6
\*LibraryFunctions20> :q
>
Leaving GHCi.
---

## References

The book commonly known as "The Haskell Book":
[Christopher Allen and Julie Moronuki, _Haskell Programming From First Principles._](http://haskellbook.com/)
Similar repositories that I found helpful:

* https://github.com/dmvianna/haskellbook
* https://github.com/johnmendonca/haskellbook-exercises

See also:

* (subreddit for asking questions related to learning Haskell through the Haskell Book)[https://www.reddit.com/r/HaskellBook/]

Other helpful links
* (WHAT I WISH I KNEW WHEN LEARNING HASKELL)[http://dev.stephendiehl.com/hask/]
    A _massive_ collection of helpful reference information. 
* (How are Hackage package names mapped to 'cabal install' names?)[https://stackoverflow.com/a/7422549]
    If you are confused about the relationship between packages and modules, and specifically wondering what to use when doing a 'cabal install'


