Exercises from the book:
Haskell Programming from first principles
by Christopher Allen and Julie Moronuki

Use at own risk! Maybe it works, maybe it doesn't...

System related

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

