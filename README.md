# dynahaskell

## Installation

You need to build a copy of GHC with [this diff](https://github.com/isovector/ghc/compare/92b6a0237e0195cee4773de4b237951addd659d9..99ebb81ea5d14434a6c895205aabc4238e22c62a).
I've [already patched it onto GHC-8.6.5](https://github.com/isovector/ghc/tree/SANDY2)
if you'd prefer that, and also [built it](https://drive.google.com/open?id=13o_E9-vCqDeSg2eZaMRYy2LZlutyJWnp)
for linux boxes.

To be honest, I'm not really sure how to install this thing; I just stuck it
into `~/.stack/programs/x86_64-linux/` and pretended it was a real ghc-variant.
Name it `ghc-custom-dyna4-tinfo6`.

You also need a [hacked version of brick](https://github.com/isovector/brick/tree/monadtransformit) downloaded side-by-side.


## Ideas

* Expose the editing actions as an effect
* Describe editing targets via srcspans
* Effect for getting info on srcspans -- what's in scope, what type it is, etc
    (don't actually need a hacked GHC for this)


## High Level

Interfaces and abstraction boundaries! Maybe the OOP guys got it right!

