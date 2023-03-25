# Sample flattening / searching functions
This code belongs to [this article](https://www.tfeb.org/fragments/2023/03/27/measuring-some-tree-traversing-functions/).

## To run the benchmarks
You probably cannot unless you are us, as it assumes that you have [`require-module`](https://tfeb.github.io/tfeb-lisp-tools/#requiring-modules-with-searching-require-module), that `needs` is visible in the `Cl-USER` package, and that it knows where the modules it tries to load are.

However all these modules are available in Quicklisp, so it should be quite easy to write a suitable system definition to load all this relying only on Quicklisp.  We have not done this however: this was just experimental code.

Apart from this, and apart from the explicitly LW-dependent files which are separate from the other code, this should be all portable Common Lisp.

Ask Tim if you actually want to run it and are confused.

## To make the pictures
You will need [Racket](https://racket-lang.org/): we used 8.8.
