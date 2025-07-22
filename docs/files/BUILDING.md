# Building choice

Choice is written with [pack](https://github.com/stefan-hoeck/idris2-pack), the standard Idris package manager. It features no dependencies, apart from those on base, so it should be safely portable across platforms.

## The Makefile
In addition, a Makefile is provided for building, testing, and documenting the code. The targets are as follows:
- `build`: builds the libraries 
- `test`: tests the libraries 
- `docs`: build the docs, and copy them to `./docs`

It also takes the following options 
- `cg`: the code generator, takes whatever options `pack` and `idris` `cg` can take

## Building the paper

The paper, funnily enough, has *far* more dependencies than the library itself. Apart from requiring a standard LaTeX compiler, it also requires these things:
- The font [Noto Serif Hebrew](https://fonts.google.com/noto/specimen/Noto+Serif+Hebrew?preview.layout=grid&query=noto+serif+hebrew) must be installed 
- Similarly, the LaTeX compiler must be compatible with the `fontspec` package
- `biblatex` must function properly 
Apart from this, no special consideration should be needed for building the paper