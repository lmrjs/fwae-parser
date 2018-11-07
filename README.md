# FWAE Parser

## Getting Started
1. Install the [Haskell Platform](https://www.haskell.org/platform/). The core installer will be sufficient.
2. In the `fwae-parser` directory, run `stack build` to install the GHC compiler and build the project.
  - If there are issues with installing `ghc-vis`, see the [documentation](http://felsin9.de/nnis/ghc-vis/#installation) and [this issue](https://github.com/def-/ghc-vis/issues/13). Windows users should run the following:
    
    `stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-gtk3 mingw-w64-x86_64-gtk2 wget unzip`
    
    `stack exec -- echo 'export PATH=/c/graphviz/bin:$PATH' >> ~/.bashrc`
3. Run `stack exec fwae-parser-exe` to run the project.
