# Business Intelligence - Guía de Trabajos Prácticos con R ^4.0.0
Diplomatura Business Intelligence (BI) - Universidad Tecnlógica Nacional Buenos Aires (UTN)

# Instruccion de instalación con TERMINAL en MacOs Catalina 10.15+:
(Ir a la fuente para Mojave o Windows: https://github.com/sethrfore/homebrew-r-srf.git)

0) Instalar Homebrew:

...% /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

1) ...% brew tap sethrfore/homebrew-r-srf

2) ...% brew info sethrfore/r-srf/r

3) ...% brew install <all dependencies and options>

4) ...% brew info sethrfore/r-srf/r (to confirm it's all there)

5) ...% brew install -s sethrfore/r-srf/r --with-cairo --with-icu4c --with-java --with-libtiff --with-openblas --with-texinfo

6) ...% Install RStudio from its web site with the installer.

7) ...% Update Compilers required by R 4.y.z by following (MacOs Catalina 10.15+):

https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/

8) Start up RStudio and run:

> capabilities()

9) Back in Terminal, clean up Brew downloads cache

...% cd ~/Library/Caches/Homebrew

...% ls

...% cd downloads

...% ls

...% rm '*.*'

...% cd '..'

...% cd Cask

...% ls

...% rm '*.*'

...% cd

...% brew cleanup

...% brew missing
