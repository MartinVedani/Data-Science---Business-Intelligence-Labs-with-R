# Business Intelligence - Guía de Trabajos Prácticos con R ^4.0.0
Diplomatura Business Intelligence (BI) - Universidad Tecnlógica Nacional Buenos Aires (UTN)

# Instruccion de instalación con TERMINAL en MacOs Catalina 10.15+:
(Ir a la fuente para Mojave o Windows: https://github.com/sethrfore/homebrew-r-srf.git)

0) Instalar Homebrew:

...% <code>/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"</code>

1) ...% <code>brew tap sethrfore/homebrew-r-srf</code>

2) ...%<code> brew info sethrfore/r-srf/r</code>

3) ...% <code>brew install _all dependencies and options_ </code>

4) ...% <code>brew info sethrfore/r-srf/r (to confirm it's all there) </code>

5) ...% <code>brew install -s sethrfore/r-srf/r --with-cairo --with-icu4c --with-java --with-libtiff --with-openblas --with-texinfo </code>

6) Install RStudio from its web site with the installer.

7) Update Compilers required by R 4.y.z by following (MacOs Catalina 10.15+):

Fuente: https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/

8) Start up RStudio and run:

> <code>capabilities()</code>

9) Back in Terminal, clean up Brew downloads cache

...% <code>cd ~/Library/Caches/Homebrew</code>

...% <code>ls</code>

...% <code>cd downloads</code>

...% <code>ls</code>

...% <code>rm ``*``.``*``</code>

...% <code>cd ``..``</code>

...% <code>cd Cask</code>

...% <code>ls</code>

...% <code>rm ``*``.``*``</code>

...% <code>cd</code>

...% <code>brew cleanup</code>

...% <code>brew missing</code>
