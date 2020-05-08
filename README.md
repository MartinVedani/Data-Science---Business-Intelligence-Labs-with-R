# Business Intelligence - Guía de Trabajos Prácticos con R ^4.0.0
Diplomatura Business Intelligence (BI) - Universidad Tecnlógica Nacional Buenos Aires (UTN)

# Instruccion de instalación con TERMINAL en MacOs Catalina 10.15+:
(Ir a la fuente para Mojave o Windows: https://github.com/sethrfore/homebrew-r-srf.git)

0) Instalar Homebrew:

~ % <code>/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"</code>

1) ~ % <code>brew tap sethrfore/homebrew-r-srf</code>

2) ~ %<code> brew info sethrfore/r-srf/r</code>

3) ~ % <code>brew install _all dependencies and options_ </code>

4) ~ % <code>brew info sethrfore/r-srf/r (to confirm it's all there) </code>

5) ~ % <code>brew install -s sethrfore/r-srf/r --with-cairo --with-icu4c --with-java --with-libtiff --with-openblas --with-texinfo </code>

6) Instalar RStudio desde su página web.

7) Actualizar los compiladores requeridos por R ^4.0.0 con las instrucciones del siguiente tutorial (MacOs Catalina 10.15+):

Fuente: https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/

8) Iniciar RStudio y ejecutar lo siguiente:

``>`` <code>capabilities()</code>

9) Sigue las intrucciones de este tutorial par acambiar la libraria donde se instalarán todos los paquetes de R. eSto ahorrará MUCHO timepo en el futuro al actualizar a nuevas versiones de R:

https://www.accelebrate.com/library/how-to-articles/r-rstudio-library

Es un instructivo para Windows, en Mac, el archivo Rprofile que hay que modificar para hacer el cambio permanente se encustra en:

<code> cd /usr/local/Cellar/r/4.0.0/lib/R/library/base/R/Rprofile </code>

10) De vuelta en la Terminal, limpiar el cache de Brew downloads

~ % <code>cd ~/Library/Caches/Homebrew</code>

~ % <code>ls</code>

~ % <code>cd downloads</code>

~ % <code>ls</code>

~ % <code>rm ``*``.``*``</code>

~ % <code>cd ``..``</code>

~ % <code>cd Cask</code>

~ % <code>ls</code>

~ % <code>rm ``*``.``*``</code>

~ % <code>cd</code>

~ % <code>brew cleanup</code>

~ % <code>brew missing</code>
