# Business Intelligence - Guía de Trabajos Prácticos con R ^4.0.0
Diplomatura Business Intelligence (BI) - Universidad Tecnlógica Nacional Buenos Aires (UTN)

El reporte de eCommerce con Simulaciones de Montecarlo del laboratorio 8 - Series de Tiempo II se puede ver en:
https://martinvedani.github.io/Business-Intelligence-Labs-with-R/

# Instruccion de instalación para Windows 10:

1) Bajar y ejecutar instalador .exe con todos las sugerencias default:
https://cran.r-project.org/

2) Instalar R-Tools for Windows con todos las sugerencias default:
https://cran.rstudio.com/bin/windows/Rtools/

3) Instalar RStudio con todos las sugerencias default:

https://rstudio.com/products/rstudio/download/#download

4) Sigue las intrucciones de este tutorial para cambiar la libraría donde se instalarán todos los paquetes de R. Esto ahorrará MUCHO timepo en el futuro al actualizar a nuevas versiones de R:

https://www.accelebrate.com/library/how-to-articles/r-rstudio-library

Es un instructivo para Windows, el archivo Rprofile que hay que modificar para hacer el cambio permanente se encustra en:

<code> C:\Program Files\R\R-4.0.0\library\base\R </code>

Copia y pega lo siguiente al final de documento uytilizando cualquier editor de texto (crea tu propia carpeta "r-library").

<code> #my custom stuff</code><br> 
<code> myPaths <- .libPaths()</code><br>
<code> myPaths <- c(myPaths, "C:/Users/marti/r-library")</code><br>
<code> myPaths <- c(myPaths[3], myPaths[1], myPaths[2])</code><br>
<code> .libPaths(myPaths)</code>

5) Para lops que prefieren un IDE oscuro o gris, pueden ir a:

``>`` <code> Tools -> Global Options -> Appearance -> Editor Theme -> "Material" </code>

6) Instalar LaTex pqra poder exportar prsentacion a pdf, word or HTML con R Markrown. Para esto, en la terminal de RStudio, utilizar el siguiente comando:

``>`` <code> tinytex::install_tinytex() </code>

# Instruccion de instalación con TERMINAL en MacOs Catalina 10.15+:

1) Instalar Homebrew:

Fuente: https://docs.brew.sh/Installation

~ % <code>/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"</code>

2) Intalar R modificado con FULL CAPABILITIES (Java, Open BLASS, Tcl-Tk, Jpeg, Png, Tiff, Cairo, etc.)

Fuente: https://github.com/sethrfore/homebrew-r-srf.git)

~ % <code>brew tap sethrfore/homebrew-r-srf</code>

3) ~ %<code> brew info sethrfore/r-srf/r</code>

4) ~ % <code>brew install _all dependencies and options_ </code>

5) ~ % <code>brew info sethrfore/r-srf/r (to confirm it's all there) </code>

6) FIRST TIME 

~ % <code>brew install -s sethrfore/r-srf/r --with-cairo --with-icu4c --with-java --with-libtiff --with-openblas --with-texinfo </code>

UPDATING VERISON OF R

~ % <code> brew upgrade sethrfore/r-srf/r --with-cairo --with-icu4c --with-java --with-libtiff --with-openblas --with-texinfo </code>

7) Instalar los siguientes paquetes que necesitaremos para compilar algoritmos genéticos y manipular datos geoespaciales para resolver el dilema del Vendedor Ambulante (The Travelling Sales Man dilema)

<code> brew install gdal udunits</code>

8) Instalar RStudio desde su página web.

https://rstudio.com/products/rstudio/download/#download

9) Actualizar los compiladores requeridos por R ^4.0.0 con las instrucciones del siguiente tutorial (MacOs Catalina 10.15+):

Fuente: https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/

10) Iniciar RStudio y ejecutar lo siguiente:

``>`` <code>capabilities()</code>

11) Sigue las intrucciones de este tutorial para cambiar la libraría donde se instalarán todos los paquetes de R. Esto ahorrará MUCHO timepo en el futuro al actualizar a nuevas versiones de R:

https://www.accelebrate.com/library/how-to-articles/r-rstudio-library

Es un instructivo para Windows, en Mac, el archivo Rprofile que hay que modificar para hacer el cambio permanente se encustra en:

<code> /usr/local/Cellar/r/4.0.0/lib/R/library/base/R/Rprofile </code> Hay que buscarlo con Finder. 

Copia y pega lo siguiente al final de documento uytilizando cualquier editor de texto (crea tu propia carpeta "r-library").

<code> #my custom stuff</code><br> 
<code> myPaths <- .libPaths()</code><br>
<code> myPaths <- c(myPaths, "/Users/martin/r-library")</code><br>
<code> myPaths <- c(myPaths[3], myPaths[1], myPaths[2])</code><br>
<code> .libPaths(myPaths)</code>

12) Para los que prefieren un IDE oscuro o gris, pueden ir a:

<code> Tools -> Global Options -> Appearance -> Editor Theme -> "Material" </code>

13) Instalar LaTex pqra poder exportar prsentacion a pdf, word or HTML con R Markrown. Para esto, en la terminal de RStudio, utilizar el siguiente comando:

``>`` <code> tinytex::install_tinytex() </code>

14) De vuelta en la Terminal, limpiar el cache de Brew downloads

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

--------------------------------------------------------------------
# BONUS: 

library(ctv) es súper interesante, puedes leer sobre ella brevemente en https://cran.r-project.org/web/packages/ctv/index.html 

Lo más interesante son los listado por vistas publicados en: https://cran.r-project.org/web/views/
