# Business Intelligence - Guía de Trabajos Prácticos con R
Diplomatura Business Intelligence (BI) - Universidad Tecnlógica Nacional Buenos Aires (UTN)

# Instruccion de instalación para Windows 10/11:

1) Bajar y ejecutar instalador .exe con todos las sugerencias default:
https://cran.r-project.org/

2) Instalar R-Tools for Windows con todos las sugerencias default:
https://cran.rstudio.com/bin/windows/Rtools/

3) Instalar RStudio con todos las sugerencias default:

https://rstudio.com/products/rstudio/download/#download

4) Lanza RStudio (siempre hay que hacerlo correr como administrador).

5) Para los que prefieren un IDE oscuro o gris, pueden ir a:

``>`` <code> Tools -> Global Options -> Appearance -> Editor Theme -> "Material" </code>

6) Instalar LaTex pqra poder exportar prsentacion a pdf, word or HTML con R Markrown. Para esto, en la terminal de RStudio, utilizar el siguiente comando:

``>`` <code> install.packages("tinytex") </code><br>
``>`` <code> tinytex::install_tinytex() </code>

7) Instalar el paquete de actualizacion de R en el futuro:

<code>https://digitalfellows.commons.gc.cuny.edu/2022/10/25/how-to-update-r-from-r-using-the-installr-package/</code><br>
``>`` <code> install.packages("installr") </code><br>
``>`` <code> installr::updateR() </code>

8) Instalar paquetes de Finanzas, Econometria, Estadistica, etc.

https://cran.r-project.org/web/packages/ctv/index.html 

Lo más interesante son los listado por vistas publicados en: https://cran.r-project.org/web/views/

``>`` <code> install.packages("ctv") </code><br>
``>`` <code> ctv::update.views(c("DifferentialEquations", "Robust", "Optimization", "OfficialStatistics", "NumericalMathematics","Finance","Econometrics", "TimeSeries","OfficialStatistics", "MissingData","GraphicalModels","ExtremeValue","Finance"), coreOnly=TRUE) </code>
   
# Instruccion de instalación con TERMINAL en MacOs:

1) Instalar Homebrew:

Fuente: https://docs.brew.sh/Installation
  
2) ~ % <code> brew install --cask r </code>

3) Instalar los siguientes paquetes que necesitaremos para compilar algoritmos genéticos y manipular datos geoespaciales para resolver el dilema del Vendedor Ambulante (The Travelling Sales Man dilema)

<code> brew install gdal udunits</code>

4) Instalar RStudio desde su página web.

https://rstudio.com/products/rstudio/download/#download

5) Actualizar los compiladores requeridos por R con las instrucciones del siguiente tutorial (MacOs Catalina 10.15+):

Fuente: https://thecoatlessprofessor.com/programming/cpp/r-compiler-tools-for-rcpp-on-macos/

6) Iniciar RStudio y ejecutar lo siguiente:

``>`` <code> capabilities() </code>
  
 
  ------ METODO ALTERNATIVO SI LAS CAPABILITIES usando el compando capabilities() FALLA -----

~ % <code> /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" </code>

1) Intalar R modificado con FULL CAPABILITIES (OpenJDK, Open BLASS, Tcl-Tk, Jpeg, Png, Tiff, Cairo, etc.)

Fuente: https://github.com/sethrfore/homebrew-r-srf.git)

~ % <code>brew tap sethrfore/homebrew-r-srf</code>

2) ~ %<code> brew info sethrfore/r-srf/r</code>

3) ~ % <code>brew install _all dependencies and options_ </code>

4) ~ % <code>brew info sethrfore/r-srf/r (to confirm it's all there) </code>

5) PRIMERA INSTALACIÓN 
  
~ % <code> brew install -s sethrfore/r-srf/r --with-cairo-x11 --with-icu4c --with-libtiff --with-openblas --with-openjdk --with-tcl-tk-x11 --with-texinfo </code>

ACTUALIZACIÓN DE R CUANDO YA SE ENCUENTRA INSTALADO

  [if brew install --cask r (step 6) fails to get all capabilities()] 
~ % <code> brew upgrade -s sethrfore/r-srf/r --with-cairo-x11 --with-icu4c --with-libtiff --with-openblas --with-openjdk --with-tcl-tk-x11 --with-texinfo </code>

6) Limpiar el cache de Brew downloads

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

7) Iniciar RStudio y ejecutar lo siguiente:

``>`` <code> capabilities() </code>
  
8) Para los que prefieren un IDE oscuro o gris, pueden ir a:

<code> Tools -> Global Options -> Appearance -> Editor Theme -> "Material" </code>

9) Instalar LaTex pqra poder exportar prsentacion a pdf, word or HTML con R Markrown. Para esto, en la terminal de RStudio, utilizar el siguiente comando:

``>`` <code> install.packages("tinytex") </code><br>
``>`` <code> tinytex::install_tinytex() </code>

10) Instalar paquetes de Finanzas, Econometria, Estadistica, etc.

https://cran.r-project.org/web/packages/ctv/index.html 

Lo más interesante son los listado por vistas publicados en: https://cran.r-project.org/web/views/

``>`` <code> install.packages("ctv") </code><br>
``>`` <code> ctv::update.views(c("DifferentialEquations", "Robust", "Optimization", "OfficialStatistics", "NumericalMathematics","Finance","Econometrics", "TimeSeries"), coreOnly=TRUE) </code>
