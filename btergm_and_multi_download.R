
install.packages("devtools")
install.packages("installr")
library(installr)
install.Rtools()
library(devtools)
devtools::install_github("tedhchen/multilayer.ergm", build_opts = c("--no-resave-data", "--no-manual"))


install.packages("C:/Users/Ferran Vega/Documents/R/win-library/3.6/btergm_1.9.4.tar", repos = NULL, type="source")


url <- "https://cran.r-project.org/src/contrib/Archive/btergm/btergm_1.9.4.tar.gz"
pkgFile <- "btergm_1.9.4.tar.gz"
download.file(url = url, destfile = pkgFile)

install.packages("xergm.common")

install.packages(pkgs=pkgFile, type="source", repos=NULL)




url <- "https://cran.r-project.org/src/contrib/Archive/xergm.common/xergm.common_1.7.7.tar.gz"
pkgFile <- "xergm.common_1.7.7.tar.gz"
download.file(url = url, destfile = pkgFile)

install.packages("xergm.common")

install.packages(pkgs=pkgFile, type="source", repos=NULL)"
pkgFile <- "btergm_1.9.4.tar.gz"
download.file(url = url, destfile = pkgFile)

install.packages("xergm.common")

install.packages(pkgs=pkgFile, type="source", repos=NULL)