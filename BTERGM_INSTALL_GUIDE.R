### BTERGM PACKAGE THAT ACTUALLY RUNS THE NETWORK ANALYSIS CANNOT BE INSTALLED
### CONVENTIONALLY.
### JUST RUN THESE 4 LINES TO INSTALL IT. IF IT DOESN'T INSTALL, WE HAVE SAVED THE
### RESULTS OF THE ANALYSIS AS .RDA FILES WHICH CAN BE LOADED INSTEAD TO LOOK AT THE RESULTS.
### THE LOAD COMMANDS ARE IN THE RELEVANT PLACES IN THE R LONG SCRIPT.

url <- "https://cran.r-project.org/src/contrib/Archive/btergm/btergm_1.9.4.tar.gz"
pkgFile <- "btergm_1.9.4.tar.gz"
download.file(url = url, destfile = pkgFile)
install.packages(pkgs=pkgFile, type="source", repos=NULL)
