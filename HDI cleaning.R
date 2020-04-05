rm(list = ls()) # clear environment
library(tidyverse)

HDI <- read.csv("Human Development Index (HDI)_clean.csv", head = TRUE, sep=";") 


#Use borders data to extract clean list of country id's so that I can appropriately match nodes


#Use borders data to extract clean list of country id's so that I can appropriately match nodes

load(file = "borders.mat.rda") 
countries <- colnames(borders.mat)
countries <- countries[-1] 
countries <- sort(countries)
# Remove .na entry, which in borders.mat implies country has no adjacencies/borders

#We can create a new data frame with the row names of the adjacency matrix,
#and match the variables you want as attributes to this new data frame:
attHDI <- data.frame('country' = as.character(countries))
attHDI$HDI1991 <- HDI$X1991[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI1992 <- HDI$X1992[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI1993 <- HDI$X1993[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI1994 <- HDI$X1994[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI1995 <- HDI$X1995[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI1996 <- HDI$X1996[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI1997 <- HDI$X1997[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI1998 <- HDI$X1998[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI1999 <- HDI$X1999[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI2000 <- HDI$X2000[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI2001 <- HDI$X2001[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI2002 <- HDI$X2002[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI2003 <- HDI$X2003[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI2004 <- HDI$X2004[match(attHDI$country, as.character(HDI$country.code))]
attHDI$HDI2005 <- HDI$X2005[match(attHDI$country, as.character(HDI$country.code))]

save(attHDI,file = "HDI.mat.rda")
