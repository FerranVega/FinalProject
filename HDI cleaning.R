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
att <- data.frame('country' = as.character(countries))
att$HDI1991 <- HDI$X1991[match(att$country, as.character(HDI$country.code))]
att$HDI1992 <- HDI$X1992[match(att$country, as.character(HDI$country.code))]
att$HDI1993 <- HDI$X1993[match(att$country, as.character(HDI$country.code))]
att$HDI1994 <- HDI$X1994[match(att$country, as.character(HDI$country.code))]
att$HDI1995 <- HDI$X1995[match(att$country, as.character(HDI$country.code))]
att$HDI1996 <- HDI$X1996[match(att$country, as.character(HDI$country.code))]
att$HDI1997 <- HDI$X1997[match(att$country, as.character(HDI$country.code))]
att$HDI1998 <- HDI$X1998[match(att$country, as.character(HDI$country.code))]
att$HDI1999 <- HDI$X1999[match(att$country, as.character(HDI$country.code))]
att$HDI2000 <- HDI$X2000[match(att$country, as.character(HDI$country.code))]
att$HDI2001 <- HDI$X2001[match(att$country, as.character(HDI$country.code))]
att$HDI2002 <- HDI$X2002[match(att$country, as.character(HDI$country.code))]
att$HDI2003 <- HDI$X2003[match(att$country, as.character(HDI$country.code))]
att$HDI2004 <- HDI$X2004[match(att$country, as.character(HDI$country.code))]
att$HDI2005 <- HDI$X2005[match(att$country, as.character(HDI$country.code))]