
rm(list = ls()) # clear environment
library(tidyverse)

GDPpc <- read.csv("GDP_percap_1990to2005_clean.csv", head = TRUE, sep=";") 


#Use borders data to extract clean list of country id's so that I can appropriately match nodes

load(file = "borders.mat.rda") 
countries <- colnames(borders.mat)
countries <- countries[-1] 
countries <- sort(countries)
# Remove .na entry, which in borders.mat implies country has no adjacencies/borders

#We can create a new data frame with the row names of the adjacency matrix,
#and match the variables you want as attributes to this new data frame:
attGDPpc <- data.frame('country' = as.character(countries))
attGDPpc$GDP1990 <- GDPpc$X1990[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP1991 <- GDPpc$X1991[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP1992 <- GDPpc$X1992[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP1993 <- GDPpc$X1993[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP1994 <- GDPpc$X1994[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP1995 <- GDPpc$X1995[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP1996 <- GDPpc$X1996[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP1997 <- GDPpc$X1997[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP1998 <- GDPpc$X1998[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP1999 <- GDPpc$X1999[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP2000 <- GDPpc$X2000[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP2001 <- GDPpc$X2001[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP2002 <- GDPpc$X2002[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP2003 <- GDPpc$X2003[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP2004 <- GDPpc$X2004[match(attGDPpc$country, as.character(GDPpc$Country.Code))]
attGDPpc$GDP2005 <- GDPpc$X2005[match(attGDPpc$country, as.character(GDPpc$Country.Code))]




