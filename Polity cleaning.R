rm(list = ls()) # clear environment
library(tidyverse)

Polity <- read.csv("Polity scores.csv", head = TRUE, sep=";") 


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
att$polity1990 <- Polity$polity2_1990[match(att$country, as.character(Polity$country.code))]
att$polity1991 <- Polity$polity2_1991[match(att$country, as.character(Polity$country.code))]
att$polity1992 <- Polity$polity2_1992[match(att$country, as.character(Polity$country.code))]
att$polity1993 <- Polity$polity2_1993[match(att$country, as.character(Polity$country.code))]
att$polity1994 <- Polity$polity2_1994[match(att$country, as.character(Polity$country.code))]
att$polity1995 <- Polity$polity2_1995[match(att$country, as.character(Polity$country.code))]
att$polity1996 <- Polity$polity2_1996[match(att$country, as.character(Polity$country.code))]
att$polity1997 <- Polity$polity2_1997[match(att$country, as.character(Polity$country.code))]
att$polity1998 <- Polity$polity2_1998[match(att$country, as.character(Polity$country.code))]
att$polity1999 <- Polity$polity2_1999[match(att$country, as.character(Polity$country.code))]
att$polity2000 <- Polity$polity2_2000[match(att$country, as.character(Polity$country.code))]
att$polity2001 <- Polity$polity2_2001[match(att$country, as.character(Polity$country.code))]
att$polity2002 <- Polity$polity2_2002[match(att$country, as.character(Polity$country.code))]
att$polity2003 <- Polity$polity2_2003[match(att$country, as.character(Polity$country.code))]
att$polity2004 <- Polity$polity2_2004[match(att$country, as.character(Polity$country.code))]
att$polity2005 <- Polity$polity2_2005[match(att$country, as.character(Polity$country.code))]
