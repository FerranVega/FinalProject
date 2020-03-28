
rm(list = ls()) # clear environment
library(tidyverse)

GDPpc <- read.csv("GDP_percap_1990to2005_clean.csv", head = TRUE, sep=";") 


#Use borders data to extract clean list of country id's so that I can appropriately match nodes

load(file = "borders.mat.rda") 
countries <- colnames(borders.mat)
countries <- countries[-1] 
countries <- sort(countries)
# Remove .na entry, which in borders.mat implies country has no adjacencies

#We can create a new data frame with the row names of the adjacency matrix,
#and match the variables you want as attributes to this new data frame:
att <- data.frame('countryID' = as.character(countries))

for (i in 1990:2005){
  att$GDPi <- as.character(GDPpc$Xi[match(att$countries, dt$studentID)])
}


