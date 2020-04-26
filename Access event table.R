
setwd("C:/Users/Ferran Vega/OneDrive/Math 23 C/Final Project")

load("C:/Users/Ferran Vega/OneDrive/Math 23 C/Final project/1990-1995 IDEA Event Forms.RData")
load("C:/Users/Ferran Vega/OneDrive/Math 23 C/Final project/1995-1999 IDEA Event Forms.RData")
load("C:/Users/Ferran Vega/OneDrive/Math 23 C/Final project/2000-2004 IDEA Event Forms.RData")

# Loading all three produce the same table? This event table is good for reference but also
# cross-check with .txt file
library(dils)
library(dplyr)

events1 <- select(x, "CLASS", "NAME", "DESCRIPT")
events1 <- events1[order(events1$CLASS),]

