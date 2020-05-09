# Insights in International Relations - Consolidated R Script
# Mohit and Ferran
# 05/08/2020

##########################################################################################################
# SET UP
##########################################################################################################

rm(list = ls()) # clear environment

library(tidyverse)
#install.packages("plyr") # Install if required!
library(plyr)
library(dils)
library(data.table)
library(dplyr)
library(lubridate)
library(Renext)
library(ggplot2)
library(EWGoF)
library(statnet)
library(btergm)
library(beepr)
library(gridExtra) 
library(igraph)
library(tibble)
library(grid)
library(gridBase)
#install.packages("ggthemes") # Install if required!
library(ggthemes)
#install.packages("stargazer") # Install if required!
library(stargazer)


##########################################################################################################
# DATA PREPARATION 1 (Cleaning of covariates used in the various analyses)
##########################################################################################################

## Borders

load(file = "data20.rda") ; load("data90.rda") ; load("data95.rda")

data20 <- select(data20,"EventDate", "SrcName","TgtName")
data95 <- select(data95,"EventDate", "SrcName","TgtName")
data90 <- select(data90,"EventDate", "SrcName","TgtName")

nodes20 <- as_tibble(union(levels(droplevels(data20$SrcName)),levels(droplevels(data20$TgtName))))
nodes95 <- as_tibble(union(levels(droplevels(data95$SrcName)),levels(droplevels(data95$TgtName))))
nodes90 <- as_tibble(union(levels(droplevels(data90$SrcName)),levels(droplevels(data90$TgtName))))

nodes <- rbind(nodes20,nodes95,nodes90)
nodes <- unique(nodes)

borders <- read.csv("borders.csv", head = TRUE, sep=";", colClasses = "character")  # load borders data
head(borders)

# Making borders acronyms consistent

fct_explicit_na(borders$country_code, na_level = ".na")

borders$country_code <- as.character(borders$country_code)

# Remove entries where the country is one that does not appear in dyadic data
remove <- which(borders$country_code == "AS" |
                  borders$country_code == "AX" |
                  borders$country_code == "BL" |
                  borders$country_code == "BQ" |
                  borders$country_code == "BV" |
                  borders$country_code == "CX" |
                  borders$country_code == "DJ" |
                  borders$country_code == "GS" |
                  borders$country_code == "HK" |
                  borders$country_code == "HM" |
                  borders$country_code == "IO" |
                  borders$country_code == "JE" |
                  borders$country_code == "MF" |
                  borders$country_code == "MH" |
                  borders$country_code == "MM" |
                  borders$country_code == "MO" |
                  borders$country_code == "PM" |
                  borders$country_code == "PN" |
                  borders$country_code == "SX" |
                  borders$country_code == "SZ" |
                  borders$country_code == "TC" |
                  borders$country_code == "TF" |
                  borders$country_code == "TL" |
                  borders$country_code == "UM" |
                  borders$country_code == "VI" |  
                  borders$country_code == "VG" |
                  borders$country_code == "WF" |
                  borders$country_code == "YT")

borders <- borders[-c(remove),]



# Remove entries where the bordering country is one that does not appear in dyadic data
remove1 <- which(borders$country_border_code == "AS" |
                   borders$country_border_code == "AX" |
                   borders$country_border_code == "BL" |
                   borders$country_border_code == "BQ" |
                   borders$country_border_code == "BV" |
                   borders$country_border_code == "CX" |
                   borders$country_border_code == "DJ" |
                   borders$country_border_code == "GS" |
                   borders$country_border_code == "HK" |
                   borders$country_border_code == "HM" |
                   borders$country_border_code == "IO" |
                   borders$country_border_code == "JE" |
                   borders$country_border_code == "MF" |
                   borders$country_border_code == "MH" |
                   borders$country_border_code == "MM" |
                   borders$country_border_code == "MO" |
                   borders$country_border_code == "PM" |
                   borders$country_border_code == "PN" |
                   borders$country_border_code == "SX" |
                   borders$country_border_code == "SZ" |
                   borders$country_border_code == "TC" |
                   borders$country_border_code == "TF" |
                   borders$country_border_code == "TL" |
                   borders$country_border_code == "UM" |
                   borders$country_border_code == "VI" |   
                   borders$country_border_code == "VG" | 
                   borders$country_border_code == "WF" |
                   borders$country_border_code == "YT")

borders <- borders[-c(remove1),]

# Now proceed to harmonize country codes with country codes in dyadic data

borders$country_code[borders$country_code == "AD"] <- "AND" 
borders$country_border_code[borders$country_border_code == "AD"] <- "AND" 

borders$country_code[borders$country_code == "AE"] <- "UAE" 
borders$country_border_code[borders$country_border_code == "AE"] <- "UAE" 

borders$country_code[borders$country_code == "AF"] <- "AFG" 
borders$country_border_code[borders$country_border_code == "AF"] <- "AFG" 

borders$country_code[borders$country_code == "AG"] <- "ANT" 
borders$country_border_code[borders$country_border_code == "AG"] <- "ANT" 

borders$country_code[borders$country_code == "AI"] <- "ANL" 
borders$country_border_code[borders$country_border_code == "AI"] <- "ANL" 

borders$country_code[borders$country_code == "AL"] <- "ALB" 
borders$country_border_code[borders$country_border_code == "AL"] <- "ALB" 

borders$country_code[borders$country_code == "AM"] <- "ARM" 
borders$country_border_code[borders$country_border_code == "AM"] <- "ARM" 

borders$country_code[borders$country_code == "AO"] <- "ANG" 
borders$country_border_code[borders$country_border_code == "AO"] <- "ANG" 

borders$country_code[borders$country_code == "AQ"] <- "_ARC" 
borders$country_border_code[borders$country_border_code == "AQ"] <- "_ARC" 

borders$country_code[borders$country_code == "AR"] <- "ARG" 
borders$country_border_code[borders$country_border_code == "AR"] <- "ARG" 

borders$country_code[borders$country_code == "AT"] <- "AUS" 
borders$country_border_code[borders$country_border_code == "AT"] <- "AUS" 

borders$country_code[borders$country_code == "AU"] <- "AUL" 
borders$country_border_code[borders$country_border_code == "AU"] <- "AUL" 

borders$country_code[borders$country_code == "AW"] <- "ARU" 
borders$country_border_code[borders$country_border_code == "AW"] <- "ARU" 

borders$country_code[borders$country_code == "AZ"] <- "AZE" 
borders$country_border_code[borders$country_border_code == "AZ"] <- "AZE" 

#####

borders$country_code[borders$country_code == "BA"] <- "BOS" 
borders$country_border_code[borders$country_border_code == "BA"] <- "BOS" 

borders$country_code[borders$country_code == "BB"] <- "BAR" 
borders$country_border_code[borders$country_border_code == "BB"] <- "BAR" 

borders$country_code[borders$country_code == "BD"] <- "BNG" 
borders$country_border_code[borders$country_border_code == "BD"] <- "BNG" 

borders$country_code[borders$country_code == "BE"] <- "BEL" 
borders$country_border_code[borders$country_border_code == "BE"] <- "BEL" 

borders$country_code[borders$country_code == "BF"] <- "BFO" 
borders$country_border_code[borders$country_border_code == "BF"] <- "BFO" 

borders$country_code[borders$country_code == "BG"] <- "BUL" 
borders$country_border_code[borders$country_border_code == "BG"] <- "BUL" 

borders$country_code[borders$country_code == "BH"] <- "BAH" 
borders$country_border_code[borders$country_border_code == "BH"] <- "BAH" 

borders$country_code[borders$country_code == "BI"] <- "BUI" 
borders$country_border_code[borders$country_border_code == "BI"] <- "BUI" 

borders$country_code[borders$country_code == "BJ"] <- "BEN" 
borders$country_border_code[borders$country_border_code == "BJ"] <- "BEN" 

borders$country_code[borders$country_code == "BM"] <- "BER" 
borders$country_border_code[borders$country_border_code == "BM"] <- "BER" 

borders$country_code[borders$country_code == "BN"] <- "BRU" 
borders$country_border_code[borders$country_border_code == "BN"] <- "BRU" 

borders$country_code[borders$country_code == "BO"] <- "BOL" 
borders$country_border_code[borders$country_border_code == "BO"] <- "BOL" 

borders$country_code[borders$country_code == "BR"] <- "BRA" 
borders$country_border_code[borders$country_border_code == "BR"] <- "BRA" 

borders$country_code[borders$country_code == "BS"] <- "BHM" 
borders$country_border_code[borders$country_border_code == "BS"] <- "BHM" 

borders$country_code[borders$country_code == "BT"] <- "BHU" 
borders$country_border_code[borders$country_border_code == "BT"] <- "BHU" 

borders$country_code[borders$country_code == "BW"] <- "BOT" 
borders$country_border_code[borders$country_border_code == "BW"] <- "BOT" 

borders$country_code[borders$country_code == "BY"] <- "BLR" 
borders$country_border_code[borders$country_border_code == "BY"] <- "BLR" 

borders$country_code[borders$country_code == "BZ"] <- "BLZ" 
borders$country_border_code[borders$country_border_code == "BZ"] <- "BLZ" 

borders$country_code[borders$country_code == "CA"] <- "CAN" 
borders$country_border_code[borders$country_border_code == "CA"] <- "CAN" 

borders$country_code[borders$country_code == "CC"] <- "COI" 
borders$country_border_code[borders$country_border_code == "CC"] <- "COI" 

borders$country_code[borders$country_code == "CD"] <- "ZAI" 
borders$country_border_code[borders$country_border_code == "CD"] <- "ZAI" 

borders$country_code[borders$country_code == "CF"] <- "CEN" 
borders$country_border_code[borders$country_border_code == "CF"] <- "CEN" 

borders$country_code[borders$country_code == "CG"] <- "CON" 
borders$country_border_code[borders$country_border_code == "CG"] <- "CON" 

borders$country_code[borders$country_code == "CH"] <- "SWZ" 
borders$country_border_code[borders$country_border_code == "CH"] <- "SWZ" 

borders$country_code[borders$country_code == "CI"] <- "IVO" 
borders$country_border_code[borders$country_border_code == "CI"] <- "IVO" 

borders$country_code[borders$country_code == "CK"] <- "COO" 
borders$country_border_code[borders$country_border_code == "CK"] <- "COO" 

######

borders$country_code[borders$country_code == "CL"] <- "CHL" 
borders$country_border_code[borders$country_border_code == "CL"] <- "CHL" 

borders$country_code[borders$country_code == "CM"] <- "CAO" 
borders$country_border_code[borders$country_border_code == "CM"] <- "CAO" 

borders$country_code[borders$country_code == "CN"] <- "CHN" 
borders$country_border_code[borders$country_border_code == "CN"] <- "CHN" 

borders$country_code[borders$country_code == "CO"] <- "COL" 
borders$country_border_code[borders$country_border_code == "CO"] <- "COL" 

borders$country_code[borders$country_code == "CR"] <- "COS" 
borders$country_border_code[borders$country_border_code == "CR"] <- "COS" 

borders$country_code[borders$country_code == "CU"] <- "CUB" 
borders$country_border_code[borders$country_border_code == "CU"] <- "CUB" 

borders$country_code[borders$country_code == "CV"] <- "CAP" 
borders$country_border_code[borders$country_border_code == "CV"] <- "CAP" 

borders$country_code[borders$country_code == "CW"] <- "CUR" 
borders$country_border_code[borders$country_border_code == "CW"] <- "CUR" 

borders$country_code[borders$country_code == "CY"] <- "CYP" 
borders$country_border_code[borders$country_border_code == "CY"] <- "CYP" 

borders$country_code[borders$country_code == "CZ"] <- "CZR" 
borders$country_border_code[borders$country_border_code == "CZ"] <- "CZR" 

borders$country_code[borders$country_code == "DE"] <- "FRG" 
borders$country_border_code[borders$country_border_code == "DE"] <- "FRG" 

borders$country_code[borders$country_code == "DK"] <- "DEN" 
borders$country_border_code[borders$country_border_code == "DK"] <- "DEN" 

borders$country_code[borders$country_code == "DM"] <- "DMI" 
borders$country_border_code[borders$country_border_code == "DM"] <- "DMI" 

borders$country_code[borders$country_code == "DO"] <- "DOM" 
borders$country_border_code[borders$country_border_code == "DO"] <- "DOM" 

borders$country_code[borders$country_code == "DZ"] <- "ALG" 
borders$country_border_code[borders$country_border_code == "DZ"] <- "ALG" 

borders$country_code[borders$country_code == "EC"] <- "ECU" 
borders$country_border_code[borders$country_border_code == "EC"] <- "ECU" 

borders$country_code[borders$country_code == "EE"] <- "EST" 
borders$country_border_code[borders$country_border_code == "EE"] <- "EST" 

borders$country_code[borders$country_code == "EG"] <- "EGY" 
borders$country_border_code[borders$country_border_code == "EG"] <- "EGY" 

borders$country_code[borders$country_code == "EH"] <- "WSA" 
borders$country_border_code[borders$country_border_code == "EH"] <- "WSA" 

borders$country_code[borders$country_code == "ER"] <- "ERI" 
borders$country_border_code[borders$country_border_code == "ER"] <- "ERI" 

borders$country_code[borders$country_code == "ES"] <- "SPN" 
borders$country_border_code[borders$country_border_code == "ES"] <- "SPN" 

borders$country_code[borders$country_code == "ET"] <- "ETH" 
borders$country_border_code[borders$country_border_code == "ET"] <- "ETH" 

borders$country_code[borders$country_code == "FI"] <- "FIN" 
borders$country_border_code[borders$country_border_code == "FI"] <- "FIN" 

########

borders$country_code[borders$country_code == "FJ"] <- "FJI" 
borders$country_border_code[borders$country_border_code == "FJ"] <- "FJI" 

borders$country_code[borders$country_code == "FK"] <- "FAL" 
borders$country_border_code[borders$country_border_code == "FK"] <- "FAL" 

borders$country_code[borders$country_code == "FM"] <- "FMS" 
borders$country_border_code[borders$country_border_code == "FM"] <- "FMS" 

borders$country_code[borders$country_code == "FO"] <- "FAI" 
borders$country_border_code[borders$country_border_code == "FO"] <- "FAI" 

borders$country_code[borders$country_code == "FR"] <- "FRN" 
borders$country_border_code[borders$country_border_code == "FR"] <- "FRN" 

borders$country_code[borders$country_code == "GA"] <- "GAB" 
borders$country_border_code[borders$country_border_code == "GA"] <- "GAB" 

borders$country_code[borders$country_code == "GB"] <- "_UK" 
borders$country_border_code[borders$country_border_code == "GB"] <- "_UK" 

borders$country_code[borders$country_code == "GD"] <- "GRN" 
borders$country_border_code[borders$country_border_code == "GD"] <- "GRN" 

borders$country_code[borders$country_code == "GE"] <- "GRG" 
borders$country_border_code[borders$country_border_code == "GE"] <- "GRG" 

borders$country_code[borders$country_code == "GF"] <- "FGU" 
borders$country_border_code[borders$country_border_code == "GF"] <- "FGU" 

borders$country_code[borders$country_code == "GG"] <- "GUE" 
borders$country_border_code[borders$country_border_code == "GG"] <- "GUE" 

borders$country_code[borders$country_code == "GH"] <- "GHA" 
borders$country_border_code[borders$country_border_code == "GH"] <- "GHA" 

borders$country_code[borders$country_code == "GI"] <- "GIB" 
borders$country_border_code[borders$country_border_code == "GI"] <- "GIB" 

borders$country_code[borders$country_code == "GL"] <- "GNL" 
borders$country_border_code[borders$country_border_code == "GL"] <- "GNL" 

borders$country_code[borders$country_code == "GM"] <- "GAM" 
borders$country_border_code[borders$country_border_code == "GM"] <- "GAM" 

borders$country_code[borders$country_code == "GN"] <- "GUI" 
borders$country_border_code[borders$country_border_code == "GN"] <- "GUI" 

borders$country_code[borders$country_code == "GP"] <- "GDL" 
borders$country_border_code[borders$country_border_code == "GP"] <- "GDL" 

borders$country_code[borders$country_code == "GQ"] <- "EQG" 
borders$country_border_code[borders$country_border_code == "GQ"] <- "EQG" 

borders$country_code[borders$country_code == "GR"] <- "GRC" 
borders$country_border_code[borders$country_border_code == "GR"] <- "GRC" 

borders$country_code[borders$country_code == "GT"] <- "GUA" 
borders$country_border_code[borders$country_border_code == "GT"] <- "GUA" 

#####

borders$country_code[borders$country_code == "GU"] <- "GUM" 
borders$country_border_code[borders$country_border_code == "GU"] <- "GUM" 

borders$country_code[borders$country_code == "GW"] <- "GNB" 
borders$country_border_code[borders$country_border_code == "GW"] <- "GNB" 

borders$country_code[borders$country_code == "GY"] <- "GUY" 
borders$country_border_code[borders$country_border_code == "GY"] <- "GUY" 

borders$country_code[borders$country_code == "HN"] <- "HON" 
borders$country_border_code[borders$country_border_code == "HN"] <- "HON" 

borders$country_code[borders$country_code == "HR"] <- "CRO" 
borders$country_border_code[borders$country_border_code == "HR"] <- "CRO" 

borders$country_code[borders$country_code == "HT"] <- "HAI" 
borders$country_border_code[borders$country_border_code == "HT"] <- "HAI" 

borders$country_code[borders$country_code == "HU"] <- "HUN" 
borders$country_border_code[borders$country_border_code == "HU"] <- "HUN" 

borders$country_code[borders$country_code == "ID"] <- "INS" 
borders$country_border_code[borders$country_border_code == "ID"] <- "INS" 

borders$country_code[borders$country_code == "IE"] <- "IRE" 
borders$country_border_code[borders$country_border_code == "IE"] <- "IRE" 

borders$country_code[borders$country_code == "IL"] <- "ISR" 
borders$country_border_code[borders$country_border_code == "IL"] <- "ISR" 

borders$country_code[borders$country_code == "IM"] <- "IOM" 
borders$country_border_code[borders$country_border_code == "IM"] <- "IOM" 

borders$country_code[borders$country_code == "IN"] <- "IND" 
borders$country_border_code[borders$country_border_code == "IN"] <- "IND" 

borders$country_code[borders$country_code == "IQ"] <- "IRQ" 
borders$country_border_code[borders$country_border_code == "IQ"] <- "IRQ" 

borders$country_code[borders$country_code == "IR"] <- "IRN" 
borders$country_border_code[borders$country_border_code == "IR"] <- "IRN" 

borders$country_code[borders$country_code == "IS"] <- "ICE" 
borders$country_border_code[borders$country_border_code == "IS"] <- "ICE" 

borders$country_code[borders$country_code == "IT"] <- "ITA" 
borders$country_border_code[borders$country_border_code == "IT"] <- "ITA"

#####

borders$country_code[borders$country_code == "JM"] <- "JAM" 
borders$country_border_code[borders$country_border_code == "JM"] <- "JAM" 

borders$country_code[borders$country_code == "JO"] <- "JOR" 
borders$country_border_code[borders$country_border_code == "JO"] <- "JOR" 

borders$country_code[borders$country_code == "JP"] <- "JPN" 
borders$country_border_code[borders$country_border_code == "JP"] <- "JPN" 

borders$country_code[borders$country_code == "KE"] <- "KEN" 
borders$country_border_code[borders$country_border_code == "KE"] <- "KEN" 

borders$country_code[borders$country_code == "KG"] <- "KYR" 
borders$country_border_code[borders$country_border_code == "KG"] <- "KYR" 

borders$country_code[borders$country_code == "KH"] <- "CAM" 
borders$country_border_code[borders$country_border_code == "KH"] <- "CAM" 

borders$country_code[borders$country_code == "KI"] <- "KIR" 
borders$country_border_code[borders$country_border_code == "KI"] <- "KIR" 

borders$country_code[borders$country_code == "KM"] <- "COM" 
borders$country_border_code[borders$country_border_code == "KM"] <- "COM" 

borders$country_code[borders$country_code == "KN"] <- "STK" 
borders$country_border_code[borders$country_border_code == "KN"] <- "STK" 

borders$country_code[borders$country_code == "KP"] <- "PRK" 
borders$country_border_code[borders$country_border_code == "KP"] <- "PRK" 

borders$country_code[borders$country_code == "KR"] <- "ROK" 
borders$country_border_code[borders$country_border_code == "KR"] <- "ROK" 

borders$country_code[borders$country_code == "KW"] <- "KUW" 
borders$country_border_code[borders$country_border_code == "KW"] <- "KUW" 

borders$country_code[borders$country_code == "KY"] <- "CAY" 
borders$country_border_code[borders$country_border_code == "KY"] <- "CAY" 

borders$country_code[borders$country_code == "KZ"] <- "KZK" 
borders$country_border_code[borders$country_border_code == "KZ"] <- "KZK" 

borders$country_code[borders$country_code == "LA"] <- "LAO" 
borders$country_border_code[borders$country_border_code == "LA"] <- "LAO" 

borders$country_code[borders$country_code == "LB"] <- "LEB" 
borders$country_border_code[borders$country_border_code == "LB"] <- "LEB" 

borders$country_code[borders$country_code == "LC"] <- "STL" 
borders$country_border_code[borders$country_border_code == "LC"] <- "STL" 

borders$country_code[borders$country_code == "LI"] <- "LIE" 
borders$country_border_code[borders$country_border_code == "LI"] <- "LIE" 

#####

borders$country_code[borders$country_code == "LK"] <- "SRI" 
borders$country_border_code[borders$country_border_code == "LK"] <- "SRI" 

borders$country_code[borders$country_code == "LR"] <- "LBR" 
borders$country_border_code[borders$country_border_code == "LR"] <- "LBR" 

borders$country_code[borders$country_code == "LS"] <- "LES" 
borders$country_border_code[borders$country_border_code == "LS"] <- "LES" 

borders$country_code[borders$country_code == "LT"] <- "LIT" 
borders$country_border_code[borders$country_border_code == "LT"] <- "LIT" 

borders$country_code[borders$country_code == "LU"] <- "LUX" 
borders$country_border_code[borders$country_border_code == "LU"] <- "LUX" 

borders$country_code[borders$country_code == "LV"] <- "LAT" 
borders$country_border_code[borders$country_border_code == "LV"] <- "LAT" 

borders$country_code[borders$country_code == "LY"] <- "LIB" 
borders$country_border_code[borders$country_border_code == "LY"] <- "LIB" 

borders$country_code[borders$country_code == "MA"] <- "MOR" 
borders$country_border_code[borders$country_border_code == "MA"] <- "MOR" 

borders$country_code[borders$country_code == "MC"] <- "MCO" 
borders$country_border_code[borders$country_border_code == "MC"] <- "MCO" 

borders$country_code[borders$country_code == "MD"] <- "MLD" 
borders$country_border_code[borders$country_border_code == "MD"] <- "MLD" 

borders$country_code[borders$country_code == "ME"] <- "MOT" 
borders$country_border_code[borders$country_border_code == "ME"] <- "MOT" 

borders$country_code[borders$country_code == "MG"] <- "MAG" 
borders$country_border_code[borders$country_border_code == "MG"] <- "MAG" 

borders$country_code[borders$country_code == "MK"] <- "MAC" 
borders$country_border_code[borders$country_border_code == "MK"] <- "MAC" 

borders$country_code[borders$country_code == "ML"] <- "MLI" 
borders$country_border_code[borders$country_border_code == "ML"] <- "MLI" 

borders$country_code[borders$country_code == "MN"] <- "MON" 
borders$country_border_code[borders$country_border_code == "MN"] <- "MON" 

borders$country_code[borders$country_code == "MP"] <- "NMI" 
borders$country_border_code[borders$country_border_code == "MP"] <- "NMI" 

borders$country_code[borders$country_code == "MQ"] <- "MAR" 
borders$country_border_code[borders$country_border_code == "MQ"] <- "MAR" 

borders$country_code[borders$country_code == "MR"] <- "MAA" 
borders$country_border_code[borders$country_border_code == "MR"] <- "MAA" 

borders$country_code[borders$country_code == "MS"] <- "MST" 
borders$country_border_code[borders$country_border_code == "MS"] <- "MST" 

#####

borders$country_code[borders$country_code == "MT"] <- "MLT" 
borders$country_border_code[borders$country_border_code == "MT"] <- "MLT" 

borders$country_code[borders$country_code == "MU"] <- "MAS" 
borders$country_border_code[borders$country_border_code == "MU"] <- "MAS" 

borders$country_code[borders$country_code == "MV"] <- "MAD" 
borders$country_border_code[borders$country_border_code == "MV"] <- "MAD" 

borders$country_code[borders$country_code == "MW"] <- "MAW" 
borders$country_border_code[borders$country_border_code == "MW"] <- "MAW" 

borders$country_code[borders$country_code == "MX"] <- "MEX" 
borders$country_border_code[borders$country_border_code == "MX"] <- "MEX" 

borders$country_code[borders$country_code == "MY"] <- "MAL" 
borders$country_border_code[borders$country_border_code == "MY"] <- "MAL" 

borders$country_code[borders$country_code == "MZ"] <- "MZM" 
borders$country_border_code[borders$country_border_code == "MZ"] <- "MZM" 

borders$country_code <- with( borders, ifelse( borders$country_name == "Namibia", "NAM", borders$country_code ) ) 
borders$country_border_code <- with( borders, ifelse( borders$country_border_name == "Namibia", "NAM", borders$country_border_code ) ) 

# This case was more problematic because the acronym was NA and it would not change easily

borders$country_code[borders$country_code == "NC"] <- "NWC" 
borders$country_border_code[borders$country_border_code == "NC"] <- "NWC" 

borders$country_code[borders$country_code == "NE"] <- "NIR" 
borders$country_border_code[borders$country_border_code == "NE"] <- "NIR" 

borders$country_code[borders$country_code == "NF"] <- "NFI" 
borders$country_border_code[borders$country_border_code == "NF"] <- "NFI" 

borders$country_code[borders$country_code == "NG"] <- "NIG" 
borders$country_border_code[borders$country_border_code == "NG"] <- "NIG" 

borders$country_code[borders$country_code == "NI"] <- "NIC" 
borders$country_border_code[borders$country_border_code == "NI"] <- "NIC" 

borders$country_code[borders$country_code == "NL"] <- "NTH" 
borders$country_border_code[borders$country_border_code == "NL"] <- "NTH" 

borders$country_code[borders$country_code == "NO"] <- "NOR" 
borders$country_border_code[borders$country_border_code == "NO"] <- "NOR" 

borders$country_code[borders$country_code == "NP"] <- "NEP" 
borders$country_border_code[borders$country_border_code == "NP"] <- "NEP" 

borders$country_code[borders$country_code == "NR"] <- "NAU" 
borders$country_border_code[borders$country_border_code == "NR"] <- "NAU" 

borders$country_code[borders$country_code == "NU"] <- "NIU" 
borders$country_border_code[borders$country_border_code == "NU"] <- "NIU" 

borders$country_code[borders$country_code == "NZ"] <- "NEW" 
borders$country_border_code[borders$country_border_code == "NZ"] <- "NEW" 

borders$country_code[borders$country_code == "OM"] <- "OMA" 
borders$country_border_code[borders$country_border_code == "OM"] <- "OMA" 

borders$country_code[borders$country_code == "PA"] <- "PAN" 
borders$country_border_code[borders$country_border_code == "PA"] <- "PAN" 

borders$country_code[borders$country_code == "PE"] <- "PER" 
borders$country_border_code[borders$country_border_code == "PE"] <- "PER" 

borders$country_code[borders$country_code == "PF"] <- "FPO" 
borders$country_border_code[borders$country_border_code == "PF"] <- "FPO" 

#####

borders$country_code[borders$country_code == "PG"] <- "PNG" 
borders$country_border_code[borders$country_border_code == "PG"] <- "PNG" 

borders$country_code[borders$country_code == "PH"] <- "PHI" 
borders$country_border_code[borders$country_border_code == "PH"] <- "PHI" 

borders$country_code[borders$country_code == "PK"] <- "PAK" 
borders$country_border_code[borders$country_border_code == "PK"] <- "PAK" 

borders$country_code[borders$country_code == "PL"] <- "POL" 
borders$country_border_code[borders$country_border_code == "PL"] <- "POL" 

borders$country_code[borders$country_code == "PR"] <- "PTR" 
borders$country_border_code[borders$country_border_code == "PR"] <- "PTR" 

borders$country_code[borders$country_code == "PS"] <- "PAL" 
borders$country_border_code[borders$country_border_code == "PS"] <- "PAL" 

borders$country_code[borders$country_code == "PT"] <- "POR" 
borders$country_border_code[borders$country_border_code == "PT"] <- "POR" 

borders$country_code[borders$country_code == "PW"] <- "PAU" 
borders$country_border_code[borders$country_border_code == "PW"] <- "PAU" 

borders$country_code[borders$country_code == "PY"] <- "PAR" 
borders$country_border_code[borders$country_border_code == "PY"] <- "PAR" 

borders$country_code[borders$country_code == "QA"] <- "QAT" 
borders$country_border_code[borders$country_border_code == "QA"] <- "QAT" 

borders$country_code[borders$country_code == "RE"] <- "REU" 
borders$country_border_code[borders$country_border_code == "RE"] <- "REU" 

borders$country_code[borders$country_code == "RO"] <- "RUM" 
borders$country_border_code[borders$country_border_code == "RO"] <- "RUM" 

borders$country_code[borders$country_code == "RS"] <- "SER" 
borders$country_border_code[borders$country_border_code == "RS"] <- "SER" 

borders$country_code[borders$country_code == "RU"] <- "RUS" 
borders$country_border_code[borders$country_border_code == "RU"] <- "RUS" 

borders$country_code[borders$country_code == "RW"] <- "RWA" 
borders$country_border_code[borders$country_border_code == "RW"] <- "RWA" 

borders$country_code[borders$country_code == "SA"] <- "SAU" 
borders$country_border_code[borders$country_border_code == "SA"] <- "SAU" 

borders$country_code[borders$country_code == "SB"] <- "SOL" 
borders$country_border_code[borders$country_border_code == "SB"] <- "SOL" 

borders$country_code[borders$country_code == "SC"] <- "SEY" 
borders$country_border_code[borders$country_border_code == "SC"] <- "SEY" 

borders$country_code[borders$country_code == "SD"] <- "SUD" 
borders$country_border_code[borders$country_border_code == "SD"] <- "SUD" 

borders$country_code[borders$country_code == "SE"] <- "SWD" 
borders$country_border_code[borders$country_border_code == "SE"] <- "SWD" 

borders$country_code[borders$country_code == "SG"] <- "SIN" 
borders$country_border_code[borders$country_border_code == "SG"] <- "SIN" 

borders$country_code[borders$country_code == "SH"] <- "STH" 
borders$country_border_code[borders$country_border_code == "SH"] <- "STH" 

borders$country_code[borders$country_code == "SI"] <- "SLV" 
borders$country_border_code[borders$country_border_code == "SI"] <- "SLV" 

#####

borders$country_code[borders$country_code == "SJ"] <- "SVD" 
borders$country_border_code[borders$country_border_code == "SJ"] <- "SVD" 

borders$country_code[borders$country_code == "SK"] <- "SLO" 
borders$country_border_code[borders$country_border_code == "SK"] <- "SLO" 

borders$country_code[borders$country_code == "SL"] <- "SIE" 
borders$country_border_code[borders$country_border_code == "SL"] <- "SIE" 

borders$country_code[borders$country_code == "SM"] <- "SMO" 
borders$country_border_code[borders$country_border_code == "SM"] <- "SMO" 

borders$country_code[borders$country_code == "SN"] <- "SEN" 
borders$country_border_code[borders$country_border_code == "SN"] <- "SEN" 

borders$country_code[borders$country_code == "SO"] <- "SOM" 
borders$country_border_code[borders$country_border_code == "SO"] <- "SOM" 

borders$country_code[borders$country_code == "SR"] <- "SUR" 
borders$country_border_code[borders$country_border_code == "SR"] <- "SUR" 

borders$country_code[borders$country_code == "SS"] <- "SUD" 
borders$country_border_code[borders$country_border_code == "SS"] <- "SUD" 

borders$country_code[borders$country_code == "ST"] <- "SAO" 
borders$country_border_code[borders$country_border_code == "ST"] <- "SAO" 

borders$country_code[borders$country_code == "SV"] <- "SAL" 
borders$country_border_code[borders$country_border_code == "SV"] <- "SAL" 

borders$country_code[borders$country_code == "SY"] <- "SYR" 
borders$country_border_code[borders$country_border_code == "SY"] <- "SYR" 

borders$country_code[borders$country_code == "TD"] <- "CHA" 
borders$country_border_code[borders$country_border_code == "TD"] <- "CHA" 

borders$country_code[borders$country_code == "TG"] <- "TOG" 
borders$country_border_code[borders$country_border_code == "TG"] <- "TOG" 

borders$country_code[borders$country_code == "TH"] <- "THI" 
borders$country_border_code[borders$country_border_code == "TH"] <- "THI" 

borders$country_code[borders$country_code == "TJ"] <- "TAJ" 
borders$country_border_code[borders$country_border_code == "TJ"] <- "TAJ" 

borders$country_code[borders$country_code == "TK"] <- "TKL" 
borders$country_border_code[borders$country_border_code == "TK"] <- "TKL" 

borders$country_code[borders$country_code == "TM"] <- "TKM" 
borders$country_border_code[borders$country_border_code == "TM"] <- "TKM" 

borders$country_code[borders$country_code == "TN"] <- "TUN" 
borders$country_border_code[borders$country_border_code == "TN"] <- "TUN" 

borders$country_code[borders$country_code == "TO"] <- "TON" 
borders$country_border_code[borders$country_border_code == "TO"] <- "TON" 

borders$country_code[borders$country_code == "TR"] <- "TUR" 
borders$country_border_code[borders$country_border_code == "TR"] <- "TUR" 

borders$country_code[borders$country_code == "TT"] <- "TRI" 
borders$country_border_code[borders$country_border_code == "TT"] <- "TRI" 

borders$country_code[borders$country_code == "TV"] <- "TUV" 
borders$country_border_code[borders$country_border_code == "TV"] <- "TUV" 

borders$country_code[borders$country_code == "TW"] <- "TAW" 
borders$country_border_code[borders$country_border_code == "TW"] <- "TAW" 

borders$country_code[borders$country_code == "TZ"] <- "TAZ" 
borders$country_border_code[borders$country_border_code == "TZ"] <- "TAZ" 

borders$country_code[borders$country_code == "UA"] <- "UKR" 
borders$country_border_code[borders$country_border_code == "UA"] <- "UKR" 

borders$country_code[borders$country_code == "UG"] <- "UGA" 
borders$country_border_code[borders$country_border_code == "UG"] <- "UGA" 

borders$country_code[borders$country_code == "US"] <- "USA" 
borders$country_border_code[borders$country_border_code == "US"] <- "USA" 

borders$country_code[borders$country_code == "UY"] <- "URU" 
borders$country_border_code[borders$country_border_code == "UY"] <- "URU" 

borders$country_code[borders$country_code == "UZ"] <- "UZB" 
borders$country_border_code[borders$country_border_code == "UZ"] <- "UZB" 

borders$country_code[borders$country_code == "VA"] <- "VAT" 
borders$country_border_code[borders$country_border_code == "VA"] <- "VAT" 

borders$country_code[borders$country_code == "VC"] <- "STV" 
borders$country_border_code[borders$country_border_code == "VC"] <- "STV" 

borders$country_code[borders$country_code == "VE"] <- "VEN" 
borders$country_border_code[borders$country_border_code == "VE"] <- "VEN" 

#borders$country_code[borders$country_code == "VI"] <- "VIR" 
#borders$country_border_code[borders$country_border_code == "VI"] <- "VIR" 
# Dropped both Virgin Islands in borders data because only one appears in dyadic data and there 
# is no way of knowing which is which

borders$country_code[borders$country_code == "VN"] <- "DRV" 
borders$country_border_code[borders$country_border_code == "VN"] <- "DRV" 

borders$country_code[borders$country_code == "VU"] <- "VAN" 
borders$country_border_code[borders$country_border_code == "VU"] <- "VAN" 

borders$country_code[borders$country_code == "WS"] <- "WSM" 
borders$country_border_code[borders$country_border_code == "WS"] <- "WSM" 

borders$country_code[borders$country_code == "YE"] <- "YEM" 
borders$country_border_code[borders$country_border_code == "YE"] <- "YEM" 

borders$country_code[borders$country_code == "ZA"] <- "SAF" 
borders$country_border_code[borders$country_border_code == "ZA"] <- "SAF" 

borders$country_code[borders$country_code == "ZM"] <- "ZAM" 
borders$country_border_code[borders$country_border_code == "ZM"] <- "ZAM" 

borders$country_code[borders$country_code == "ZW"] <- "ZIM" 
borders$country_border_code[borders$country_border_code == "ZW"] <- "ZIM" 

borders$country_border_code[borders$country_border_code == ""] <- ".na" 

# Create vector with unique IDs of both reference countries and border countries
countries <- sort(unique(c(borders$country_code, borders$country_border_code)))
length(countries)

# Create matrix to populate with border info
borders.mat <- matrix(0, nrow =length(countries) , ncol = length(countries)) 
colnames(borders.mat) <- as.character(countries)
rownames(borders.mat) <- as.character(countries)
# A glimpse of what this looks like
borders.mat[1:4, 1:4]


# use a loop to fill the matrix: 1 whenever there is a border , 0 otherwise. (ADJACENCY MATRIX)
i <- 1
for (i in 1:nrow(borders)) {
  row.index <- which(countries == borders[i, 1])
  col.index <- which(countries == borders[i, 3])
  borders.mat[row.index, col.index] <- 1
}
borders.mat[4, ] # Does it work for Afghanistan? Yep!

save(borders.mat,file = "borders.mat.rda")


## Civil wars

'd2 <- read.csv("kingdata.csv")
d9 <- read.csv("final1990.csv")
d95 <- read.csv("final1995.csv")

civil <- subset(d2,d2$EventForm == "<CLAS>" & d2$SrcName == d2$TgtName)
save(civil,file = "civil20s.rda") #These are later loaded into ERGM_asylum script. Keep
#it that way because it might mess up the if-then I put in the for loop.

civil <- subset(d9,d9$EventForm == "<CLAS>" & as.character(d9$SrcName) == as.character(d9$TgtName))
save(civil,file = "civil90s.rda")

civil <- subset(d95,d95$EventForm == "<CLAS>" & d95$SrcName == d95$TgtName)
save(civil,file = "civil95s.rda")'

# These .rda files ar created by the code commented out above. We load the .rda files directly for conveninence
# (both for us and the grader) as the .csv files are extremely large. However, if the grader wants to check that 
# bit of cleaning code as well, the .csv files are also part of the submitted .zip file.
load(file = "civil90s.rda")
civil90 <- subset(civil, civil$SrcSector == "<INSU>")
load(file = "civil95s.rda") 
civil95 <- subset(civil, civil$SrcSector == "<INSU>")
load(file = "civil20s.rda") 
civil20 <- subset(civil, civil$SrcSector == "<INSU>")

## Econ aid 


'As above, this cleaning code is commented out to calling the very large .csv 
files. Load the .rda file below.

d2000 <- read.csv("kingdata.csv")
d1995 <- read.csv("final1995.csv")
d1990 <- read.csv("final1990.csv")

d20 <- subset(d2000, d2000$EventForm == "<EEAI>")
d20 <- subset(d20, d20$SrcName != d20$TgtName)

d95 <- subset(d1995, d1995$EventForm == "<EEAI>")
d95 <- subset(d95, d95$SrcName != d95$TgtName)

d90 <- subset(d1990, d1990$EventForm == "<EEAI>")
d90 <- subset(d90, as.character(d90$SrcName) != as.character(d90$TgtName))

d20 -> ea20s
d95 -> ea95s
d90 -> ea90s

save(ea20s,ea95s,ea90s,file = "econaids.rda")'

load(file = "econaids.rda")


## GDP per capita

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

attGDPpc <- attGDPpc[,1:17]

save(attGDPpc,file = "GDPpc.mat.rda")


## HDI 

HDI <- read.csv("Human Development Index (HDI)_clean.csv", head = TRUE, sep=";") 

#Use borders data to extract clean list of country id's so that I can appropriately match nodes

load(file = "borders.mat.rda") 
countries <- colnames(borders.mat)
countries <- countries[-1] 
countries <- sort(countries)
# Remove .na entry, which in borders.mat implies country has no adjacencies/borders

#We can create a new data frame with the row names of the adjacency matrix,
#and match the variables you want as attributes to this new data frame:
attHDI <- data.frame('country' = as.character(countries))
attHDI$HDI1990 <- HDI$X1990[match(attHDI$country, as.character(HDI$country.code))]
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


## RELIGIONS

'
Data cleaning code below was run separately until line 1009. Then a column with 
country codes was added in excel to the written csv file saved at line 1038. 

This file is then loaded and manipulated further to generate the final dataset.
Please load major_religion_1 directly at line 1042.

x <- read.csv("Religious_Composition_by_Country_2010-2050.csv")
x <- subset(x, x$Year == "2010")

x <- x[, -(1:5)]
x <- x[-(1:7), ]
x <- x[, -2]
x <- mutate_all(x, as.character)
for (i in 1:nrow(x)) {
  if (x[i, 2] == "< 1.0") {
    x[i, 2] <- "0.1"
  }
  if (x[i, 2] == ">99.0") {
    x[i, 2] <- "99"
  }
  if (x[i, 3] == "< 1.0") {
    x[i, 3] <- "0.1"
  }
  if (x[i, 3] == ">99.0") {
    x[i, 3] <- "99"
  }
  if (x[i, 4] == "< 1.0") {
    x[i, 4] <- "0.1"
  }
  if (x[i, 4] == ">99.0") {
    x[i, 4] <- "99"
  }
  if (x[i, 5] == "< 1.0") {
    x[i, 5] <- "0.1"
  }
  if (x[i, 5] == ">99.0") {
    x[i, 5] <- "99"
  }
  if (x[i, 6] == "< 1.0") {
    x[i, 6] <- "0.1"
  }
  if (x[i, 6] == ">99.0") {
    x[i, 6] <- "99"
  }
  if (x[i, 7] == "< 1.0") {
    x[i, 7] <- "0.1"
  }
  if (x[i, 7] == ">99.0") {
    x[i, 7] <- "99"
  }
  if (x[i, 8] == "< 1.0") {
    x[i, 8] <- "0.1"
  }
  if (x[i, 8] == ">99.0") {
    x[i, 8] <- "99"
  }
  if (x[i, 9] == "< 1.0") {
    x[i, 9] <- "0.1"
  }
  if (x[i, 9] == ">99.0") {
    x[i, 9] <- "99"
  }
}
x[, -1] <- mutate_all(x[, -1], as.numeric)
x$major <- colnames(x[, -1])[max.col(x[, -1], ties.method = "first")]

write.csv(x, "major_religion.csv")'

x <- read.csv("major_religion_1.csv")
x$Code <- as.character(x$Code)
x <- subset(x, x$Code != "#N/A")
write.csv(x, "major_religion.csv")


## WARS

p <- read.csv("dyadwars2.csv")

cow_to_king <- levels(p$namea)
cow_to_king2 <- levels(p$nameb)

r <- read.csv("c2.csv")
wars <- read.csv("dyadwars2.csv")
wars <- select(wars,namea,nameb,hihost,year)
wars$namea <- mapvalues(wars$namea,from = levels(wars$namea),to = as.vector(r$Y))
wars$nameb <- mapvalues(wars$nameb,from = levels(wars$nameb),to = as.vector(r$Y))
wars <- subset(wars,wars$namea != "N" & wars$nameb != "N")
wars1990 <- subset(wars,wars$year == 1990)
wars1991 <- subset(wars,wars$year == 1991)
wars1992 <- subset(wars,wars$year == 1992)
wars1993 <- subset(wars,wars$year == 1993)
wars1994 <- subset(wars,wars$year == 1994)
wars1995 <- subset(wars,wars$year == 1995)
wars1996 <- subset(wars,wars$year == 1996)
wars1997 <- subset(wars,wars$year == 1997)
wars1998 <- subset(wars,wars$year == 1998)
wars1999 <- subset(wars,wars$year == 1999)
wars2000 <- subset(wars,wars$year == 2000)
wars2001 <- subset(wars,wars$year == 2001)
wars2002 <- subset(wars,wars$year == 2002)
wars2003 <- subset(wars,wars$year == 2003)
wars2004 <- subset(wars,wars$year == 2004)
wars2005 <- subset(wars,wars$year == 2005)
wars_by_year <- list(wars1990,wars1991,wars1992,wars1993,wars1994,wars1995,wars1996,wars1997,wars1998,wars1999,
                     wars2000,wars2001,wars2002,wars2003,wars2004,wars2005)

save(wars_by_year,file = "wars_by_year.rda")


## ALLIANCE

ally2 <- read.csv("a2.csv")
ally2 <- select(ally2,namea,nameb,year)
ally2 <- subset(ally2,ally2$namea != "N" & ally2$nameb != "N")
ally1990 <- subset(ally2,ally2$year == 1990)
ally1991 <- subset(ally2,ally2$year == 1991)
ally1992 <- subset(ally2,ally2$year == 1992)
ally1993 <- subset(ally2,ally2$year == 1993)
ally1994 <- subset(ally2,ally2$year == 1994)
ally1995 <- subset(ally2,ally2$year == 1995)
ally1996 <- subset(ally2,ally2$year == 1996)
ally1997 <- subset(ally2,ally2$year == 1997)
ally1998 <- subset(ally2,ally2$year == 1998)
ally1999 <- subset(ally2,ally2$year == 1999)
ally2000 <- subset(ally2,ally2$year == 2000)
ally2001 <- subset(ally2,ally2$year == 2001)
ally2002 <- subset(ally2,ally2$year == 2002)
ally2003 <- subset(ally2,ally2$year == 2003)
ally2004 <- subset(ally2,ally2$year == 2004)
ally2005 <- subset(ally2,ally2$year == 2005)

ally_by_year <- list(ally1990,ally1991,ally1992,ally1993,ally1994,ally1995,ally1996,ally1997,ally1998,ally1999,
                     ally2000,ally2001,ally2002,ally2003,ally2004,ally2005)

save(ally_by_year,file = "ally_by_year.rda")



##########################################################################################################
# TOPIC 1: Peer influence in international relations
##########################################################################################################

#Weibull_pdf formula
weibull_pdf <- function(t,eta,beta){
  beta*(t)^(beta-1)*exp(-1*(t/eta)^beta)/(eta^beta)
}

#Just integrating and vectorizing. Actual formula in Latex will be....1-e^-(t/eta)^beta.
weibull_cdf <- Vectorize(function(p,eta,beta){
  integrate(weibull_pdf,lower=10^(-100),upper=p,eta=eta,beta=beta)$value
})

#Hazard rate will be pdf/survival... (Algebra done in Latex. Differentiation shows that how time affects
#the hazard rate will depend on beta.)
hazardrate <- Vectorize(function(x,eta,beta){
  weibull_pdf(x,eta,beta)/(1-weibull_cdf(x,eta,beta))
})
#Let's verify....
curve(hazardrate(x,1,0.9)) # Weibull with beta < 1 has decreasing hazard rate.
curve(hazardrate(x,1,1)) # Weibull with beta = 1 has constant hazard rate.
curve(hazardrate(x,1,1.5)) # Weibull with beta > 1 has increasing hazard rate.

'A Weibull r.v. with β<1 can imply a “positive” peer influence. The event encourages more events. 
When β>1, we see “negative” peer influence. The event discourages subsequent events which become 
more probable as time passes since the last event.'


## A. PEER INFLUENCE IN ARMED ASSISSTANCE REQUESTS.
# DATASET USED : 10 mm dyadic events (1990 - 1995), Gary King.

load("mem90.rda") #Load the raw data.
eventtype <- "<ASKI>" #Choose the event <ASKI> (Ask for armed assistance).
timeframe <- "weeks"

data <- subset(data90,data90$EventForm == eventtype) #Subset for only ASKI events.
data$SrcName <- as.character(data$SrcName) 
data$TgtName <- as.character(data$TgtName)
data <- subset(data, data$SrcName != data$TgtName) #Only inter-country events picked.
data <- dplyr::select(data,"EventDate","SrcName","TgtName")
data$EventDate <- mdy(data$EventDate) #Date format standardized.
data <- data[order(data$EventDate),] #Arrange chronologically.

attach(data)
vec90 <- c()

#This for loop populates the vec90 vector with date differences of successive events.
#It is computed such that none of the actors(src or tgt) are repeated in successive events.
#This is done to avoid mass-action events like a country asking for armed assisstance from
#multiple countries at once or multiple countries asking a single country for armed
#assisstance at once. Such events would obviously be peer influenced. By doing this,
#we remove such trivial examples of peer-influence.
for(i in 1:(nrow(data)-1)){
  if(SrcName[i] != SrcName[i+1] & SrcName[i] != TgtName[i+1]
     & TgtName[i] != TgtName[i+1] & TgtName[i] != SrcName[i+1]){
    j <- difftime(strptime(data$EventDate[i+1], format = "%Y-%m-%d"),
                  strptime(data$EventDate[i], format = "%Y-%m-%d"),units=timeframe)
    vec90 <- c(vec90,j)
  }
}
detach(data)

vec90 <- replace(vec90,which(vec90==0),0.00000001); vec90 #Remove 0 entries as support for weibull is x>0.

#We use Likelihood Ratio test for g.o.f of exponential and weibull distributions.
l1 <- LK.test(vec90,"LR",nsim = 200)  #Check for g.o.f of exponential distbn.
w1 <- WLK.test(vec90,type="EW",procedure="LR") #Check for g.o.f of weibull distbn.
l1;w1
paste("pval of Expo distbn =",l1$p.value,sep = " ")
paste("pval of Weibull distbn =",w1$p.value,sep = " ")
paste("shape parameter of fitted Weibull =",round(w1$estimate[2],digits = 3),sep = " ")


#Graph it.
df <- as.data.frame(vec90)
df$index <- c(1:length(vec90))

x <- seq(0, 30, length.out=100)
df2 <- with(df, data.frame(x = x, y = dexp(x,l1$estimate)))
df3 <- with(df, data.frame(x = x, y = weibull_pdf(x,w1$estimate[1],w1$estimate[2])))

ASKIplot <- ggplot(df,aes(x=vec90)) + 
  geom_histogram(aes(y=..density..),binwidth = 0.8,color = "black",fill = alpha("black",0.1)) +
  geom_line(data = df2, aes(x = x, y = y,color = "Exponential"), size = 1) +
  geom_line(data = df3, aes(x = x, y = y,color = "Weibull"), size = 1) +
  geom_density(aes(y=..density..),fill ="#FF6666",alpha = 0.4) +
  scale_x_continuous(name = "Weeks since last event") +
  scale_y_continuous(name = "Density") +
  scale_color_manual(name = "Distributions", 
                     values = c("Exponential" = "#244747", "Weibull" = "#e3120b")) +
  ggtitle("Time intervals between Armed Assistance requests") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold.italic"))
ASKIplot

#The weibull fits excellently (pval = 1) and estimates a shape parameter != 1.
#This gives sufficient evidence that armed assisstance requests are NOT poisson distributed.
#The weibull shape parameter is 0.769. 

#This means the distribution of time between events
#exhibits the property of "infant mortality". Probability of event happening immediately or
#soon after a preceding event is high, which falls as time passes since the last event.

#USING INTEGRATION [EXTRA POINT]:
#Let's calculate the Expected number of days between two subsequent armed assist requests.
integrand1 <- function(t,eta,beta){
  t*(beta*(t)^(beta-1)*exp(-1*(t/eta)^beta)/(eta^beta))
}

exp_days <- function(eta,beta)integrate(integrand1,lower = 10^(-100),upper = Inf,eta=eta,beta=beta)$value

exp_days(w1$estimate[1],w1$estimate[2]) #6 weeks.

## B. NO PEER INFLUENCE IN GIVING ULTIMATUMS.
# DATASET USED : 10 mm dyadic events (1990 - 1995), Gary King.

eventtype <- "<ULTI>" #Choose the event <ULTI> (Give Ultimatum).
timeframe <- "weeks"

data <- subset(data90,data90$EventForm == eventtype) #Subset for only ULTI events.
data$SrcName <- as.character(data$SrcName) 
data$TgtName <- as.character(data$TgtName)
data <- subset(data, data$SrcName != data$TgtName) #Only inter-country events picked.
data <- dplyr::select(data,"EventDate","SrcName","TgtName")
data$EventDate <- mdy(data$EventDate) #Date format standardized.
data <- data[order(data$EventDate),] #Arrange chronologically.

attach(data)
vec90 <- c()

#This for loop populates the vec90 vector with date differences of successive events.
#It is computed such that none of the actors(src or tgt) are repeated in successive events.
#This is done to avoid mass-action events like a country giving ultimatums to
#multiple countries at once or multiple countries giving ultimatums to a single country 
#at once. Such events would obviously be peer influenced (as in cases of war). 
#By doing this, we remove such trivial examples of peer-influence.
for(i in 1:(nrow(data)-1)){
  if(SrcName[i] != SrcName[i+1] & SrcName[i] != TgtName[i+1]
     & TgtName[i] != TgtName[i+1] & TgtName[i] != SrcName[i+1]){
    j <- difftime(strptime(data$EventDate[i+1], format = "%Y-%m-%d"),
                  strptime(data$EventDate[i], format = "%Y-%m-%d"),units=timeframe)
    vec90 <- c(vec90,j)
  }
}
detach(data)

vec90 <- replace(vec90,which(vec90==0),0.00000001); vec90 #Remove 0 entries as support for weibull is x>0.

#We use Likelihood Ratio test for g.o.f of exponential and weibull distributions.
l1 <- LK.test(vec90,"LR",nsim = 200)  #Check for g.o.f of exponential distbn.
w1 <- WLK.test(vec90,type="EW",procedure="LR") #Check for g.o.f of weibull distbn.
l1;w1

paste("pval of Expo distbn =",l1$p.value,sep = " ")
paste("pval of Weibull distbn =",w1$p.value,sep = " ")
paste("shape parameter of fitted Weibull =",round(w1$estimate[2],digits = 3),sep = " ")

#Graph it.
df <- as.data.frame(vec90)
df$index <- c(1:length(vec90))

x <- seq(0, 30, length.out=100)
df2 <- with(df, data.frame(x = x, y = dexp(x,l1$estimate)))
df3 <- with(df, data.frame(x = x, y = weibull_pdf(x,w1$estimate[1],w1$estimate[2])))


ULTIplot <- ggplot(df,aes(x=vec90)) + 
  geom_histogram(aes(y=..density..),binwidth = 0.8,color = "black",fill = alpha("black",0.1)) +
  geom_line(data = df2, aes(x = x, y = y,color = "Exponential"), size = 1) +
  geom_line(data = df3, aes(x = x, y = y,color = "Weibull"), size = 1) +
  geom_density(aes(y=..density..),fill ="#FF6666",alpha = 0.4) +
  scale_x_continuous(name = "Weeks since last event") +
  scale_y_continuous(name = "Density") +
  scale_color_manual(name = "Distributions", 
                     values = c("Exponential" = "#244747", "Weibull" = "#e3120b")) +
  ggtitle("Time intervals between Ultimatums") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold.italic"))
ULTIplot

#The weibull fits excellently (pval > 0.95) and estimates a shape parameter ~ 1.
#This gives strong evidence that giving ultimatums might be poisson distributed.
#The weibull shape parameter is 1.02. This means we are close to seeing the property of memorylessness.



##########################################################################################################
# INSTALLATION OF BTERGM

'Warning: If the installation code below does not work for some reason, please load files asylmod_summary.rda
and threatmod_summary.rda to see the results of both network analyses!'
  
#load(file = "asylmod_summary.rda")
#load(file = "threatmod_summary.rda") 
# These load commands will be found at the end of both network analyses to display the results.
##########################################################################################################


url <- "https://cran.r-project.org/src/contrib/Archive/btergm/btergm_1.9.4.tar.gz"
pkgFile <- "btergm_1.9.4.tar.gz"
download.file(url = url, destfile = pkgFile)
install.packages(pkgs=pkgFile, type="source", repos=NULL)


##########################################################################################################
# TOPIC 2: Does providing Aid buy Softpower? 
##########################################################################################################

# DATASET USED : 10 mm dyadic events (1990 - 2005), Gary King.

# Preparation

load("mem20.rda") #Load data from 2000-2005
load("mem95.rda") #Load data from 1995-2000
load("mem90.rda") #Load data from 1990-1995

totalvector_generator <- function(eventtype, timeframe){
  #2000-2005
  data <- subset(data20, data20$EventForm == eventtype)
  data <- subset(data, data$SrcName != data$TgtName)
  data <- subset(data, data$SrcLevel == "<CTRY>" & data$TgtLevel == "<CTRY>")
  data <- dplyr::select(data, "EventDate", "SrcName", "TgtName")
  data$EventDate <- mdy_hms(data$EventDate)
  data <- data[order(data$EventDate),]
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  
  attach(data)
  
  vec1 <- c()
  for (i in 1:(nrow(data) - 1)) {
    if (SrcName[i] != SrcName[i + 1] & SrcName[i] != TgtName[i + 1]
        &
        TgtName[i] != TgtName[i + 1] & TgtName[i] != SrcName[i + 1]) {
      j <- difftime(
        strptime(data$EventDate[i + 1], format = "%Y-%m-%d"),
        strptime(data$EventDate[i], format = "%Y-%m-%d"),
        units = timeframe
      )
      vec1 <- c(vec1, j)
    }
  }
  detach(data)
  
  vec1 <- replace(vec1, which(vec1 == 0), 0.00000001)
  
  #1995-2000
  data <- subset(data95, data95$EventForm == eventtype)
  data <- subset(data, data$SrcName != data$TgtName)
  data <-
    subset(data, data$SrcLevel == "<CTRY>" &
             data$TgtLevel == "<CTRY>")
  
  data <- dplyr::select(data, "EventDate", "SrcName", "TgtName")
  data$EventDate <- ymd(data$EventDate)
  data <- data[order(data$EventDate),]
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  
  attach(data)
  
  vec95 <- c()
  for (i in 1:(nrow(data) - 1)) {
    if (SrcName[i] != SrcName[i + 1] & SrcName[i] != TgtName[i + 1]
        &
        TgtName[i] != TgtName[i + 1] & TgtName[i] != SrcName[i + 1]) {
      j <- difftime(
        strptime(data$EventDate[i + 1], format = "%Y-%m-%d"),
        strptime(data$EventDate[i], format = "%Y-%m-%d"),
        units = timeframe
      )
      vec95 <- c(vec95, j)
    }
  }
  detach(data)
  
  vec95 <- vec95[!is.na(vec95)]
  vec95 <- replace(vec95, which(vec95 == 0), 0.00000001)
  
  #1990-1995
  data <- subset(data90, data90$EventForm == eventtype)
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  data <- subset(data, data$SrcName != data$TgtName)
  data <-
    subset(data, data$SrcLevel == "<CTRY>" &
             data$TgtLevel == "<CTRY>")
  
  data <- dplyr::select(data, "EventDate", "SrcName", "TgtName")
  data$EventDate <- mdy(data$EventDate)
  data <- data[order(data$EventDate),]
  
  attach(data)
  
  vec90 <- c()
  for (i in 1:(nrow(data) - 1)) {
    if (SrcName[i] != SrcName[i + 1] & SrcName[i] != TgtName[i + 1]
        &
        TgtName[i] != TgtName[i + 1] & TgtName[i] != SrcName[i + 1]) {
      j <- difftime(
        strptime(data$EventDate[i + 1], format = "%Y-%m-%d"),
        strptime(data$EventDate[i], format = "%Y-%m-%d"),
        units = timeframe
      )
      vec90 <- c(vec90, j)
    }
  }
  detach(data)
  
  vec90 <- replace(vec90, which(vec90 == 0), 0.00000001)
  totalvec <<- c(vec1, vec95, vec90)
}
#save(totalvector_generator, file = "totalvector_generator.rda")
#### Let's start with economic aid.
#Total for Aid
totalvector_generator("<EEAI>","weeks")
aid_totalvec <- totalvec
save(aid_totalvec, file = "EEAI_vec_pre.rda")

###############################################################

#Total for Threats.
totalvector_generator("<THRT>","weeks")
threat_totalvec <- totalvec
save(threat_totalvec, file = "THRT_vec_pre.rda")
#Analysis.....
l1 <- LK.test(aid_totalvec, "LR", nsim = 200)
l2 <- LK.test(threat_totalvec, "LR", nsim = 200)

#Graphs....
df <- as.data.frame(aid_totalvec)
df$index <- c(1:length(aid_totalvec))

x <- seq(0, 30, length.out=100)
df2 <- with(df, data.frame(x = x, y = dexp(x,l1$estimate)))

Aidplot <- ggplot(df,aes(x=aid_totalvec)) + 
  geom_histogram(aes(y=..density..),binwidth = 0.8,color = "black",fill = alpha("black",0.1)) +
  geom_line(data = df2, aes(x = x, y = y,color = "Exponential"), size = 1) +
  geom_density(aes(y=..density..),fill ="#FF6666",alpha = 0.4) +
  ylim(c(0,0.5)) +
  xlim(c(0,10)) +
  scale_x_continuous(name = "Weeks since last event") +
  scale_y_continuous(name = "Density") +
  scale_color_manual(name = "Distributions", 
                     values = c("Exponential" = "#244747")) +
  ggtitle("Economic Aid") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold.italic"))


df <- as.data.frame(threat_totalvec)
df$index <- c(1:length(threat_totalvec))

x <- seq(0, 30, length.out=100)
df2 <- with(df, data.frame(x = x, y = dexp(x,l1$estimate)))

Threatplot <- ggplot(df,aes(x=threat_totalvec)) + 
  geom_histogram(aes(y=..density..),binwidth = 0.8,color = "black",fill = alpha("black",0.1)) +
  geom_line(data = df2, aes(x = x, y = y,color = "Exponential"), size = 1) +
  geom_density(aes(y=..density..),fill ="#FF6666",alpha = 0.4) +
  ylim(c(0,0.5)) +
  xlim(c(0,70)) +
  scale_x_continuous(name = "Weeks since last event") +
  scale_y_continuous(name = "Density") +
  scale_color_manual(name = "Distributions", 
                     values = c("Exponential" = "#244747")) +
  ggtitle("Threat") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold.italic"))


grid.arrange(Aidplot,Threatplot, ncol = 2)

############################### Let's graph the aid&threat network for 1994 as an
############################### illustration.

load(file = "data90.rda")
data90 <- subset(data90,data90$EventForm == "<EEAI>" | data90$EventForm == "<THRT>")
data90$EventDate <- mdy(data90$EventDate)
data90 <- subset(data90, data90$EventDate < "1994-01-01" & data90$EventDate >= "1993-01-01")
save(data90, file = "atgraph_data90.rda")
############### GRAPH WORK STARTS.

nodes90 <- as_tibble(union(levels(droplevels(data90$SrcName)),levels(droplevels(data90$TgtName))))
nodes <- unique(nodes90)

colnames(nodes) <- "label"
nodes <- nodes %>% rowid_to_column("id")

data1 <- select(data90,"SrcName","TgtName","EventForm")

attach(data1)

per_route <- data1 %>%  
  group_by(SrcName, TgtName, EventForm) %>%
  dplyr::summarise(weight = n()) %>% 
  ungroup()
per_route

detach(data1)

edges <- per_route %>% 
  left_join(nodes, by = c("SrcName" = "label")) %>% 
  dplyr::rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("TgtName" = "label")) %>% 
  dplyr::rename(to = id)
edges <- select(edges, from, to, weight, EventForm)
edges$EventForm <- droplevels(edges$EventForm)

levels(edges$EventForm) <- c(1,2)
edges$EventForm <- as.numeric(edges$EventForm)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
set_edge_attr(routes_igraph,"EventForm",value = edges$EventForm)
deg <- igraph::degree(routes_igraph, mode="all")

V(routes_igraph)$color <- "tomato"
E(routes_igraph)$arrow.size <- .4
colorss <- c("green","red")
E(routes_igraph)$color <- colorss[E(routes_igraph)$EventForm]
save(routes_igraph,file="at_graph.rda")
plot(routes_igraph,vertex.size = 10,vertex.label.cex = 0.7,vertex.label.color = "black",
     main = "Aid and Threat Network for 1993")
legend("topleft",c("Economic Aid","Threat"),fill = colorss)
#Use zoom to see it better.

'pdf("atgraphFINAL2.pdf",10,10) # Saves the plot as a pdf.
plot(routes_igraph,vertex.size = 10,vertex.label.cex = 0.7,vertex.label.color = "black",
     main = "Aid and Threat Network for 1994")
legend("topleft",c("Economic Aid","Threat"),fill = colorss)
dev.off()'

# Analysis

load(file = "borders.mat.rda") 
load(file = "wars_by_year.rda") 
load(file = "ally_by_year.rda")
load(file = "data90.rda")
load(file = "data95.rda") 
load(file = "data20.rda")

for (i in 1:15){
  if(i > 5){
    if(i < 11){
      data <- data95
      aid <- ea95s
    } else { aid <- ea20s
    data <- data20 }
  } else { aid <- ea90s 
  data <- data90 }
  
  data <- subset(data,data$EventForm == "<THRT>")
  data <- dplyr::select(data,"EventDate", "SrcName","TgtName")
  aid <- dplyr::select(aid,"EventDate","TgtName","SrcName")
  
  if(i > 5){
    if(i < 11){
      data$EventDate <- ymd(data$EventDate)
      aid$EventDate <- ymd(aid$EventDate)
    } else {data$EventDate <- mdy_hms(data$EventDate)
    aid$EventDate <- mdy_hms(aid$EventDate)}
  } else {data$EventDate <- mdy(data$EventDate)
  aid$EventDate <- mdy(aid$EventDate)}
  
  data <- data[order(data$EventDate),]
  aid <- aid[order(aid$EventDate),]
  
  j <- i + 1989
  k <- i + 1990
  startdate <- paste(j,"/01/01",sep = "")
  enddate <- paste(k,"/01/01",sep = "")
  data <- subset(data,data$EventDate < enddate & data$EventDate >= startdate) ###################### 1990 - 1995.
  aid  <- subset(aid,  aid$EventDate < enddate & aid$EventDate  >= startdate)
  
  data <- data[,-1]
  aid <- aid[,-1]
  
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  data$counter <- c(rep(1,nrow(data)))
  
  aid$SrcName <- as.character(aid$SrcName)
  aid$TgtName <- as.character(aid$TgtName)
  aid$counter <- c(rep(1,nrow(aid)))
  
  aid <- ddply(aid,.(TgtName,SrcName),nrow)
  
  ally1990 <- ally_by_year[[i]]
  war1990 <- wars_by_year[[i]]
  
  ally1990_1 <- ally1990
  temp <- ally1990_1[,1]
  ally1990_1[,1] <- ally1990_1[,2]
  ally1990_1[,2] <- temp
  ally1990 <- rbind(ally1990, ally1990_1) # Makes alliances edglist "complete"
  
  colnames(borders.mat)[3] <- "UK_"
  rownames(borders.mat)[3] <- "UK_"
  
  borders.mat_without_NA <- as.matrix(borders.mat[-(1:2),-(1:2)])
  
  bord <- EdgelistFromAdjacency(borders.mat_without_NA, nodelist = colnames(borders.mat_without_NA))
  
  landlock <- borders.mat[,1]
  
  m1 <- merge(war1990[,-4],ally1990,by = c("namea","nameb"),all = TRUE)
  colnames(m1)[3] <- "Wars"
  colnames(m1)[4] <- "Alliance"
  m1[,3] <- ifelse(is.na(m1[,3]), 0, m1[,3])
  m1[,4] <- ifelse(is.na(m1[,4]), 0, 1)
  
  colnames(bord)[1] <- "namea"
  colnames(bord)[2] <- "nameb"
  
  m1 <- merge(m1,bord, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[5] <- "border"
  
  colnames(aid)[1] <- "namea"
  colnames(aid)[2] <- "nameb"
  
  m1 <- merge(m1, aid, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[6] <- "Aid"
  
  colnames(data)[1] <- "namea"
  colnames(data)[2] <- "nameb"
  
  m1 <- merge(m1, data, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[ncol(m1)] <- "Threat"
  
  m1[is.na(m1)] = 0
  
  load(file = "GDPpc.mat.rda") 
  attGDPpc$country <- as.character(attGDPpc$country)
  
  attGDPpc$country[attGDPpc$country == "_UK"] <- "UK_"
  node.att.1990 <- attGDPpc[,c(1,i+1)] # Create GDP node att column
  
  colnames(node.att.1990)[2] <- "GDP"
  
  
  node.att.1990 <- node.att.1990[as.character(node.att.1990$GDP)!= "" ,]
  node.att.1990 <- na.omit(node.att.1990)
  
  edge.att.1990 <- filter(m1, is.element(m1$namea, node.att.1990$country) & is.element(m1$nameb, node.att.1990$country)) 
  node.att.1990 <- filter(node.att.1990, is.element(node.att.1990$country, edge.att.1990$namea) & is.element(node.att.1990$country, edge.att.1990$nameb)) 
  
  edge.att.1990 <- edge.att.1990[order(edge.att.1990$namea, edge.att.1990$nameb),]
  threat_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,ncol(m1))])
  
  threatnet <- network(threat_adj_1990$adjacency,directed = TRUE,matrix.type = "adjacency")
  
  network::set.vertex.attribute(threatnet, 'Per Capita Income', as.numeric(node.att.1990$GDP))
  network::set.network.attribute(threatnet,'Alliance', edge.att.1990$Alliance)
  network::set.network.attribute(threatnet,'Border', edge.att.1990$border)
  network::set.network.attribute(threatnet,'Aid', edge.att.1990$Aid)
  
  q <- paste("threatnet",j,"final",".rda", sep = "")
  save(threatnet, file = q)
}


load(file = 'threatnet2000final.rda')
threatnet -> tnet2000
load(file = 'threatnet2001final.rda')
threatnet -> tnet2001
load(file = 'threatnet2002final.rda')
threatnet -> tnet2002
load(file = 'threatnet2003final.rda')
threatnet -> tnet2003
load(file = 'threatnet2004final.rda')
threatnet -> tnet2004
load(file = 'threatnet1990final.rda')
threatnet -> tnet1990
load(file = 'threatnet1991final.rda')
threatnet -> tnet1991
load(file = 'threatnet1992final.rda')
threatnet -> tnet1992
load(file = 'threatnet1993final.rda')
threatnet -> tnet1993
load(file = 'threatnet1994final.rda')
threatnet -> tnet1994
load(file = 'threatnet1995final.rda')
threatnet -> tnet1995
load(file = 'threatnet1996final.rda')
threatnet -> tnet1996
load(file = 'threatnet1997final.rda')
threatnet -> tnet1997
load(file = 'threatnet1998final.rda')
threatnet -> tnet1998
load(file = 'threatnet1999final.rda')
threatnet -> tnet1999
netlist <- list(tnet1990,tnet1991,tnet1992,tnet1993,tnet1994
                ,tnet1995,tnet1996,tnet1997,tnet1998,tnet1999,
                tnet2000,tnet2001,tnet2002,tnet2003,tnet2004)

#### CAUTION! THE FOLLOWING CODE MIGHT TAKE BETWEEN 5-10 MINUTES TO RUN, IF YOU
#### CAN'T/ DON'T WANT TO RUN THE ACTUAL BTERGM ANALYSIS, JUST LOAD THE RESULTS 
#### AT LINE 1786

model_fulltime11 <- btergm(netlist ~ edges + mutual() 
                           + nodeocov('Per Capita Income')
                           + nodeicov('Per Capita Income')
                           + edgecov('Alliance')
                           + edgecov('Border')
                           + edgecov('Aid'),
                           R = 50)
summary(model_fulltime11)

# IF BTERGM DOESN'T INSTALL FOR THE GRADER, JUST LOAD THE SUMMARY BELOW TO DISPLAY THE RESULTS 
load(file = "threatmod_summary.rda") # Load results.
sum_threatmod # Results.


##########################################################################################################
# TOPIC 3: The (not-so)Great Escape : Analyzing Political Exile.
##########################################################################################################

# DATASET USED : 10 mm dyadic events (1990 - 2005), Gary King.

# Preparation

load(file = "totalvector_generator.rda")
load("mem20.rda") #Load data from 2000-2005
load("mem95.rda") #Load data from 1995-2000
load("mem90.rda") #Load data from 1990-1995

totalvector_generator("<HIDE>","weeks") #HIDE is asylum events.
#Total political flight events.
asylum_totalvec <- totalvec
save(asylum_totalvec, file = "HIDE_vec_pre.rda")

#Analysis.....
l3 <- LK.test(asylum_totalvec,"LR",nsim = 200)
l3

#Graphs....
df <- as.data.frame(asylum_totalvec)
df$index <- c(1:length(asylum_totalvec))

x <- seq(0, 30, length.out=100)
df2 <- with(df, data.frame(x = x, y = dexp(x,l3$estimate)))

Asylumplot <- ggplot(df,aes(x=asylum_totalvec)) + 
  geom_histogram(aes(y=..density..),binwidth = 0.8,color = "black",fill = alpha("black",0.1)) +
  geom_line(data = df2, aes(x = x, y = y,color = "Exponential"), size = 1) +
  geom_density(aes(y=..density..),fill ="#FF6666",alpha = 0.4) +
  scale_x_continuous(name = "Weeks since last event") +
  scale_y_continuous(name = "Density") +
  scale_color_manual(name = "Distributions", 
                     values = c("Exponential" = "#244747")) +
  ggtitle("Asylum") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold.italic"))
Asylumplot

############################### Let's graph the asylum network from 2000-2005 as an
############################### illustration.

load(file = "data20.rda")
data20 <- subset(data20,data20$EventForm == "<HIDE>")
#save(data20, file = "asylgraph_vec20.rda")
############### GRAPH WORK STARTS.

nodes20 <- as_tibble(union(levels(droplevels(data20$SrcName)),levels(droplevels(data20$TgtName))))
nodes <- unique(nodes20)

colnames(nodes) <- "label"
nodes <- nodes %>% rowid_to_column("id")

data1 <- data20

attach(data1)

per_route <- data1 %>%  
  group_by(SrcName, TgtName) %>%
  dplyr::summarise(weight = n()) %>% 
  ungroup()
per_route

detach(data1)

edges <- per_route %>% 
  left_join(nodes, by = c("SrcName" = "label")) %>% 
  dplyr::rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("TgtName" = "label")) %>% 
  dplyr::rename(to = id)
edges <- select(edges, from, to, weight)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

deg <- igraph::degree(routes_igraph, mode="all")

V(routes_igraph)$color <- "tomato"
E(routes_igraph)$arrow.size <- .4
E(routes_igraph)$edge.color <- "gray80"
E(routes_igraph)$width <- 1+E(routes_igraph)$weight

#save(routes_igraph,file="asylum_graph.rda")
plot(routes_igraph,vertex.size = 13,vertex.label.cex = 0.9,vertex.label.color = "black")
#Use zoom to see it better.

'pdf("fleegraphFINAL.pdf",10,10) # Saves the plot as a pdf.
plot(routes_igraph,vertex.size = 13,vertex.label.cex = 0.9,vertex.label.color = "black")
dev.off()
'

# Analysis

load(file = "borders.mat.rda") 
load(file = "wars_by_year.rda") 
load(file = "ally_by_year.rda")
load(file = "data90.rda")
load(file = "data95.rda") 
load(file = "data20.rda")

load(file = "civil90s.rda")
civil90 <- subset(civil, civil$SrcSector == "<INSU>")
load(file = "civil95s.rda") 
civil95 <- subset(civil, civil$SrcSector == "<INSU>")
load(file = "civil20s.rda") 
civil20 <- subset(civil, civil$SrcSector == "<INSU>")

for (i in 1:15){
  if(i > 5){
    if(i < 11){
      data <- data95
      civil <- civil95
    } else { civil <- civil20
    data <- data20 }
  } else { civil <- civil90 
  data <- data90 }
  data <- subset(data,data$EventForm == "<HIDE>")
  data <- dplyr::select(data,"EventDate", "SrcName","TgtName")
  civil <- dplyr::select(civil,"EventDate","SrcName","TgtName")
  if(i > 5){
    if(i < 11){
      data$EventDate <- ymd(data$EventDate)
      civil$EventDate <- ymd(civil$EventDate)
    } else {data$EventDate <- mdy_hms(data$EventDate)
    civil$EventDate <- mdy_hms(civil$EventDate)}
  } else {data$EventDate <- mdy(data$EventDate)
  civil$EventDate <- mdy(civil$EventDate)}
  
  data <- data[order(data$EventDate),]
  civil <- civil[order(civil$EventDate),]
  
  j <- i + 1989
  k <- i + 1990
  startdate <- paste(j,"/01/01",sep = "")
  enddate <- paste(k,"/01/01",sep = "")
  data <- subset(data,data$EventDate < enddate & data$EventDate >= startdate) ###################### 1990 - 1995.
  civil  <- subset(civil,  civil$EventDate < enddate & civil$EventDate  >= startdate)
  
  data <- data[,-1]
  civil <- civil[,-1]
  
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  data$counter <- c(rep(1,nrow(data)))
  
  civil$SrcName <- as.character(civil$SrcName)
  civil$TgtName <- as.character(civil$TgtName)
  civil$counter <- c(rep(1,nrow(civil)))
  
  civil <- ddply(civil,.(SrcName,TgtName),nrow)
  civil <- civil[,-2]
  colnames(civil)[1] <- "country"
  
  ally1990 <- ally_by_year[[i]]
  war1990 <- wars_by_year[[i]]
  
  ally1990_1 <- ally1990
  temp <- ally1990_1[,1]
  ally1990_1[,1] <- ally1990_1[,2]
  ally1990_1[,2] <- temp
  ally1990 <- rbind(ally1990, ally1990_1) # Makes alliances edglist "complete"
  
  colnames(borders.mat)[3] <- "UK_"
  rownames(borders.mat)[3] <- "UK_"
  
  borders.mat_without_NA <- as.matrix(borders.mat[-(1:2),-(1:2)])
  
  bord <- EdgelistFromAdjacency(borders.mat_without_NA, nodelist = colnames(borders.mat_without_NA))
  
  landlock <- borders.mat[,1]
  
  m1 <- merge(war1990[,-4],ally1990,by = c("namea","nameb"),all = TRUE)
  colnames(m1)[3] <- "Wars"
  colnames(m1)[4] <- "Alliance"
  m1[,3] <- ifelse(is.na(m1[,3]), 0, m1[,3])
  m1[,4] <- ifelse(is.na(m1[,4]), 0, 1)
  
  colnames(bord)[1] <- "namea"
  colnames(bord)[2] <- "nameb"
  
  m1 <- merge(m1,bord, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[5] <- "border"
  
  colnames(data)[1] <- "namea"
  colnames(data)[2] <- "nameb"
  
  m1 <- merge(m1, data, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[ncol(m1)] <- "Asylum"
  
  m1[is.na(m1)] = 0
  
  load(file = "GDPpc.mat.rda") 
  attGDPpc$country <- as.character(attGDPpc$country)
  attGDPpc$country[attGDPpc$country == "_UK"] <- "UK_"
  node.att.1990 <- attGDPpc[,c(1,i+1)] # Create GDP node att column
  colnames(node.att.1990)[2] <- "GDP"
  
  node.att.1990 <- merge(node.att.1990, civil, by = "country", all = TRUE) #Merging civil war data.
  colnames(node.att.1990)[3] <- "CivilWars"
  node.att.1990$CivilWars[is.na(node.att.1990$CivilWars)] = 0
  
  load(file = "HDI.mat.rda") 
  attHDI$country <- as.character(attHDI$country)
  attHDI$country[attHDI$country == "_UK"] <- "UK_"
  node.att.1990 <- merge(node.att.1990, attHDI[,c(1,i+1)], by = "country") # Merging HDI node att
  colnames(node.att.1990)[4] <- "HDI"
  
  Religion <- read.csv("major_religion.csv", head = TRUE, sep=",") 
  Religion <- select(Religion, "Code", "major")
  colnames(Religion)[1] <- "country"
  Religion$country <- as.character(Religion$country)
  node.att.1990 <- merge(node.att.1990, Religion, by = "country") # Merging religion node att
  colnames(node.att.1990)[5] <- "RELIGION"
  
  node.att.1990 <- node.att.1990[as.character(node.att.1990$GDP)!= "" ,]
  node.att.1990 <- na.omit(node.att.1990)
  
  edge.att.1990 <- filter(m1, is.element(m1$namea, node.att.1990$country) & is.element(m1$nameb, node.att.1990$country)) 
  node.att.1990 <- filter(node.att.1990, is.element(node.att.1990$country, edge.att.1990$namea) & is.element(node.att.1990$country, edge.att.1990$nameb)) 
  
  edge.att.1990 <- edge.att.1990[order(edge.att.1990$namea, edge.att.1990$nameb),]
  
  asylum_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,ncol(m1))])
  
  asylumnet <- network(asylum_adj_1990$adjacency,directed = TRUE,matrix.type = "adjacency")
  
  network::set.vertex.attribute(asylumnet, 'Per Capita Income', as.numeric(node.att.1990$GDP))
  network::set.vertex.attribute(asylumnet, 'Civil Conflicts', as.numeric(node.att.1990$CivilWars))
  network::set.vertex.attribute(asylumnet, 'HDI', node.att.1990$HDI)
  network::set.vertex.attribute(asylumnet, 'Religion', as.character(node.att.1990$RELIGION))
  #network::set.network.attribute(asylumnet,'Wars', edge.att.1990$Wars)
  network::set.network.attribute(asylumnet,'Alliance', edge.att.1990$Alliance)
  network::set.network.attribute(asylumnet,'Border', edge.att.1990$border)
  
  
  q <- paste("asylumnet",j,"final",".rda", sep = "")
  save(asylumnet, file = q)
}

load(file = 'asylumnet2000final.rda')
asylumnet -> anet2000
load(file = 'asylumnet2001final.rda')
asylumnet -> anet2001
load(file = 'asylumnet2002final.rda')
asylumnet -> anet2002
load(file = 'asylumnet2003final.rda')
asylumnet -> anet2003
load(file = 'asylumnet2004final.rda')
asylumnet -> anet2004
load(file = 'asylumnet1990final.rda')
asylumnet -> anet1990
load(file = 'asylumnet1991final.rda')
asylumnet -> anet1991
load(file = 'asylumnet1992final.rda')
asylumnet -> anet1992
load(file = 'asylumnet1993final.rda')
asylumnet -> anet1993
load(file = 'asylumnet1994final.rda')
asylumnet -> anet1994
load(file = 'asylumnet1995final.rda')
asylumnet -> anet1995
load(file = 'asylumnet1996final.rda')
asylumnet -> anet1996
load(file = 'asylumnet1997final.rda')
asylumnet -> anet1997
load(file = 'asylumnet1998final.rda')
asylumnet -> anet1998
load(file = 'asylumnet1999final.rda')
asylumnet -> anet1999
netlist <- list(anet1990,anet1991,anet1992,anet1993,anet1994
                ,anet1995,anet1996,anet1997,anet1998,anet1999,
                anet2000,anet2001,anet2002,anet2003,anet2004)

#### CAUTION! THE FOLLOWING CODE MIGHT TAKE BETWEEN 5-10 MINUTES TO RUN, IF YOU
#### CAN'T/ DON'T WANT TO RUN THE ACTUAL BTERGM ANALYSIS, JUST LOAD THE RESULTS 
#### AT LINE 2089

asylfull6_rel <- btergm(netlist ~ edges + mutual()
                        + nodeocov('Per Capita Income')
                        + nodeicov('Per Capita Income')
                        + nodematch('Religion')
                        + nodeocov('Civil Conflicts')
                        + nodeicov('Civil Conflicts')
                        + nodeocov('HDI')
                        + nodeicov('HDI')
                        + edgecov('Alliance')
                        + edgecov('Border')
                        ,
                        R = 50
)
sum_asylnet <- summary(asylfull6_rel)
summary(asylfull6_rel)

# IF BTERGM DOESN'T INSTALL FOR THE GRADER, JUST LOAD THE SUMMARY BELOW TO DISPLAY THE RESULTS 
load(file = "asylmod_summary.rda") # Load results.
sum_asylnet # Results.



##########################################################################################################
# TOPIC 5: The World Belligerence Index : Constructing a Global Index using PCA
##########################################################################################################

## A) PCA

# Prepare data for PCA

## Load data and create counts for each action type

load("data90.rda")
load("data95.rda")
load("data20.rda")

data <- rbind(data90, data95, data20) # Bind together
data %>% select(SrcName, EventForm) -> data # Select "actors" and "actions" columns

Actions <- data %>%  
  group_by(SrcName, EventForm) %>%
  dplyr::summarise(weight = n()) %>% 
  ungroup()

Actions <- tail(Actions, -32)
colnames(Actions) <- c("Countries", "Events", "Counts")

countries <- as.data.frame(sort(unique(data$SrcName)))
countries <- tail(countries, -5) # Remove actors that are not countries
events <- as.data.frame(sort(unique(data$EventForm)))

PCA_data <- expand.grid(c(countries, events))
colnames(PCA_data) <- c("Countries", "Events")
PCA_data <- PCA_data[order(PCA_data$Countries),]

PCA_data_1 <- merge(PCA_data, Actions, by = c("Countries", "Events") , all.x = TRUE)
PCA_data_1$Counts[is.na(PCA_data_1$Counts)] = 0

PCA_data_1 <- reshape(PCA_data_1, idvar = "Countries", timevar = "Events", direction = "wide")

## Save matrix and create a transpose of it just in case it could be useful

save(PCA_data_1, file = "PCA_data.rda")

names(PCA_data_1) <- gsub("Counts.", "", names(PCA_data_1), fixed = TRUE)
names(PCA_data_1) <- gsub("<", "", names(PCA_data_1), fixed = TRUE)
names(PCA_data_1) <- gsub(">", "", names(PCA_data_1), fixed = TRUE)

PCA_Transp <- as.data.frame(t(PCA_data_1)) 
colnames(PCA_Transp) <- PCA_data_1$Countries
PCA_Transp <- PCA_Transp[-1,]

row.names(PCA_Transp) <- gsub("Counts.", "", row.names(PCA_Transp), fixed = TRUE)
row.names(PCA_Transp) <- gsub("<", "", row.names(PCA_Transp), fixed = TRUE)
row.names(PCA_Transp) <- gsub(">", "", row.names(PCA_Transp), fixed = TRUE)

save(PCA_Transp, file = "PCA_data_T.rda")

# PCA analysis

'Load the matrix and trim the number of columns to include in PCA

 Note that before trimming, I merge some vectors that, for our purposes
 would capture similar types of behavior
'
##########################
load(file = "PCA_data.rda")
#########################

names(PCA_data_1) <- gsub("Counts.", "", names(PCA_data_1), fixed = TRUE)
names(PCA_data_1) <- gsub("<", "", names(PCA_data_1), fixed = TRUE)
names(PCA_data_1) <- gsub(">", "", names(PCA_data_1), fixed = TRUE)

PCA_data_1 <- head(PCA_data_1,-15) # Remove last non country entries

attach(PCA_data_1)

PCA_data_1$ADIS_1 <- ADIS+TDIS # Armed forces demonstrations
PCA_data_1$BIOA <- CBIO+TCBR # Biochemical attacks or threats
PCA_data_1$COMP_1 <- COMP+FCOM+ICOM # Various types of complaints
PCA_data_1$ESAN <- EASS+EESB # Easening of sanctions
PCA_data_1$EAID <- EEAI+EHAI # Extensions of aid
PCA_data_1$HAID <- HAID+HECO+REDA # Reductions of aid
PCA_data_1$POLPER <- HIDE+POAR # Political fights and political arrests
PCA_data_1$MREADY <- MALT+MOBL+MTHR # Military readiness (reported increae in readiness of armed forces,activate inactive forces,military threats)
PCA_data_1$UNREST <- RIOT+STRI # Riots and strikes
PCA_data_1$THRT_1 <- THRT+TATT+TRSA+TSAN+TUNS+ULTI # Various types of threats (excluding war threats)

detach(PCA_data_1)
attach(PCA_data_1)


## First attempt, throw all vectors in.

PCA_data_1 <- select(PCA_data_1, "Countries", "ADIS_1", "AERI", "APOL", "ATNE", "ASSA", "BANA", "BEAT",
                     "BLAM", "BLAW", "BREL", "BVIO", "CALL", "BIOA", "CENS", "CLAS", "COLL", "COMP_1", "CORP",
                     "COUP", "DEFY", "DMOB", "DWAR", "ESAN", "EAID", "EMAI", "EXIL", "FORG", "GASY", "GRPG",
                     "HAID", "POLPER", "HTAK", "IMPR", "MREADY", "MDEM", "MOCC", "MONI", "NEGO", "NUCA", "OPEN",
                     "PASS", "PEXE", "RAID", "RALL", "RCEA", "UNREST", "RWCF", "SANC", "SEZR", "SHEP", "THRT_1",
                     "TWAR")
detach(PCA_data_1)
attach(PCA_data_1)


## Create symmetric matrix
m<- nrow(PCA_data_1) ; m
A <- PCA_data_1[,2:53] # Select relevant vectors
S <- var(A) # Covariance symmetric matrix

# A.PCA <- scale(A, center = TRUE, scale = c(rep(sqrt(m-1),52)))
# check <- t(A.PCA)%*%A.PCA 
# check ; S # The two symmetric matrices match. Proceed using matrix S

#Now we have a symmetric matrix to which the spectral theorem applies.
Eig <- eigen(S)
Eig.vals <- Eig$values
P <- Eig$vectors; P # This is the change of basis matrix, composed of the eigenvectors
#plot(Eig.vals, type = "l", from = 0, to = 10)
#abline(h=10)
#sum(Eig.vals >= 10) 
# As shown in the graph, there are 10 eigenvectors with eigenvalue of 10 or above

## In case of PCA, "variance" means summative variance or multivariate variability or overall
# variability or total variability. Below, I calculate the summative variance and then
# calculate the proportion of this explained by each eigenvector. Then I take the eigenvalues
# and divide them by this amount to obtain the proportion of the variance. In the loop, I also
# store the variances.
summ_var <- 0 
variances <- numeric(52)

for (i in 1:52) {
  for (j in 1:52){
    if (i == j){
      summ_var <- summ_var + S[i,j]
      variances[j] <- S[i,j]
    }
  }
}

prop.var <- Eig.vals/summ_var
sum(prop.var) # adds up to 1, we are in business. 

PC_index <- 1:52
ggplot(data=as.data.frame(prop.var), aes(y=prop.var*100, x=PC_index)) + geom_line(linetype = "dashed") + geom_point() + geom_hline(aes(yintercept=10), col="red") + geom_hline(aes(yintercept=5), col="blue") + labs(y = "%", x = "Components", title="Proportion of variance explained") + scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
# Horizonal line at 10% and 5% of the variation. Only one component above this threshold.

prop.var[1] # It seems like the first component, the one with the largest eigenvalue, accounts for 
# over 92 percent of the total variance. 

# Add relevant eigenvectors from P to the dataframe
data <- cbind(S,P[,1])  
colnames(data) <- c(colnames(A), "Eig1")
options(scipen=999)
as.matrix(signif(sort(data[,53])), digits=2)

detach(PCA_data_1)

## 2nd attempt: Try enforcing a threshold for minimum variance. Which action vectors remain?

## Variance of at least 1?

vars<-as.data.frame(variances)
rownames(vars) <- c("ADIS_1", "AERI", "APOL", "ATNE", "ASSA", "BANA", "BEAT",
                    "BLAM", "BLAW", "BREL", "BVIO", "CALL", "BIOA", "CENS", "CLAS", "COLL", "COMP_1", "CORP",
                    "COUP", "DEFY", "DMOB", "DWAR", "ESAN", "EAID", "EMAI", "EXIL", "FORG", "GASY", "GRPG",
                    "HAID", "POLPER", "HTAK", "IMPR", "MREADY", "MDEM", "MOCC", "MONI", "NEGO", "NUCA", "OPEN",
                    "PASS", "PEXE", "RAID", "RALL", "RCEA", "UNREST", "RWCF", "SANC", "SEZR", "SHEP", "THRT_1",
                    "TWAR")

vars$threshold.met <- (vars$variances >= 1)
variances.trimmed <- subset(vars, vars$threshold.met == TRUE)
row.names(variances.trimmed)

PCA_data_2 <- select(PCA_data_1, "Countries", "AERI","BANA","BLAM","BREL","CALL","CLAS","COLL","COMP_1", "CORP","DMOB",
                     "DWAR","ESAN","EAID", "EMAI","EXIL","GRPG","HAID","POLPER", "IMPR","MDEM","NEGO","OPEN",
                     "PASS","PEXE","RAID", "RALL","SANC","SHEP","THRT_1")

attach(PCA_data_2)

## Create symmetric matrix
m_2<- nrow(PCA_data_2) ; m_2
A_2 <- PCA_data_2[,2:30] # Select relevant vectors
S_2 <- var(A_2) # Covariance symmetric matrix

#Now we have a symmetric matrix to which the spectral theorem applies.
Eig_2 <- eigen(S_2)
Eig.vals_2 <- Eig_2$values
P_2 <- Eig_2$vectors; P_2 # This is the change of basis matrix, composed of the eigenvectors

## Lets once again calculate total variance and the proportions explained by each component
summ_var_2 <- 0 
variances_2 <- numeric(29)

for (i in 1:29) {
  for (j in 1:29){
    if (i == j){
      summ_var_2 <- summ_var_2 + S_2[i,j]
      variances_2[j] <- S_2[i,j]
    }
  }
}

prop.var_2 <- Eig.vals_2/summ_var_2
sum(prop.var_2) # adds up to ~1 (not quite 1 probably due to rounding), we are in business. 

PC_index <- 1:29
ggplot(data=as.data.frame(prop.var_2), aes(y=prop.var_2*100, x=PC_index)) + geom_line(linetype = "dashed") + geom_point() + geom_hline(aes(yintercept=10), col="red") + geom_hline(aes(yintercept=5), col="blue") + labs(y = "%", x = "Components", title="Proportion of variance explained") + scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
# Horizonal line at 10% and 5% of the variation. Only one component above this threshold.

prop.var_2[1] # It seems like the first component, the one with the largest eigenvalue, accounts for 
# over 92 percent of the total variance. 

# Add relevant eigenvectors from P_2 to the dataframe
try2 <- as.data.frame(P_2[,1:3])
data_2 <- cbind(S_2,try2)  
colnames(data_2) <- c(colnames(A_2), "Eig1", "Eig2", "Eig3")
row.names(try2) <- c(colnames(A_2))
options(scipen=999)

try2 <- try2[order(try2$V1),]
PC1 <- as.data.frame(try2$V1)
rownames(PC1) <- rownames(try2) ; PC1

try2 <- try2[order(try2$V2),]
PC2 <- as.data.frame(try2$V2)
rownames(PC2) <- rownames(try2) ; PC2

try2 <- try2[order(try2$V3),]
PC3 <- as.data.frame(try2$V3)
rownames(PC3) <- rownames(try2) ; PC3

detach(PCA_data_2)

## Third attempt: removed variables BANA (restriction on civil activity), BLAM (criticism), CALL (call for
## action), OPEN (disclose info)

PCA_data_3 <- select(PCA_data_2, "Countries", "AERI","BREL","CLAS","COLL","COMP_1", "CORP","DMOB",
                     "DWAR","ESAN","EAID", "EMAI","EXIL","GRPG","HAID","POLPER", "IMPR","MDEM","NEGO",
                     "PASS","PEXE","RAID", "RALL","SANC","SHEP","THRT_1")

attach(PCA_data_3)

## Create symmetric matrix
m_3<- nrow(PCA_data_3) ; m_3
A_3 <- PCA_data_3[,2:26] # Select relevant vectors
S_3 <- var(A_3) # Covariance symmetric matrix

#Now we have a symmetric matrix to which the spectral theorem applies.
Eig_3 <- eigen(S_3)
Eig.vals_3 <- Eig_3$values
P_3 <- Eig_3$vectors; P_3 # This is the change of basis matrix, composed of the eigenvectors

## Lets once again calculate total variance and the proportions explained by each component
summ_var_3 <- 0 
variances_3 <- numeric(25)

for (i in 1:25) {
  for (j in 1:25){
    if (i == j){
      summ_var_3 <- summ_var_3 + S_3[i,j]
      variances_3[j] <- S_3[i,j]
    }
  }
}

prop.var_3 <- Eig.vals_3/summ_var_3
sum(prop.var_3) # adds up to 1, we are in business. 

PC_index <- 1:25
ggplot(data=as.data.frame(prop.var_3), aes(y=prop.var_3*100, x=PC_index)) + geom_line(linetype = "dashed") + geom_point() + geom_hline(aes(yintercept=10), col="red") + geom_hline(aes(yintercept=5), col="blue") + labs(y = "%", x = "Components", title="Proportion of variance explained") + scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
# Horizonal line at 10% and 5% of the variation. Only one component above this threshold.

prop.var_3[1] # It seems like the first component, the one with the largest eigenvalue, accounts for 
# over 93 percent of the total variance. 

# Add relevant eigenvectors from P_3 to the dataframe
try3 <- as.data.frame(P_3[,1:3])
data_3 <- cbind(S_3,try3)  
colnames(data_3) <- c(colnames(A_3), "Eig1", "Eig2", "Eig3")
row.names(try3) <- c(colnames(A_3))
options(scipen=999)

try3 <- try3[order(try3$V1),] 
PC1 <- as.data.frame(try3$V1)
rownames(PC1) <- rownames(try3) ; PC1

try3 <- try3[order(try3$V2),]
PC2 <- as.data.frame(try3$V2)
rownames(PC2) <- rownames(try3) ; PC2

try3 <- try3[order(try3$V3),]
PC3 <- as.data.frame(try3$V3)
rownames(PC3) <- rownames(try3) ; PC3

detach(PCA_data_3)

## 4th attempt: use PC3 data, but convert counts to percentage of total events by column
total_counts <- colSums(PCA_data_3[,2:26]) ; total_counts
PCA_data_4 <- PCA_data_3

attach(PCA_data_4)

PCA_data_4[,2:26] <- scale(PCA_data_3[,2:26], center = FALSE, scale = total_counts)

## Create symmetric matrix
m_4<- nrow(PCA_data_4) ; m_4
A_4 <- PCA_data_4[,2:26] # Select relevant vectors
S_4 <- var(A_4) # Covariance symmetric matrix

#Now we have a symmetric matrix to which the spectral theorem applies.
Eig_4 <- eigen(S_4)
Eig.vals_4 <- Eig_4$values
P_4 <- Eig_4$vectors; P_4 # This is the change of basis matrix, composed of the eigenvectors

## Lets once again calculate total variance and the proportions explained by each component
summ_var_4 <- 0 
variances_4 <- numeric(25)

for (i in 1:25) {
  for (j in 1:25){
    if (i == j){
      summ_var_4 <- summ_var_4 + S_4[i,j]
      variances_4[j] <- S_4[i,j]
    }
  }
}

prop.var_4 <- Eig.vals_4/summ_var_4
sum(prop.var_4) # adds up to 1, we are in business. 

PC_index <- 1:25
ggplot(data=as.data.frame(prop.var_4), aes(y=prop.var_4*100, x=PC_index)) + geom_line(linetype = "dashed") + geom_point() + geom_hline(aes(yintercept=10), col="red") + geom_hline(aes(yintercept=5), col="blue") + labs(y = "%", x = "Components", title="Proportion of variance explained") + scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
# Horizonal line at 10% and 5% of the variation. 3 components above the 10% threshold.

prop.var_4[1] # It seems like the first component, the one with the largest eigenvalue, accounts for 
# over 52.5 percent of the total variance. 
prop.var_4[2] # It seems like the second component, the one with the second largest eigenvalue, accounts for 
# over 18 percent of the total variance. 
prop.var_4[3] # It seems like the second component, the one with the second largest eigenvalue, accounts for 
# over 12.7 percent of the total variance. 
prop.var_4[4] # It seems like the second component, the one with the second largest eigenvalue, accounts for 
# over 5 percent of the total variance. 

# Add relevant eigenvectors from P_4 to the dataframe
try4 <- as.data.frame(P_4[,1:4])
data_4 <- cbind(S_4,try4)  
colnames(data_4) <- c(colnames(A_4), "Eig1", "Eig2", "Eig3", "Eig4")
row.names(try4) <- c(colnames(A_4))
options(scipen=999)

try4 <- try4[order(try4$V1),]
PC1 <- as.data.frame(try4$V1)
rownames(PC1) <- rownames(try4) ; PC1

try4 <- try4[order(try4$V2),]
PC2 <- as.data.frame(try4$V2)
rownames(PC2) <- rownames(try4) ; PC2

try4 <- try4[order(try4$V3),]
PC3 <- as.data.frame(try4$V3)
rownames(PC3) <- rownames(try4) ; PC3

try4 <- try4[order(try4$V4),]
PC4 <- as.data.frame(try4$V4)
rownames(PC4) <- rownames(try4) ; PC4

detach(PCA_data_4)

## One last attempt, second PC in try4 had .99 loading on CORP. Try removing it

PCA_data_5 <- select(PCA_data_2, "Countries", "AERI","BREL","CLAS","COLL","COMP_1", "DMOB",
                     "DWAR","ESAN","EAID", "EMAI","EXIL","GRPG","HAID","POLPER", "IMPR","MDEM","NEGO",
                     "PASS","PEXE","RAID", "RALL","SANC","SHEP","THRT_1")

attach(PCA_data_5)

total_counts <- colSums(PCA_data_5[,2:25]) ; total_counts
PCA_data_5[,2:25] <- scale(PCA_data_5[,2:25], center = FALSE, scale = total_counts)

## Create symmetric matrix
m_5<- nrow(PCA_data_5) ; m_5
A_5 <- PCA_data_5[,2:25] # Select relevant vectors
S_5 <- var(A_5) # Covariance symmetric matrix

#Now we have a symmetric matrix to which the spectral theorem applies.
Eig_5 <- eigen(S_5)
Eig.vals_5 <- Eig_5$values
P_5 <- Eig_5$vectors; P_5 # This is the change of basis matrix, composed of the eigenvectors

## Lets once again calculate total variance and the proportions explained by each component
summ_var_5 <- 0 
variances_5 <- numeric(24)

for (i in 1:24) {
  for (j in 1:24){
    if (i == j){
      summ_var_5 <- summ_var_5 + S_5[i,j]
      variances_5[j] <- S_5[i,j]
    }
  }
}

prop.var_5 <- Eig.vals_5/summ_var_5
sum(prop.var_5) # adds up to 1, we are in business. 

PC_index <- 1:24
ggplot(data=as.data.frame(prop.var_5), aes(y=prop.var_5*100, x=PC_index)) + geom_line(linetype = "dashed") + geom_point() + geom_hline(aes(yintercept=10), col="red") + geom_hline(aes(yintercept=5), col="blue") + labs(y = "%", x = "Components", title="Proportion of variance explained") + scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
# Horizonal line at 10% and 5% of the variation. 2 components above the 10% threshold.

prop.var_5[1] # It seems like the first component, the one with the largest eigenvalue, accounts for 
# over 64.1 percent of the total variance. 
prop.var_5[2] # It seems like the second component, the one with the second largest eigenvalue, accounts for 
# over 15 percent of the total variance. 
prop.var_5[3] # It seems like the second component, the one with the second largest eigenvalue, accounts for 
# over 6.7 percent of the total variance. 
prop.var_5[5] # It seems like the second component, the one with the second largest eigenvalue, accounts for 
# just below 2 percent of the total variance. 

# Add relevant eigenvectors from P_5 to the dataframe
try5 <- as.data.frame(P_5[,1:3])
data_5 <- cbind(S_5,try5)  
colnames(data_5) <- c(colnames(A_5), "Eig1", "Eig2", "Eig3")
row.names(try5) <- c(colnames(A_5))
options(scipen=999)

try5 <- try5[order(try5$V1),]
PC1 <- as.data.frame(try5$V1)
rownames(PC1) <- rownames(try5) ; PC1

try5 <- try5[order(try5$V2),]
PC2 <- as.data.frame(try5$V2)
rownames(PC2) <- rownames(try5) ; PC2

try5 <- try5[order(try5$V3),]
PC3 <- as.data.frame(try5$V3)
rownames(PC3) <- rownames(try5) ; PC3

'
The first component PC1 has negative loadings for each action type and is likely
accounting from something related to the overall activity level of countries in
the international arena. PC2 and PC3 on the other hand do seem to capture something
related to belligerance. We focus on PC2 since this accounts for over 15% of 
the total variation. On PC2 we see positive loadings on actions that we
might normally associate to belligerant and/or aggresive nations. The actions with 
positive loadings are:
RAID (Armed actions)
POLPER (Political flight/arrests)
AERI (Missile attacks)
CLAS (Armed battle)
PASS (All uses of non-armed physical force in assaults against people)
EXIL (Expel)
PEXE (Small arms attack)
GRPG (Artillery attack)
'
PC2

detach(PCA_data_5)

## Create the belligerance index!

Belligerance.index <- as.data.frame(PCA_data_5$Countries)
Index <- (PCA_data_5$RAID+PCA_data_5$POLPER+PCA_data_5$AERI+PCA_data_5$CLAS+PCA_data_5$PASS+PCA_data_5$EXIL+PCA_data_5$PEXE+PCA_data_5$GRPG)/8
Belligerance.index$Index <- Index*100
Belligerance.index <- Belligerance.index[order(-Belligerance.index$Index),]
colnames(Belligerance.index) <- c("country", "Index")

attach(Belligerance.index)

# Check highest values (plot only if index is above 1 after scaling by a factor of 100)
Belligerance.index_high <- subset(Belligerance.index, Belligerance.index$Index>1)
colnames(Belligerance.index_high) <- c("Country", "Index")

p<-ggplot(data=Belligerance.index_high, aes(x=reorder(Country, -Index), y=Index)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
p1 <- p + theme_economist() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Belligerant countries", x = "Country") + geom_hline(aes(yintercept=1), col="red") 
p1 + scale_y_continuous(breaks = c(1, 5, 10,15))

detach(Belligerance.index)

## B) Regression on Belligerance Index

## Set up regressions on belligerance index

## Load node attributes (we will use 2003 nodal attributes for this regression)
load(file = "Reg_attributes.rda") 
Reg_vars <- merge(Belligerance.index,node.att.regressions,by = "country",all = TRUE)

Reg_vars <- na.omit(Reg_vars)
#Reg_vars$Index <- Reg_vars$Index*100
Reg_vars$GDP <- log(as.numeric(Reg_vars$GDP))

attach(Reg_vars)

ggplot(Reg_vars, aes(x = Reg_vars$GDP, y = Reg_vars$Index)) + geom_point() + geom_hline(aes(yintercept=5), col="black") + geom_hline(aes(yintercept=3), col="red") + geom_hline(aes(yintercept=1.5), col="blue") + labs(x="log GDP per capita", y="Belligerance Index")
cor(Reg_vars$GDP, Reg_vars$Index)

ggplot(Reg_vars, aes(x = Reg_vars$HDI, y = Reg_vars$Index)) + geom_point() + geom_hline(aes(yintercept=5), col="black") + geom_hline(aes(yintercept=3), col="red") + geom_hline(aes(yintercept=1.5), col="blue") + labs(x="HDI", y="Belligerance Index")
cor(Reg_vars$HDI, Reg_vars$Index)

ggplot(Reg_vars, aes(x = Reg_vars$No.border, y = Reg_vars$Index)) + geom_point() + geom_hline(aes(yintercept=5), col="black") + geom_hline(aes(yintercept=3), col="red") + geom_hline(aes(yintercept=1.5), col="blue") + labs(x="No border = 1", y="Belligerance Index")
cor(Reg_vars$No.border, Reg_vars$Index)

ggplot(Reg_vars, aes(x = Reg_vars$CivilWars, y = Reg_vars$Index)) + geom_point() + geom_hline(aes(yintercept=5), col="black") + geom_hline(aes(yintercept=3), col="red") + geom_hline(aes(yintercept=1.5), col="blue") + labs(x="Civil war events", y="Belligerance Index")
cor(Reg_vars$CivilWars, Reg_vars$Index)

# Create matrix A from which we can create the "trimmed" projection matrix formula
# that gives us the coefficients.

v1 <- c(rep(1, nrow(Reg_vars)))
A <- cbind(v1, Reg_vars$GDP, Reg_vars$HDI, Reg_vars$CivilWars, Reg_vars$No.border)
coeff <- solve(t(A)%*%A)%*%t(A)%*%Reg_vars$Index


reg_model <- lm(Reg_vars$Index ~ Reg_vars$GDP + Reg_vars$HDI + Reg_vars$CivilWars + Reg_vars$No.border)
summary(reg_model) ; coeff
# The coefficients match those provided by the integrated lm function. Lm reveals some
# significant coefficients. Specifically, it suggests that HDI and the ocurrance
# of civil wars lead to increases in the level of belligerance shown by countries
# as measured by our index. However, given suspicion (motivated by the initial
# scatterplots of the dependent variable with each individual regressor)
# that a linear model might not be appropriate for this analysis, we advise not to 
# read too much into these.

stargazer(reg_model, type="text",
          dep.var.labels=c("Belligerance Index"),
          covariate.labels=c("Log GDP per cap.","Human Development Index (HDI)","Civil war",
                             "Lacking border"), out="linear_model.pdf")

detach(Reg_vars)

##########################################################################################################
# TOPIC 4: Minding their own business : Does Geographical Isolation impact development?
##########################################################################################################

# Preparation

# Saving attributes 2003 attributes for chi-squared analysis and permutation test purposes. We select this
# year for that exercise because we have the least amount of missing data in this year. Note that 
# the resulting "Reg_attributes.rda" file will also be used at the end of topic 5.

load(file = "borders.mat.rda") 
load(file = "wars_by_year.rda") 
load(file = "ally_by_year.rda")
load(file = "data90.rda")
load(file = "data95.rda") 
load(file = "data20.rda")

load(file = "civil90s.rda")
civil90 <- civil
load(file = "civil95s.rda") 
civil95 <- civil
load(file = "civil20s.rda") 
civil20 <- civil

for (i in 14:14){ # select only 2003 data (14th year of the 15 between 1990 and 2004)
  if(i > 5){
    if(i < 11){
      data <- data95
      civil <- civil95
    } else { civil <- civil20
    data <- data20 }
  } else { civil <- civil90 
  data <- data90 }
  data <- subset(data,data$EventForm == "<HIDE>")
  data <- dplyr::select(data,"EventDate", "SrcName","TgtName")
  civil <- dplyr::select(civil,"EventDate","SrcName","TgtName")
  if(i > 5){
    if(i < 11){
      data$EventDate <- ymd(data$EventDate)
      civil$EventDate <- ymd(civil$EventDate)
    } else {data$EventDate <- mdy_hms(data$EventDate)
    civil$EventDate <- mdy_hms(civil$EventDate)}
  } else {data$EventDate <- mdy(data$EventDate)
  civil$EventDate <- mdy(civil$EventDate)}
  
  data <- data[order(data$EventDate),]
  civil <- civil[order(civil$EventDate),]
  
  j <- i + 1989
  k <- i + 1990
  startdate <- paste(j,"/01/01",sep = "")
  enddate <- paste(k,"/01/01",sep = "")
  data <- subset(data,data$EventDate < enddate & data$EventDate >= startdate) ###################### 1990 - 1995.
  civil  <- subset(civil,  civil$EventDate < enddate & civil$EventDate  >= startdate)
  
  data <- data[,-1]
  civil <- civil[,-1]
  
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  data$counter <- c(rep(1,nrow(data)))
  
  civil$SrcName <- as.character(civil$SrcName)
  civil$TgtName <- as.character(civil$TgtName)
  civil$counter <- c(rep(1,nrow(civil)))
  
  civil <- ddply(civil,.(SrcName,TgtName),nrow)
  civil <- civil[,-2]
  colnames(civil)[1] <- "country"
  
  ally1990 <- ally_by_year[[i]]
  war1990 <- wars_by_year[[i]]
  
  ally1990_1 <- ally1990
  temp <- ally1990_1[,1]
  ally1990_1[,1] <- ally1990_1[,2]
  ally1990_1[,2] <- temp
  ally1990 <- rbind(ally1990, ally1990_1) # Makes alliances edglist "complete"
  
  colnames(borders.mat)[3] <- "UK_"
  rownames(borders.mat)[3] <- "UK_"
  
  #mig1990 <- EdgelistFromAdjacency(as.matrix(migrants1990[,2:190]), nodelist = colnames(migrants1990[,2:190]))
  
  borders.mat_without_NA <- as.matrix(borders.mat[-(1:2),-(1:2)])
  
  bord <- EdgelistFromAdjacency(borders.mat_without_NA, nodelist = colnames(borders.mat_without_NA))
  
  landlock <- as.data.frame(borders.mat[,1])
  landlock$country <- rownames(landlock)
  landlock <- landlock[-1,]
  colnames(landlock) <- c("No.Border", "country")
  
  m1 <- merge(war1990[,-4],ally1990,by = c("namea","nameb"),all = TRUE)
  colnames(m1)[3] <- "Wars"
  colnames(m1)[4] <- "Alliance"
  m1[,3] <- ifelse(is.na(m1[,3]), 0, m1[,3])
  m1[,4] <- ifelse(is.na(m1[,4]), 0, 1)
  
  colnames(bord)[1] <- "namea"
  colnames(bord)[2] <- "nameb"
  
  m1 <- merge(m1,bord, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[5] <- "border"
  
  #colnames(mig1990)[1] <- "namea"
  #colnames(mig1990)[2] <- "nameb"
  
  #m1 <- merge(m1, mig1990, by = c("namea", "nameb"), all = TRUE)
  #colnames(m1)[6] <- "Mig"
  
  colnames(data)[1] <- "namea"
  colnames(data)[2] <- "nameb"
  
  m1 <- merge(m1, data, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[ncol(m1)] <- "Asylum"
  
  m1[is.na(m1)] = 0
  
  load(file = "GDPpc.mat.rda") 
  attGDPpc$country <- as.character(attGDPpc$country)
  attGDPpc$country[attGDPpc$country == "_UK"] <- "UK_"
  node.att.regressions <- attGDPpc[,c(1,i+1)] # Create GDP node att column
  colnames(node.att.regressions)[2] <- "GDP"
  
  node.att.regressions <- merge(node.att.regressions, civil, by = "country", all = TRUE) #Merging civil war data.
  colnames(node.att.regressions)[3] <- "CivilWars"
  node.att.regressions$CivilWars[is.na(node.att.regressions$CivilWars)] = 0
  
  load(file = "HDI.mat.rda") 
  attHDI$country <- as.character(attHDI$country)
  attHDI$country[attHDI$country == "_UK"] <- "UK_"
  node.att.regressions <- merge(node.att.regressions, attHDI[,c(1,i+1)], by = "country") # Merging HDI node att
  colnames(node.att.regressions)[4] <- "HDI"
  
  node.att.regressions <- merge(node.att.regressions, landlock, by = "country") # Merging landlock node att
  colnames(node.att.regressions)[5] <- "No.border"
  
  node.att.regressions <- node.att.regressions[as.character(node.att.regressions$GDP)!= "" ,]
  node.att.regressions <- na.omit(node.att.regressions)
  
  edge.att.1990 <- filter(m1, is.element(m1$namea, node.att.regressions$country) & is.element(m1$nameb, node.att.regressions$country)) 
  node.att.regressions <- filter(node.att.regressions, is.element(node.att.regressions$country, edge.att.1990$namea) & is.element(node.att.regressions$country, edge.att.1990$nameb)) 
  
  edge.att.1990 <- edge.att.1990[order(edge.att.1990$namea, edge.att.1990$nameb),]
  
}

save(node.att.regressions, file = "Reg_attributes.rda") 


# Analysis

load(file = "Reg_attributes.rda") # These are 2003 node attributes that were also used in ERGM analyses
Reg_vars <- merge(Belligerance.index,node.att.regressions,by = "country",all = TRUE)
Reg_vars <- na.omit(Reg_vars)

## Do Chi-squared test: Border/no border & HDI classifications
chisq_data <- Reg_vars[,c(1,5:6)]
attach(chisq_data)

'The UNDP classifies each country into one of three development groups: Low human development for HDI 
scores between 0.0 and 0.5, Medium human development for HDI scores between 0.5 and 0.8. High human 
development for HDI scores between 0.8 and 1.0.'

## Create categorical version of HDI in accordance to UNDP guidelines
chisq_data$HDI_cat <-  cut(HDI, 
                           breaks=c(-Inf, 0.5, 0.8, Inf), 
                           labels=c("Low","Middle","High"))

## Observed table
attach(chisq_data)
Obs_table <- table(No.border, HDI_cat); Obs_table

'The Chi square test used in the Contingency table approach requires at least 80% of the cells 
to have an expected count greater than 5 or else the sum of the cell Chi squares will not have
a Chi square distribution. In our case, only 1/6 of the cells have a count lower than 5.
'

Obs = matrix(c(39, 3, 70, 18, 28, 7), ncol=3) ; Obs
colnames(Obs) = c('Low', 'Middle', 'High')
rownames(Obs) = c('Border', 'No border') ;Obs

rowsums <- c(sum(Obs[1,]),sum(Obs[2,])) ; rowsums
colsums <- c(sum(Obs[,1]),sum(Obs[,2]),sum(Obs[,3])) ; colsums
total <- sum(Obs)

# Expected table
Exp <- matrix(nrow=2, ncol=3) ; Exp

for (i in 1:2){
  for (j in 1:3){
    Exp[i,j] <- rowsums[i]*colsums[j]/total
  }
}

# Determine the degrees of freedom
dfs <- (nrow(Obs)-1)*(ncol(Obs)-1) 

ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
} # borrow Paul's function

chisq <- ChiSq(Obs,Exp) ; chisq
pvalue <- pchisq(chisq, dfs, lower.tail = FALSE); pvalue # 0.1447803
# This pvalue is not small enough for us to reject the null hypothesis of independence.
# There is over a 14% chance that the observed data pattern could have arisen by chance
# under the assumption that the null hypothesis of independence is true. We are not 
# comfortable with such a high probability for making a type I error (false positive), 
# and thus conclude that the we cannot reject the null hypothesis of independence in this case.

# We could have also used the integrated R command for carrying out a chi squared test 
# of independence, which uses Yates continuity correction to handle improve accuracy  
# after approximating discrete quantities with a continuous distribution 
# (See Chihara&Hesterberg, p. 370)

chisq.test(Obs) # This yields a chi squared statistic and a pvalue that are very 
# close to the ones we calculated by hand. As above, we cannot reject the null hypothesis
# that the two categorical variables are independent.

# Permutation test on Border/no Border & HDI level

idx <- which(No.border == 1) # Index for countries without a border

Mean.NoBorder <- mean(HDI[idx]) 
Mean.Border <- mean(HDI[-idx])
observed <- Mean.Border - Mean.NoBorder ; observed 
'On average, Countries that do have a border have HDIs 0.07096872 lower 
than countries without a border'

# Now carry out permutation test to check whether this difference is significant
N <- 10000
diff <- numeric(N)

for (i in 1:N){
  samp <- sample(nrow(chisq_data), sum(No.border == 1)) 
  # obtain random sample of size equal to number of countries without a border in the data
  weightSamp <- mean(HDI[samp]) # mean for random sample
  weightOther <- mean(HDI[-samp]) # mean for complement
  diff[i] <- weightOther - weightSamp # calculate the difference
}

breaks <- pretty(range(diff), n = nclass.FD(diff), min.n = 1)
bwidth <- breaks[2]-breaks[1]
ggplot(data = as.data.frame(diff),aes(diff)) + theme_economist() + geom_histogram(binwidth=bwidth,fill="white",colour="black") + geom_vline(aes(xintercept=observed), col="red") + labs(title = "Simulated differences", x = "Diff", y="Count")
# The red line is quite far into the left tail of the histogram. Seems 
# like the difference is significant from the graph, but lets calculate the
# exact p-value!

# Calculate pvalue
(sum(diff <= observed)+1)/(N+1) #One tailed: Check if the observed difference is large
# enough to determine that countries with a border have an HDI that is significantly 
# lower than countries that do not have a border. The p-value is very small, 0.02239776, meaning
# that that it is extremely unlikely (roughly a 2% chance) that the observed difference happened 
# by chance if the null hypothesis of equal HDIs were true. However, we did not try to establish
# the sign of the difference a priori, so we should instead carry out a two-tailed test.
# To do so, we multiply the p-value by 2.

((sum(diff <= observed)+1)/(N+1))*2 # Two tailed, multiply this pvalue by two
# The pvalue remains small (0.04479552). This means that there it is quite unlikely 
# (roughly 4.5% chance) that the observed difference happened by chance under the 
# assumption that the null hypothesis of equal HDIs were true. We can thus confidently 
# (at the 5% significance level) reject this null hypothesis and conclude that HDI levels
# for countries that do not have a border are significantly different from the HDI levels
# of countries that do have a border.

detach(chisq_data)
