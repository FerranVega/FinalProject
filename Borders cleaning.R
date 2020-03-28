
rm(list = ls()) # clear environment
library(tidyverse)

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
