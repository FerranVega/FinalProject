library(tidyverse)
#install.packages("plyr")
library(plyr)
library(dils)
library(data.table)
################################################################# EFINDEX
x <- subset(x, x$Year >= 1990 & x$Year <= 2005)
x$last <- paste(as.character(x$Country), as.character(x$Year))
x <- unique(x)
x <- select(x, -"last")
x <- spread(x, Year, EFindex)
x <- na.omit(x)
write.csv(x, "EFIndex_withoutNA.csv")
kingcountries <- x
write.csv(kingcountries, "kingcountries.csv")

EFIndex_withNA <- read.csv("EFIndex_withNA.csv")
save(EFIndex_withNA, file = "EFIndex_withNA.rda")
################################################################## RELIGIONS
x <- read.csv("Religious_Composition_by_Country_2010-2050.csv")
x <- subset(x, x$Year == "2010")
library(tidyverse)

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

write.csv(x, "major_religion.csv")
x <- read.csv("major_religion.csv")
x$Code <- as.character(x$Code)
x <- subset(x, x$Code != "#N/A")
write.csv(x, "major_religion.csv")
################################################################## DOMINANT ETHNIC
x <- read.csv("EPR-2019.csv")
x <- subset(x, x$from <= 2000 & x$to >= 2000)
x <-
  subset(
    x,
    x$status == "DOMINANT" |
      x$status == "JUNIOR PARTNER" |
      x$status == "MONOPOLY" | x$status == "SENIOR PARTNER"
  )
x$status <- as.character(x$status)
x$status <- replace(x$status, x$status == "MONOPOLY", 1)
x$status <- replace(x$status, x$status == "DOMINANT", 2)
x$status <- replace(x$status, x$status == "SENIOR PARTNER", 3)
x$status <- replace(x$status, x$status == "JUNIOR PARTNER", 4)
x <- x[order(x$status), ]
x <- distinct(x, statename, .keep_all = TRUE)
ethnic_dom_grp <- select(x, statename, group, size)

edg <- read.csv("edg.csv")
ethnic_dom_grp <- select(edg,-"X")

write.csv(ethnic_dom_grp,"ethnic_dom_grup.csv")

################################################################ WARS

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
################################################################ CIVIL FROM 10 MM
################################################################ ALLIANCE

ally <- read.csv("cowally.csv")
ally <- select(ally,ccode1,ccode2,year)
ally$namea <- mapvalues(ally$namea,from = levels(ally$namea),to = as.vector(r$Y))
ally$nameb <- mapvalues(ally$nameb,from = levels(ally$nameb),to = as.vector(r$Y))
ally <- subset(ally,ally$namea != "N" & ally$nameb != "N")

ally <- dplyr::mutate(ally,id = row_number())
ally <- ally[order(ally$ccode1),]

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

################################################### UN MIGRANTS ( COMMUNITY SIZE )

x <- read.csv("unmigrants.csv")
#new <- read.csv("xfac.csv")
x$Country <- mapvalues(x$Country,from = levels(x$Country),to = as.vector(new$new))

xfac <- levels(x$Country)
x <- subset(x,x$Country != "N")


y <- colnames(x)

newcols <- read.csv("col.csv")

colnames(x) <- newcols$namee

x2 <- x

x[, -2] <- mutate_all(x[,-2], as.character)
x <- as.data.frame(lapply(x, function(y) gsub(",", "", y)))
x[, -2] <- mutate_all(x[,-2], as.character)
x[, -2] <- mutate_all(x[,-2], as.numeric)

x[is.na(x)] = 0

x <- x[order(x$Country),]


mig1990 <- subset(x,x$Year == 2000)
x3 <- mig1990
x3 <- x3 %>% select(-contains("V."))
rowmatch <- as.character(sort(x3$Country))
colmatch <- colnames(x3)[-(1:3)]
colmatch <- colmatch[-4]
colmatch <- colmatch[-26]
both <- intersect(rowmatch,colmatch)
both <- c(colnames(x3)[1:3],both)
x3 <- x3[which(x3$Country %in% both), which(names(x3) %in% both)]
x3 <- select(x3,-c("Total","Year"))
nn <- as.character(x3[,1])
x3_t <- as.data.frame(t(x3[,-1]))
x3_t <- setDT(x3_t, keep.rownames = TRUE)[]
colnames(x3_t) <- c("Country",nn)
x3_t$Totals <- as.numeric(rowSums(x3_t[,-1]))
for(i in 1:nrow(x3_t)){
  x3_t[i,2:190] <- sapply(x3_t[i,2:190], FUN = function(k) k/x3_t[i,191])
}
nam <- paste("mig","2000",sep = ".")
assign(nam,x3_t)

migrants_by_year <- list(mig.1990,mig.1995,mig.2000)

save(migrants_by_year,file = "migrants_by_year.rda")


