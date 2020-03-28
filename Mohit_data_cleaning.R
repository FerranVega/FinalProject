library(tidyverse)
################################################################# EFINDEX
x <- subset(x,x$Year >= 1990 & x$Year <= 2005)
x$last <- paste(as.character(x$Country),as.character(x$Year))
x <- unique(x)
x <- select(x,-"last")
x <- spread(x,Year,EFindex)
x <- na.omit(x)
write.csv(x,"EFIndex_withoutNA.csv")
kingcountries <- x
write.csv(kingcountries,"kingcountries.csv")

EFIndex_withNA <- read.csv("EFIndex_withNA.csv")
save(EFIndex_withNA,file = "EFIndex_withNA.rda")
################################################################## RELIGIONS
x <- read.csv("Religious_Composition_by_Country_2010-2050.csv")
x <-subset(x,x$Year == "2010")
library(tidyverse)

x <- x[,-(1:5)]
x <- x[-(1:7),]
x <- x[,-2]
x <- mutate_all(x,as.character)
for(i in 1:nrow(x)){
  if(x[i,2] == "< 1.0"){x[i,2] <- "0.1"}
  if(x[i,2] == ">99.0"){x[i,2] <- "99"}
  if(x[i,3] == "< 1.0"){x[i,3] <- "0.1"}
  if(x[i,3] == ">99.0"){x[i,3] <- "99"}
  if(x[i,4] == "< 1.0"){x[i,4] <- "0.1"}
  if(x[i,4] == ">99.0"){x[i,4] <- "99"}
  if(x[i,5] == "< 1.0"){x[i,5] <- "0.1"}
  if(x[i,5] == ">99.0"){x[i,5] <- "99"}
  if(x[i,6] == "< 1.0"){x[i,6] <- "0.1"}
  if(x[i,6] == ">99.0"){x[i,6] <- "99"}
  if(x[i,7] == "< 1.0"){x[i,7] <- "0.1"}
  if(x[i,7] == ">99.0"){x[i,7] <- "99"}
  if(x[i,8] == "< 1.0"){x[i,8] <- "0.1"}
  if(x[i,8] == ">99.0"){x[i,8] <- "99"}
  if(x[i,9] == "< 1.0"){x[i,9] <- "0.1"}
  if(x[i,9] == ">99.0"){x[i,9] <- "99"}
}
x[,-1] <- mutate_all(x[,-1],as.numeric)
x$major <- colnames(x[,-1])[max.col(x[,-1],ties.method="first")]

write.csv(x,"major_religion.csv")
x <- read.csv("major_religion.csv")
x$Code <- as.character(x$Code)
x <- subset(x,x$Code != "#N/A")
write.csv(x,"major_religion.csv")
################################################################## DOMINANT ETHNIC
x <- read.csv("EPR-2019.csv")
x <- subset(x, x$from <= 1990 & x$to >= 1990)


