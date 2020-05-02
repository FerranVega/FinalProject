library(dplyr)
library(lubridate)
library(Renext)
library(ggplot2)
library(EWGoF)

#################### SUB-TOPIC : ANALYZING POLITICAL FLIGHT. [ PRE-ERGM ]
#################### INTRO : EXPLAIN POISSON PROCESS, EXPONENTIAL.
# When analyzing political flight to other countries, 
#we'll first need to ensure that these events are not purely random. This
#can be tested by .....
#################### EXPLAIN IDEA OF HOW NOT EXPO = GOOD FOR ERGM.

# DATASET USED : 10 mm dyadic events (1990 - 2005), Gary King.

load("mem20.rda") #Load data from 2000-2005
load("mem95.rda") #Load data from 1995-2000
load("mem90.rda") #Load data from 1990-1995

eventtype <- "<HIDE>" #Choose only political flight events.
timeframe <- "weeks"

#2000-2005
data <- subset(data20,data20$EventForm == eventtype)
data <- subset(data, data$SrcName != data$TgtName)
data <- subset(data, data$SrcLevel == "<CTRY>" & data$TgtLevel == "<CTRY>")

data <- dplyr::select(data,"EventDate","SrcName","TgtName")
data$EventDate <- mdy_hms(data$EventDate)
data <- data[order(data$EventDate),]
data$SrcName <- as.character(data$SrcName)
data$TgtName <- as.character(data$TgtName)

attach(data)

vec1 <- c()
for(i in 1:(nrow(data)-1)){
  if(SrcName[i] != SrcName[i+1] & SrcName[i] != TgtName[i+1]
     & TgtName[i] != TgtName[i+1] & TgtName[i] != SrcName[i+1]){
    j <- difftime(strptime(data$EventDate[i+1], format = "%Y-%m-%d"),
                  strptime(data$EventDate[i], format = "%Y-%m-%d"),units=timeframe)
    vec1 <- c(vec1,j)
  }
}
detach(data)

vec1 <- replace(vec1,which(vec1==0),0.00000001)

#1995-2000
data <- subset(data95,data95$EventForm == eventtype)
data <- subset(data, data$SrcName != data$TgtName)
data <- subset(data, data$SrcLevel == "<CTRY>" & data$TgtLevel == "<CTRY>")

data <- dplyr::select(data,"EventDate","SrcName","TgtName")
data$EventDate <- ymd(data$EventDate)
data <- data[order(data$EventDate),]
data$SrcName <- as.character(data$SrcName)
data$TgtName <- as.character(data$TgtName)

attach(data)

vec95 <- c()
for(i in 1:(nrow(data)-1)){
  if(SrcName[i] != SrcName[i+1] & SrcName[i] != TgtName[i+1]
     & TgtName[i] != TgtName[i+1] & TgtName[i] != SrcName[i+1]){
    j <- difftime(strptime(data$EventDate[i+1], format = "%Y-%m-%d"),
                  strptime(data$EventDate[i], format = "%Y-%m-%d"),units=timeframe)
    vec95 <- c(vec95,j)
  }
}
detach(data)

vec95 <- vec95[!is.na(vec95)]
vec95 <- replace(vec95,which(vec95==0),0.00000001)

#1990-1995
data <- subset(data90,data90$EventForm == eventtype)
data$SrcName <- as.character(data$SrcName)
data$TgtName <- as.character(data$TgtName)
data <- subset(data, data$SrcName != data$TgtName)
data <- subset(data, data$SrcLevel == "<CTRY>" & data$TgtLevel == "<CTRY>")

data <- dplyr::select(data,"EventDate","SrcName","TgtName")
data$EventDate <- mdy(data$EventDate)
data <- data[order(data$EventDate),]

attach(data)

vec90 <- c()
for(i in 1:(nrow(data)-1)){
  if(SrcName[i] != SrcName[i+1] & SrcName[i] != TgtName[i+1]
     & TgtName[i] != TgtName[i+1] & TgtName[i] != SrcName[i+1]){
    j <- difftime(strptime(data$EventDate[i+1], format = "%Y-%m-%d"),
                  strptime(data$EventDate[i], format = "%Y-%m-%d"),units=timeframe)
    vec90 <- c(vec90,j)
  }
}
detach(data)

vec90 <- replace(vec90,which(vec90==0),0.00000001)

#Total political flight events.
asylum_totalvec <- c(vec1,vec95,vec90)


#Analysis.....
l3 <- LK.test(asylum_totalvec,"LR",nsim = 200)
l3

#Graphs....
hist(asylum_totalvec, probability = TRUE)
curve(dexp(x,l3$estimate),add = TRUE, type = "l")


################# ADD CHI SQ. GOF TEST TO AID AND THREAT TOTALVEC.
################# USE THE FUNCTION INSTEAD OF WHOLE CODE....MAKE IT IN AID-THREAT TOPIC.
################# IMPROVE GRAPHS USING GGPLOT2. ADD QQ OR PP.
################# NEXT SECTION WILL USE CHI-SQ. AND PERM TESTS ON ASYLUM HYPOTHESES.


