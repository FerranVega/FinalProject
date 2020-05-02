library(dplyr)
library(lubridate)
library(Renext)
library(ggplot2)
library(EWGoF)

#################### SUB-TOPIC : PEER PRESSURE IN INTERNATIONAL RELATIONS.
#################### INTRO : EXPLAIN POISSON PROCESS, EXPONENTIAL AND WEIBULL.
####################         EXPLAIN IDEA OF HOW WEIBULL = PEER PRESSURE.

########## A. PEER PRESSURE IN ARMED ASSISSTANCE REQUESTS.
# DATASET USED : 10 mm dyadic events (1990 - 1995), Gary King.

load("mem90.rda") #Load the raw data.
eventtype <- "<ASKI>" #Choose the event <ASKI> (Ask for armed assisstance).
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
#How to print all of it at once?
#Note on chi-sq. unsuitability.

#Graph it.
hist(vec90, probability = TRUE)
curve(dweibull(x,w1$estimate[2],w1$estimate[1]),add = TRUE, type = "l", col = "red")
curve(dexp(x,l1$estimate),add = TRUE, type = "l", col = "blue" )

#The weibull fits excellently (pval = 1) and estimates a shape parameter != 1.
#This gives sufficient evidence that armed assisstance requests are NOT poisson distributed.
#The weibull shape parameter is 0.769. This means the distribution of time between events
#exhibits the property of "infant mortality". Probability of event happening immediately or
#soon after a preceding event is high, which falls as time passes since the last event.

########## B. NO PEER PRESSURE IN GIVING ULTIMATUMS.
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
#How to print all of it at once?
#Note on chi-sq. unsuitability.

#Graph it.
hist(vec90, probability = TRUE)
curve(dweibull(x,w1$estimate[2],w1$estimate[1]),add = TRUE, type = "l", col = "red")
curve(dexp(x,l1$estimate),add = TRUE, type = "l", col = "blue" )

#The weibull fits excellently (pval > 0.95) and estimates a shape parameter ~ 1.
#This gives strong evidence that giving ultimatums might be poisson distributed.
#The weibull shape parameter is 1.02. This means we can see the property of memorylessness.

#DISCUSS ECONOMIC/POLITICAL MOTIVATIONS FOR RESULTS OBSERVED. INCLUDE CAVEATS.
#IMPROVE GRAPHS USING GGPLOT2. ADD QQ OR PP.


