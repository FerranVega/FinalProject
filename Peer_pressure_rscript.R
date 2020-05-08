library(dplyr)
library(lubridate)
library(Renext)
library(ggplot2)
library(EWGoF)

#################### SUB-TOPIC : PEER INFLUENCE IN INTERNATIONAL RELATIONS.
#################### INTRO : EXPLAIN POISSON PROCESS, EXPONENTIAL AND WEIBULL.
# Explain props of weibull. Make these functions. Derive formulae for pdf,cdf,survival,
# hazard. Differentiate hazard to explain point of weibull v. expo.

#Weibull_pdf formula
weibull_pdf <- function(t,eta,beta){
  beta*(t)^(beta-1)*exp(-1*(t/eta)^beta)/(eta^beta)
}

#Just integrating and vectorizing. Actual formula in Latex will be....1-e^-(t/eta)^beta.
weibull_cdf <- Vectorize(function(p,eta,beta){
  integrate(weibull_pdf,lower=10^(-100),upper=p,eta=eta,beta=beta)$value
})

#Hazard rate will be pdf/survival.....do algebra in Latex and diff to prove how beta affects
#the hazard rate.
hazardrate <- Vectorize(function(x,eta,beta){
  weibull_pdf(x,eta,beta)/(1-weibull_cdf(x,eta,beta))
})
#Let's verify....
curve(hazardrate(x,1,0.9)) # Weibull with beta < 1 has decreasing hazard rate.
curve(hazardrate(x,1,1)) # Weibull with beta = 1 has constant hazard rate.
curve(hazardrate(x,1,1.5)) # Weibull with beta > 1 has increasing hazard rate.

####################         EXPLAIN IDEA OF HOW WEIBULL = PEER INFLUENCE.

########## A. PEER INFLUENCE IN ARMED ASSISSTANCE REQUESTS.
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
#How to print all of it at once?
#Note on chi-sq. unsuitability.

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

########## B. NO PEER INFLUENCE IN GIVING ULTIMATUMS.
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


#DISCUSS ECONOMIC/POLITICAL MOTIVATIONS FOR RESULTS OBSERVED. INCLUDE CAVEATS.









