rm(list=ls())
setwd("C:/Users/Ferran Vega/OneDrive/Math 23 C/Final Project/FinalProject")

library(dils)
library(dplyr)

# Prepare data for PCA

## Load data and create counts for each action type

load("data90.rda")
load("data95.rda")
load("data20.rda")

data <- rbind(data90, data95, data20) # Bind together
data %>% select(SrcName, EventForm) -> data # Select "actors" and "actions" columns

Actions <- data %>%  
  group_by(SrcName, EventForm) %>%
  summarise(weight = n()) %>% 
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

'Load the matrix and trim the number of columns to include in PCA

 Note that before trimming, I merge some vectors that, for our purposes
 would capture similar types of behavior
'
rm(list=ls())
setwd("C:/Users/Ferran Vega/OneDrive/Math 23 C/Final Project/FinalProject")
library(dils)
library(dplyr)
library(stats)

##########################
load(file = "PCA_data.rda")
#########################

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

PCA_data_1 <- select(PCA_data_1, "Countries", "ADIS_1", "AERI", "APOL", "ATNE", "ASSA", "BANA", "BEAT",
                     "BLAM", "BLAW", "BREL", "BVIO", "CALL", "BIOA", "CENS", "CLAS", "COLL", "COMP_1", "CORP",
                     "COUP", "DEFY", "DMOB", "DWAR", "ESAN", "EAID", "EMAI", "EXIL", "FORG", "GASY", "GRPG",
                     "HAID", "POLPER", "HTAK", "IMPR", "MREADY", "MDEM", "MOCC", "MONI", "NEGO", "NUCA", "OPEN",
                     "PASS", "PEXE", "RAID", "RALL", "RCEA", "UNREST", "RWCF", "SANC", "SEZR", "SHEP", "THRT_1",
                     "TWAR")

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

plot(prop.var, type = "l")
points(c(1:52),prop.var)
abline(h=.1) # Horizonal line at 10% of the variation. Only one component with this threshold.

prop.var[1] # It seems like the first component, the one with the largest eigenvalue, accounts for 
# over 92 percent of the total variance. 
  
# Add relevant eigenvectors from P to the dataframe
data <- cbind(S,P[,1])  
colnames(data) <- c(colnames(A), "Eig1")
options(scipen=999)
as.matrix(signif(sort(data[,53])), digits=2)

detach(PCA_data_1)

####################################################################################


## Done with prcomp
events.pc <- prcomp(~ADIS_1 + AERI+APOL+ATNE+ASSA+BANA+BEAT+
        BLAM+BLAW+BREL+BVIO+CALL+BIOA+CENS+CLAS+COLL+COMP_1+CORP+
        COUP+DEFY+DMOB+DWAR+ESAN+EAID+EMAI+EXIL+FORG+GASY+GRPG+
        HAID+POLPER+HTAK+IMPR+MREADY+MDEM+MOCC+MONI+NEGO+NUCA+OPEN+
        PASS+PEXE+RAID+RALL+RCEA+UNREST+RWCF+SANC+SEZR+SHEP+THRT_1+TWAR, 
        data=PCA_data_1, center=T, scale=T, retx=T)

#Screeplot
screeplot(events.pc, type = "l")
abline(h=10)
summary(events.pc)
events.pc$rotation

