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



## First attempt, throw all vectors in.

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

plot(prop.var_2, type = "l")
points(c(1:29),prop.var_2)
abline(h=.1) # Horizonal line at 10% of the variation. Only one component with this threshold.
abline(h=.05, col="red") # Horizontal line at 5%

prop.var_2[1] # It seems like the first component, the one with the largest eigenvalue, accounts for 
# over 92 percent of the total variance. 

# Add relevant eigenvectors from P_2 to the dataframe
try2 <- as.data.frame(P_2[,1:3])
data_2 <- cbind(S_2,try2)  
colnames(data_2) <- c(colnames(A_2), "Eig1", "Eig2", "Eig3")
row.names(try2) <- c(colnames(A_2))
options(scipen=999)

try2 <- try2[order(try2$V1),]
try2 <- try2[order(try2$V2),]
try2 <- try2[order(try2$V3),]

## Third attempt: removed variables BANA (restriction on civil activity), BLAM (criticism), CALL (call for
## action), OPEN (disclose info)

PCA_data_3 <- select(PCA_data_2, "Countries", "AERI","BREL","CLAS","COLL","COMP_1", "CORP","DMOB",
                     "DWAR","ESAN","EAID", "EMAI","EXIL","GRPG","HAID","POLPER", "IMPR","MDEM","NEGO",
                     "PASS","PEXE","RAID", "RALL","SANC","SHEP","THRT_1")

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

plot(prop.var_3, type = "l")
points(c(1:25),prop.var_3)
abline(h=.1) # Horizonal line at 10% of the variation. Only one component with this threshold.
abline(h=.05, col="red") # Horizontal line at 5%

prop.var_3[1] # It seems like the first component, the one with the largest eigenvalue, accounts for 
# over 93 percent of the total variance. 

# Add relevant eigenvectors from P_3 to the dataframe
try3 <- as.data.frame(P_3[,1:3])
data_3 <- cbind(S_3,try3)  
colnames(data_3) <- c(colnames(A_3), "Eig1", "Eig2", "Eig3")
row.names(try3) <- c(colnames(A_3))
options(scipen=999)

try3 <- try3[order(try3$V1),]
try3 <- try3[order(try3$V2),]
try3 <- try3[order(try3$V3),]


## 4th attempt: use PC3 data, but convert counts to percentage of total events by column
total_counts <- colSums(PCA_data_3[,2:26]) ; total_counts
PCA_data_4 <- PCA_data_3
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

plot(prop.var_4, type = "l")
points(c(1:25),prop.var_4)
abline(h=.1) # Horizonal line at 10% of the variation. Only one component with this threshold.
abline(h=.05, col="red") # Horizontal line at 5%

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
try4 <- try4[order(try4$V2),]
try4 <- try4[order(try4$V3),]
try4 <- try4[order(try4$V4),]

## One last attempt, second PC in try4 had .99 loading on CORP. Try removing it

PCA_data_5 <- select(PCA_data_2, "Countries", "AERI","BREL","CLAS","COLL","COMP_1", "DMOB",
                     "DWAR","ESAN","EAID", "EMAI","EXIL","GRPG","HAID","POLPER", "IMPR","MDEM","NEGO",
                     "PASS","PEXE","RAID", "RALL","SANC","SHEP","THRT_1")

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

plot(prop.var_5, type = "l")
points(c(1:24),prop.var_5)
abline(h=.1) # Horizonal line at 10% of the variation. Only one component with this threshold.
abline(h=.05, col="red") # Horizontal line at 5%

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
try5 <- try5[order(try5$V2),]
try5 <- try5[order(try5$V3),]

detach(PCA_data_1)
attach(PCA_data_5)

## Create the belligerance index!

Belligerance.index <- as.data.frame(PCA_data_5$Countries)
Index <- (RAID+POLPER+AERI+CLAS+PASS+EXIL+PEXE+GRPG)/8
Belligerance.index$Index <- Index*100

# Check highest values
Belligerance.index_high <- subset(Belligerance.index,Belligerance.index$Index>1)


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
try <- events.pc$rotation

