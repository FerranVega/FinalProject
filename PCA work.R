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
library(ggplot2)

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
rownames(PC1) <- rownames(try5)

try5 <- try5[order(try5$V2),]
PC2 <- as.data.frame(try5$V2)
rownames(PC2) <- rownames(try5)

try5 <- try5[order(try5$V3),]
PC3 <- as.data.frame(try5$V3)
rownames(PC3) <- rownames(try5)

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

detach(PCA_data_1)
attach(PCA_data_5)

## Create the belligerance index!

Belligerance.index <- as.data.frame(PCA_data_5$Countries)
Index <- (RAID+POLPER+AERI+CLAS+PASS+EXIL+PEXE+GRPG)/8
Belligerance.index$Index <- Index*100
Belligerance.index <- Belligerance.index[order(-Belligerance.index$Index),]
colnames(Belligerance.index) <- c("country", "Index")

# Check highest values (plot only if index is above 1 after scaling by a factor of 100)
Belligerance.index_high <- subset(Belligerance.index,Belligerance.index$Index>1)
colnames(Belligerance.index_high) <- c("Country", "Index")
library(grid)
library(gridBase)
library(ggthemes)

p<-ggplot(data=Belligerance.index_high, aes(x=reorder(Country, -Index), y=Index)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
p1 <- p + theme_economist() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Belligerant countries", x = "Country") + geom_hline(aes(yintercept=1), col="red") 
p1 + scale_y_continuous(breaks = c(1, 5, 10,15))

## Set up regressions on belligerance index

## Load node attributes (we will use 2003 nodal attributes for this regression)
load(file = "Reg_attributes.rda") 
Reg_vars <- merge(Belligerance.index,node.att.regressions,by = "country",all = TRUE)

Reg_vars <- na.omit(Reg_vars)
#Reg_vars$Index <- Reg_vars$Index*100
Reg_vars$GDP <- log(as.numeric(Reg_vars$GDP))

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

#install.packages("stargazer") 
library(stargazer)

stargazer(reg_model, type="text",
          dep.var.labels=c("Belligerance Index"),
          covariate.labels=c("Log GDP per cap.","Human Development Index (HDI)","Civil war",
                             "Lacking border"), out="linear_model.pdf")
detach(PCA_data_5)

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

