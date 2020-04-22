library(tidyverse)
library(lubridate)
library(dils)
library(statnet)
library(data.table)

################################################################ ASYLUM EDGELIST.

#load("data90.rda")
#load("data95.rda")
load("data20.rda")

data20 <- subset(data20,data20$EventForm == "<HIDE>")
#data95 <- subset(data95,data95$EventForm == "<HIDE>")
#data90 <- subset(data90,data90$EventForm == "<HIDE>")

data20 <- dplyr::select(data20,"EventDate", "SrcName","TgtName")
#data95 <- dplyr::select(data95,"EventDate", "SrcName","TgtName")
#data90 <- dplyr::select(data90,"EventDate", "SrcName","TgtName")

#data90$EventDate <- mdy(data90$EventDate)
#data90 <- data90[order(data90$EventDate),]
#data90 <- subset(data90,data90$EventDate < "1992/01/01") ###################### 1990 - 1992.

data20$EventDate <- mdy_hms(data20$EventDate)
data20 <- data20[order(data20$EventDate),]
data20 <- subset(data20,data20$EventDate < "2005-01-01" & data20$EventDate >= "2004-01-01") ###################### 1990 - 1995.

#data90 <- data90[,-1]

#data90$SrcName <- as.character(data90$SrcName)
#data90$TgtName <- as.character(data90$TgtName)
#data90$counter <- c(rep(1,nrow(data90)))

data20 <- data20[,-1]

data20$SrcName <- as.character(data20$SrcName)
data20$TgtName <- as.character(data20$TgtName)
data20$counter <- c(rep(1,nrow(data20)))

################################################################ CREATING OTHER EDGELISTS

load(file = "migrants_by_year.rda")
load(file = "borders.mat.rda") 
load(file = "wars_by_year.rda") 
load(file = "ally_by_year.rda") 

ally1990 <- ally_by_year[[15]]
ally1990_1 <- ally1990
temp <- ally1990_1[,1]
ally1990_1[,1] <- ally1990_1[,2]
ally1990_1[,2] <- temp
ally1990 <- rbind(ally1990, ally1990_1) # Makes alliances edglist "complete"

war1990 <- wars_by_year[[15]] # Already "complete" edgelist
migrants1990 <- migrants_by_year[[3]] # Change every 5 years


#mig <- graph.adjacency(as.matrix(migrants1990[,2:190]))
#get.edgelist(mig)

colnames(borders.mat)[3] <- "UK_"
rownames(borders.mat)[3] <- "UK_"
borders.mat_without_NA <- as.matrix(borders.mat[-(1:2),-(1:2)])
bord <- EdgelistFromAdjacency(borders.mat_without_NA, nodelist = colnames(borders.mat_without_NA))
landlock <- borders.mat[,1]

mig1990 <- EdgelistFromAdjacency(as.matrix(migrants1990[,2:190]), nodelist = colnames(migrants1990[,2:190]))


m1 <- merge(war1990[,-4],ally1990,all = TRUE)
colnames(m1)[3] <- "Wars"
colnames(m1)[4] <- "Alliance"
m1[,3] <- ifelse(is.na(m1[,3]), 0, m1[,3])
m1[,4] <- ifelse(is.na(m1[,4]), 0, 1)

colnames(bord)[1] <- "namea"
colnames(bord)[2] <- "nameb"

m2 <- merge(m1,bord, by = c("namea", "nameb"), all = TRUE)
colnames(m2)[5] <- "border"

colnames(mig1990)[1] <- "namea"
colnames(mig1990)[2] <- "nameb"

m3 <- merge(m2, mig1990, by = c("namea", "nameb"), all = TRUE)
colnames(m3)[6] <- "Mig"

colnames(data20)[1] <- "namea"
colnames(data20)[2] <- "nameb"

m3$namea <- as.character(m3$namea)
m3$nameb <- as.character(m3$nameb)

m4 <- merge(m3, data20, by = c("namea", "nameb"), all = TRUE)
colnames(m4)[7] <- "Asylum"

m4[is.na(m4)] = 0

################################################################ CREATING NODAL ATTRIBUTES.

load(file = "GDPpc.mat.rda") 
attGDPpc$country <- as.character(attGDPpc$country)

attGDPpc$country[attGDPpc$country == "_UK"] <- "UK_"
node.att.1990 <- attGDPpc[,1:2] # Create GDP node att column

#load(file = "HDI.mat.rda") 
#attHDI$country <- as.character(attHDI$country)

#attHDI$country[attHDI$country == "_UK"] <- "UK_"
#node.att.1990 <- merge(node.att.1990, attHDI[1:2], by = "country") # Merging HDI node att


#EFI <- read.csv("EFIndex_withNA.csv") 
#EFI <- EFI[3:19]
#colnames(EFI)[1] <- "country"
#EFI$country <- as.character(EFI$country)
#node.att.1990 <- merge(node.att.1990, EFI[1:2], by = "country") # Merging ethnic frac node att
#colnames(node.att.1990)[4] <- "EFI1990"


#Religion <- read.csv("major_religion.csv", head = TRUE, sep=",") 
#Religion <- select(Religion, "Code", "major")
#colnames(Religion)[1] <- "country"
#Religion$country <- as.character(Religion$country)
#node.att.1990 <- merge(node.att.1990, Religion[1:2], by = "country") # Merging religion node att

node.att.1990 <- node.att.1990[as.character(node.att.1990$GDP1990)!= "" ,]
node.att.1990 <- na.omit(node.att.1990)


################################################################ CONSOLIDATING NODES AND EDGES.

edge.att.1990 <- filter(m4, is.element(m4$namea, node.att.1990$country) & is.element(m4$nameb, node.att.1990$country)) 
node.att.1990 <- filter(node.att.1990, is.element(node.att.1990$country, edge.att.1990$namea) & is.element(node.att.1990$country, edge.att.1990$nameb)) 

edge.att.1990 <- edge.att.1990[order(edge.att.1990$namea, edge.att.1990$nameb),]

#war_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,3)])
#alliance_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,4)])
#bord_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,5)])
#mig_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,6)])
asylum_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,7)])

#Adjlist <- list(war_adj_1990$adjacency,alliance_adj_1990$adjacency,bord_adj_1990$adjacency,mig_adj_1990$adjacency)

#save(node.att.1990,file = "Asylum_Node_Attributes_1990.rda")
#save(Adjlist,file = "Asylum_Edge_Adjacencies_1990.rda")
#save(asylum_adj_1990,file = "Asylum_Adjacency_1990.rda")

################################################################ ERGM

asylumnet <- network(asylum_adj_1990$adjacency,directed = TRUE,matrix.type = "adjacency")


network::set.vertex.attribute(asylumnet, 'Per Capita Income', as.numeric(node.att.1990$GDP1990))
#network::set.vertex.attribute(asylumnet, 'HDI', node.att.1990$HDI1990)
#network::set.vertex.attribute(asylumnet, 'EFI', node.att.1990$EFI1990)
#network::set.vertex.attribute(asylumnet, 'Religion', as.character(node.att.1990$major))
network::set.network.attribute(asylumnet,'Wars', edge.att.1990$Wars)
network::set.network.attribute(asylumnet,'Alliance', edge.att.1990$Alliance)
network::set.network.attribute(asylumnet,'Border', edge.att.1990$border)
network::set.network.attribute(asylumnet,'Migrants', edge.att.1990$Mig)


save(asylumnet, file = "asylumnet2004final.rda")





#model_2 <- ergm(asylumnet ~ edges + mutual() + nodeocov('Per Capita Income')
#                + nodeicov('Per Capita Income') + absdiff('Per Capita Income')
#                + gwidegree(1,fixed = TRUE) + gwodegree(1, fixed = TRUE)
#                + edgecov('Wars') + edgecov('Alliance')
#                + edgecov('Border') + edgecov('Migrants')
#                )
#
#summary(model_2)
