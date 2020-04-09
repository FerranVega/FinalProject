library(igraph)
library(dils)
library(dplyr)
## CREATING EDGES for 1990

load(file = "migrants_by_year.rda")
load(file = "borders.mat.rda") 
load(file = "wars_by_year.rda") 
load(file = "ally_by_year.rda") 

ally1990 <- ally_by_year[[1]]
war1990 <- wars_by_year[[1]]
migrants1990 <- migrants_by_year[[1]]


mig <- graph.adjacency(as.matrix(migrants1990[,2:190]))
get.edgelist(mig)

colnames(borders.mat)[3] <- "UK_"
rownames(borders.mat)[3] <- "UK_"

mig1990 <- EdgelistFromAdjacency(as.matrix(migrants1990[,2:190]), nodelist = colnames(migrants1990[,2:190]))

borders.mat_without_NA <- as.matrix(borders.mat[-(1:2),-(1:2)])

bord <- EdgelistFromAdjacency(borders.mat_without_NA, nodelist = colnames(borders.mat_without_NA))

landlock <- borders.mat[,1]

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

load(file = "GDPpc.mat.rda") 
attGDPpc$country <- as.character(attGDPpc$country)

attGDPpc$country[attGDPpc$country == "_UK"] <- "UK_"
node.att.1990 <- attGDPpc[,1:2] # Create GDP node att column

load(file = "HDI.mat.rda") 
attHDI$country <- as.character(attHDI$country)

attHDI$country[attHDI$country == "_UK"] <- "UK_"
node.att.1990 <- merge(node.att.1990, attHDI[1:2], by = "country") # Merging HDI node att


EFI <- read.csv("EFIndex_withNA.csv") 
EFI <- EFI[3:19]
colnames(EFI)[1] <- "country"
EFI$country <- as.character(EFI$country)
node.att.1990 <- merge(node.att.1990, EFI[1:2], by = "country") # Merging ethnic frac node att
colnames(node.att.1990)[4] <- "EFI1990"


Religion <- read.csv("major_religion.csv", head = TRUE, sep=",") 
Religion <- select(Religion, "Code", "major")
colnames(Religion)[1] <- "country"
Religion$country <- as.character(Religion$country)
node.att.1990 <- merge(node.att.1990, Religion[1:2], by = "country") # Merging religion node att

node.att.1990 <- node.att.1990[as.character(node.att.1990$GDP1990)!= "" ,]
node.att.1990 <- na.omit(node.att.1990)

m3 <- na.omit(m3)

edge.att.1990 <- filter(m3, is.element(m3$namea, node.att.1990$country) & is.element(m3$nameb, node.att.1990$country)) 
node.att.1990 <- filter(node.att.1990, is.element(node.att.1990$country, edge.att.1990$namea) & is.element(node.att.1990$country, edge.att.1990$nameb)) 

edge.att.1990[,3] <- ifelse(is.na(edge.att.1990[,3]), 0, edge.att.1990[,3])
edge.att.1990[,4] <- ifelse(is.na(edge.att.1990[,4]), 0, edge.att.1990[,4])
edge.att.1990[,5] <- ifelse(is.na(edge.att.1990[,5]), 0, edge.att.1990[,5])

edge.att.1990[,1] <- as.character(edge.att.1990$namea)
edge.att.1990[,2] <- as.character(edge.att.1990$nameb)

edge.att.1990 <- edge.att.1990[order(edge.att.1990$namea, edge.att.1990$nameb),]

war_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,3)])
alliance_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,4)])
bord_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,5)])
mig_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,6)])


