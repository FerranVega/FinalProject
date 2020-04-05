library(igraph)
library(dils)
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

m2 <- merge(m1,bord, by = c("namea", "nameb"))

colnames(mig1990)[1] <- "namea"
colnames(mig1990)[2] <- "nameb"

m3 <- merge(m2, mig1990, by = c("namea", "nameb"))
m3 <- m3[,-5]
colnames(m3)[5] <- "Mig"

load(file = "GDPpc.mat.rda") 
attGDPpc$country <- as.character(attGDPpc$country)

attGDPpc$country[attGDPpc$country == "_UK"] <- "UK_"
node.att.1990 <- attGDPpc[,1:2]




