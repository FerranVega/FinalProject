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

m1 <- merge(war1990[,-3],ally1990,all = TRUE)

m2 <- merge(m1,bord,all = TRUE)





