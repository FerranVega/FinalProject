#install.packages("tidyverse")
library(tidyverse)
#install.packages("igraph")
library(igraph)
#install.packages("ndtv")
#install.packages("htmlwidgets")
#install.packages("latticeExtra")
library(ndtv)
library(htmlwidgets)
library(latticeExtra)
library(lubridate)
library(statnet)



#data2000 <- read.csv("kingdata.csv") # kingdata has data for 2000-05.
#data1995 <- read.csv("final1995.csv") # final1995 has data for 1995-00.
#data1990 <- read.csv("final1990.csv") # final1990 has data for 1990-95.
#nodes90 <- levels(data1990$SrcName)
#nodes90 <- subset(nodes90, nodes90 != "UN")
#save(nodes90,file="nodes90.rda")

#data2000 <- select(data2000,-"X") # Cleaning up data. Making the dataframes compatible.
#data1995 <- select(data1995,-"X")
#data1990 <- select(data1990,-"X")
#data1990 <- data1990[-1,]
#data1990$SrcName <- droplevels(data1990$SrcName) 
#data1990$TgtName <- droplevels(data1990$TgtName)
#levels(data1990$TgtName) <- c(levels(data1990$TgtName),levels(data1990$SrcName)[249])
#levels(data1990$TgtName) <- sort(levels(data1990$TgtName))
#levels(data1990$TgtName) <- c(levels(data1990$TgtName),levels(data1990$SrcName)[279])
#levels(data1990$TgtName) <- sort(levels(data1990$TgtName))


#colnames(data1995) <- colnames(data2000)


#data20 <- subset(data2000,data2000$SrcLevel == "<CTRY>" & data2000$TgtLevel == "<CTRY>" & data2000$SrcName != data2000$TgtName)
#data95 <- subset(data1995,data1995$SrcLevel == "<CTRY>" & data1995$TgtLevel == "<CTRY>" & data1995$SrcName != data1995$TgtName)
#data90 <- subset(data1990,data1990$SrcLevel == "<CTRY>" & data1990$TgtLevel == "<CTRY>" & data1990$SrcName != data1990$TgtName)


#data20 <- select(data20,"EventDate", "SrcName","TgtName")
#data95 <- select(data95,"EventDate", "SrcName","TgtName")
#data90 <- select(data90,"EventDate", "SrcName","TgtName")

#save(data20,file = "data20.rda")
#save(data95,file = "data95.rda")
#save(data90,file = "data90.rda")

#load(file = "data20.rda")
#load(file = "data95.rda")
load(file = "data90.rda")
#load(file = "nodes90.rda")
##################################################

data1 <- subset(data90,data90$EventForm == "<EEAI>" & data90$TgtName != "UN" & data90$SrcName != "UN")
data1 <- select(data1,"EventDate", "SrcName","TgtName")
data1$EventDate <- mdy(data1$EventDate)
data1 <- data1[order(data1$EventDate),]


conflictvars <- c("<FCOM>","<ICOM>","<MBLO>","<MDEM>","<MOCC>","<MTHR>","<RAID>","<RCEA>","<SANC>",
                  "<TATT>","<TBLO>","<TBOE>","<TCBR>","<THEN>","<THME>","<THRT>","<TNUC>","<TOCC>",
                  "<TRBR>","<TRSA>","<TSAN>","<TUNS>","<TWAR>","<ULTI>","<WARN>")

data2 <- filter(data90,EventForm %in% conflictvars)
data2 <- select(data2,"EventDate", "SrcName","TgtName")
data2$EventDate <- mdy(data2$EventDate)
data2 <- data2[order(data2$EventDate),]

#data1 <- subset(data1,data1$EventDate > "1992-01-01" & data1$EventDate < "1993-01-01") # Now we have only 1990 values.
#data2 <- subset(data2,data2$EventDate > "1992-01-01" & data2$EventDate < "1993-01-02")
##################################################
#attach(data1)

nodes1 <- as_tibble(union(levels(droplevels(data1$SrcName)),levels(droplevels(data1$TgtName))))
nodesb <- as_tibble(union(levels(droplevels(data2$SrcName)),levels(droplevels(data2$TgtName))))
nodes <- unique(rbind(nodes1,nodesb))

#nodes <- as.tibble(nodes90)

#detach(data1)

colnames(nodes) <- "label"
nodes <- nodes %>% rowid_to_column("id")

attach(data2)

per_route <- data2 %>%  
  group_by(SrcName, TgtName) %>%
  summarise(weight = n()) %>% 
  ungroup()
per_route

detach(data2)

edges2 <- per_route %>% 
  left_join(nodes, by = c("SrcName" = "label")) %>% 
  rename(from = id)

edges2 <- edges2 %>% 
  left_join(nodes, by = c("TgtName" = "label")) %>% 
  rename(to = id)
edges2 <- select(edges2, from, to, weight)

edgesboth <- edges2

#edgesboth$type <- c(rep(0,nrow(edges1)),rep(1,nrow(edges2)))
#edgesboth$type <- factor(edgesboth$type)

bothgraph <- graph_from_data_frame(d = edgesboth, vertices = nodes, directed = TRUE)

deg <- igraph::degree(bothgraph, mode="all")
colss <- c("Green","Red")
set_edge_attr(bothgraph,"type",value = edgesboth$type)

V(bothgraph)$color <- "tomato"
E(bothgraph)$arrow.size <- .3
E(bothgraph)$color <- colss[(E(bothgraph)$type)+1]
E(bothgraph)$width <- 1+E(bothgraph)$weight/12

pdf("bothgraph2000yr.pdf",10,10)
plot(bothgraph,vertex.size = 10,vertex.label.cex = 0.7,vertex.label.color = "black")
dev.off()

fullnet <- intergraph::asNetwork(bothgraph)
threatnet <- fullnet
#save(fullnet,file = "fullnet.rda")
#save(threatnet,file = "threatnet.rda")

##########################################################################################

#load(file = "fullnet.rda")
#set.edge.attribute(fullnet,"ThreatorAid",c(rep("Aid",46),rep("Threat",15)))
#summary(fullnet)
#em1 <- ergm(fullnet ~ edges)
#summary(em1)
#load(file = "aidmat1990")
#load(file = "threatnet.rda")

##########################################################################################

load(file = "Aid-Threat_multiplex_network.rda")
summary(fullnet)

library(multilayer.ergm)
#library(btergm)
#install.packages("multinet")
library(multinet)
#######################

help("chemg_data")


















