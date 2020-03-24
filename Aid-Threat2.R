#install.packages("tidyverse")
#install.packages("igraph")
#install.packages("ndtv")
#install.packages("htmlwidgets")
#install.packages("latticeExtra")
#install.packages("Matrix")
#install.packages("dils")
library(tidyverse)
library(igraph)
library(ndtv)
library(htmlwidgets)
library(latticeExtra)
library(lubridate)
library(statnet)
library(Matrix)
library(dils)
library(multilayer.ergm)


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

################################################## STARTS HERE ..................
#load(file = "data20.rda")
#load(file = "data95.rda")
load(file = "data90.rda")

# Layer 1 is all this.....
data1forms <- c("<EHAI>")

# Layer 2 is all this.....
data2forms <- c("<FCOM>","<ICOM>","<MBLO>","<MDEM>","<MOCC>","<MTHR>","<RAID>","<RCEA>","<SANC>",
                  "<TATT>","<TBLO>","<TBOE>","<TCBR>","<THEN>","<THME>","<THRT>","<TNUC>","<TOCC>",
                  "<TRBR>","<TRSA>","<TSAN>","<TUNS>","<TWAR>","<ULTI>","<WARN>")
   
    
                                                                
data1 <- filter(data90,EventForm %in% data1forms)
data1 <- select(data1,"EventDate", "SrcName","TgtName")
data1$EventDate <- mdy(data1$EventDate)
data1 <- data1[order(data1$EventDate),]

data2 <- filter(data90,EventForm %in% data2forms)
data2 <- select(data2,"EventDate", "SrcName","TgtName")
data2$EventDate <- mdy(data2$EventDate)
data2 <- data2[order(data2$EventDate),]

#data1 <- subset(data1,data1$EventDate) # Now we have only 1990 values.
#data2 <- subset(data2,data2$EventDate)
##################################################
#attach(data1)

nodes1 <- as_tibble(union(levels(droplevels(data1$SrcName)),levels(droplevels(data1$TgtName))))
nodesb <- as_tibble(union(levels(droplevels(data2$SrcName)),levels(droplevels(data2$TgtName))))
nodes <- unique(rbind(nodes1,nodesb))                            # Not all countries are taken.

#nodes1 <- as_tibble(union(levels(data1$SrcName),levels(data1$TgtName)))
#nodesb <- as_tibble(union(levels(data2$SrcName),levels(data2$TgtName)))
#nodes <- unique(rbind(nodes1,nodesb))                            # All countries are taken.

#detach(data1)

colnames(nodes) <- "label"
nodes <- nodes %>% rowid_to_column("id")

#save(nodes,file = "allnodes90aidthreat")

attach(data1)

per_route <- data1 %>%  
  group_by(SrcName, TgtName) %>%
  summarise(weight = n()) %>% 
  ungroup()
per_route

detach(data1)

edges1 <- per_route %>% 
  left_join(nodes, by = c("SrcName" = "label")) %>% 
  rename(from = id)

edges1 <- edges1 %>% 
  left_join(nodes, by = c("TgtName" = "label")) %>% 
  rename(to = id)
edges1 <- select(edges1, from, to, weight)

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

##########################################################################

w <- expand.grid(nodes$id,nodes$id)
colnames(w) <- c("from","to")

ww <- merge(w,edges1,by = c("from","to"),all = TRUE)
ww <- merge(ww,edges2,by = c("from","to"),all = TRUE)
ww$weight.x <- replace_na(ww$weight.x,0)
ww$weight.y <- replace_na(ww$weight.y,0)

colnames(ww) <- c("ida","idb","edge1","edge2")

#save(ww,file = "ww.rda")
#save(fullnet,file = "Aid-Threat_multiplex_network.rda")
#load(file = "ww.rda")
edge1 <- select(ww,"ida","idb","edge1")
edge2 <- select(ww,"ida","idb","edge2")

aidmat <- AdjacencyFromEdgelist(edge1)
threatmat <- AdjacencyFromEdgelist(edge2)

#save(aidmat,file = "aidmatrix.rda")
#save(threatmat,file = "threatmatrix.rda")

############################################################################# MULTI-LAYER ERGM.....

aidthreatnet <- to.multiplex(aidmat$adjacency,threatmat$adjacency,output = "network")

free <- to.multiplex(matrix(1, ncol = nrow(nodes), nrow = nrow(nodes)), matrix(1, ncol = nrow(nodes), nrow = nrow(nodes)), 
                     output = "network", offzeros = TRUE)

#save(aidthreatnet,free,file = "multilayer_ergm_everythingneeded.rda")

#load(file = "multilayer_ergm_everythingneeded.rda")

mod.within <- ergm(aidthreatnet
                   ~ edges_layer(layer = 1) + edges_layer(layer = 2)
                   + mutual("layer.mem", diff = T),
                   control = control.ergm(seed = 206424),
                   constraints = ~fixallbut(free))

summary(mod.within) 

mod.cross <-  ergm(aidthreatnet
                   ~ edges_layer(layer = 1) + edges_layer(layer = 2)
                   + mutual("layer.mem", diff = T)
                   + duplexdyad(c("e", "f"), layers = list(1, 2)),
                   control = control.ergm(seed = 206424),
                   constraints = ~fixallbut(free))

summary(mod.cross) 

#### OBSERVATIONS
# For EEAI and conflictvars, there is reciprocity(weird) between Aid & Threats.
# For EHAI and conflictvars, there is reinforcement between Aid & Threats(again,weird). Need to check dataset.


