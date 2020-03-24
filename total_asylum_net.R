#install.packages("tidyverse")
library(tidyverse)

#setwd("C:/Users/Ferran Vega/OneDrive/Math 23 C/Final project")


data2000 <- read.csv("kingdata.csv")  # kingdata has data for 2000-05.
data1995 <- read.csv("final1995.csv") # final1995 has data for 1995-00.
data1990 <- read.csv("final1990.csv") # final1990 has data for 1990-95.

data2000 <- select(data2000,-"X") # Cleaning up data. Making the dataframes compatible.
data1995 <- select(data1995,-"X")
data1990 <- select(data1990,-"X")
data1990 <- data1990[-1,]
data1990$SrcName <- droplevels(data1990$SrcName) 
data1990$TgtName <- droplevels(data1990$TgtName)
levels(data1990$TgtName) <- c(levels(data1990$TgtName),levels(data1990$SrcName)[249])
levels(data1990$TgtName) <- sort(levels(data1990$TgtName))
levels(data1990$TgtName) <- c(levels(data1990$TgtName),levels(data1990$SrcName)[279])
levels(data1990$TgtName) <- sort(levels(data1990$TgtName))


#colnames(data1995) <- colnames(data2000)


data20 <- subset(data2000,data2000$EventForm == "<HIDE>" & data2000$SrcLevel == "<CTRY>"
                 & data2000$TgtLevel == "<CTRY>" & data2000$SrcName != data2000$TgtName)
data95 <- subset(data1995,data1995$EventForm == "<HIDE>" & data1995$SrcLevel == "<CTRY>"
                 & data1995$TgtLevel == "<CTRY>" & data1995$SrcName != data1995$TgtName)
data90 <- subset(data1990,data1990$EventForm == "<HIDE>" & data1990$SrcLevel == "<CTRY>"
                 & data1990$TgtLevel == "<CTRY>" & data1990$SrcName != data1990$TgtName)


data20 <- select(data20,"EventDate", "SrcName","TgtName")
data95 <- select(data95,"EventDate", "SrcName","TgtName")
data90 <- select(data90,"EventDate", "SrcName","TgtName")

############### GRAPH WORK STARTS.

nodes20 <- as_tibble(union(levels(droplevels(data20$SrcName)),levels(droplevels(data20$TgtName))))
nodes95 <- as_tibble(union(levels(droplevels(data95$SrcName)),levels(droplevels(data95$TgtName))))
nodes90 <- as_tibble(union(levels(droplevels(data90$SrcName)),levels(droplevels(data90$TgtName))))

nodes <- rbind(nodes20,nodes95,nodes90)
nodes <- unique(nodes)

colnames(nodes) <- "label"
nodes <- nodes %>% rowid_to_column("id")

data1 <- rbind(data20,data95,data90)

attach(data1)

per_route <- data1 %>%  
  group_by(SrcName, TgtName) %>%
  summarise(weight = n()) %>% 
  ungroup()
per_route

detach(data1)

edges <- per_route %>% 
  left_join(nodes, by = c("SrcName" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("TgtName" = "label")) %>% 
  rename(to = id)
edges <- select(edges, from, to, weight)


#install.packages("igraph")
library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

deg <- degree(routes_igraph, mode="all")

V(routes_igraph)$color <- "tomato"
E(routes_igraph)$arrow.size <- .4
E(routes_igraph)$edge.color <- "gray80"
E(routes_igraph)$width <- 1+E(routes_igraph)$weight

pdf("fleegraphtotal.pdf",10,10) # Saves the plot as a pdf.
plot(routes_igraph,vertex.size = 7,vertex.label.cex = 0.5,vertex.label.color = "black")
dev.off()


############ SAME THING IS DONE FOR GRAPHING "THREATS" AND "AID".


