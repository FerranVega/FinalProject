library(dplyr)
library(lubridate)
library(Renext)
library(ggplot2)
library(EWGoF)
library(igraph)
load(file = "totalvector_generator.rda")
load("mem20.rda") #Load data from 2000-2005
load("mem95.rda") #Load data from 1995-2000
load("mem90.rda") #Load data from 1990-1995
#################### SUB-TOPIC : ANALYZING POLITICAL FLIGHT. [ PRE-ERGM ]
#################### INTRO : EXPLAIN POISSON PROCESS, EXPONENTIAL.
# When analyzing political flight to other countries, 
#we'll first need to ensure that these events are not purely random. This
#can be tested by .....
#################### EXPLAIN IDEA OF HOW NOT EXPO = GOOD FOR ERGM.

# DATASET USED : 10 mm dyadic events (1990 - 2005), Gary King.

totalvector_generator("<HIDE>","weeks") #HIDE is asylum events.
#Total political flight events.
asylum_totalvec <- totalvec
save(asylum_totalvec, file = "HIDE_vec_pre.rda")

#Analysis.....
l3 <- LK.test(asylum_totalvec,"LR",nsim = 200)
l3

#Graphs....
df <- as.data.frame(asylum_totalvec)
df$index <- c(1:length(asylum_totalvec))

x <- seq(0, 30, length.out=100)
df2 <- with(df, data.frame(x = x, y = dexp(x,l3$estimate)))

Asylumplot <- ggplot(df,aes(x=asylum_totalvec)) + 
  geom_histogram(aes(y=..density..),binwidth = 0.8,color = "black",fill = alpha("black",0.1)) +
  geom_line(data = df2, aes(x = x, y = y,color = "Exponential"), size = 1) +
  geom_density(aes(y=..density..),fill ="#FF6666",alpha = 0.4) +
  scale_x_continuous(name = "Weeks since last event") +
  scale_y_continuous(name = "Density") +
  scale_color_manual(name = "Distributions", 
                     values = c("Exponential" = "#244747")) +
  ggtitle("Asylum") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold.italic"))
Asylumplot


################# NEXT SECTION WILL USE CHI-SQ.
### ASYLUM HYPOTHESES : DIFF IN RELIGIOUS COUNTRY GROUPS.

############################### Let's graph the asylum network from 2000-2005 as an
############################### illustration.

load(file = "data20.rda")
data20 <- subset(data20,data20$EventForm == "<HIDE>")
#save(data20, file = "asylgraph_vec20.rda")
############### GRAPH WORK STARTS.

nodes20 <- as_tibble(union(levels(droplevels(data20$SrcName)),levels(droplevels(data20$TgtName))))
nodes <- unique(nodes20)

colnames(nodes) <- "label"
nodes <- nodes %>% rowid_to_column("id")

data1 <- data20

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

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

deg <- degree(routes_igraph, mode="all")

V(routes_igraph)$color <- "tomato"
E(routes_igraph)$arrow.size <- .4
E(routes_igraph)$edge.color <- "gray80"
E(routes_igraph)$width <- 1+E(routes_igraph)$weight

#save(routes_igraph,file="asylum_graph.rda")
plot(routes_igraph,vertex.size = 13,vertex.label.cex = 0.9,vertex.label.color = "black")
#Use zoom to see it better.

pdf("fleegraphFINAL.pdf",10,10) # Saves the plot as a pdf.
plot(routes_igraph,vertex.size = 13,vertex.label.cex = 0.9,vertex.label.color = "black")
dev.off()
