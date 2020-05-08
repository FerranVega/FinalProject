library(dplyr)
library(lubridate)
library(Renext)
library(ggplot2)
library(EWGoF)
library(gridExtra) 
library(igraph)
library(tibble)

#################### SUB-TOPIC : DOES AID BUY SOFTPOWER? [ PRE-ERGM ]
#################### INTRO : EXPLAIN POISSON PROCESS, EXPONENTIAL.
# When analyzing the relationship between economic aid grants and the softpower it
#provides, we'll first need to ensure that these events are not purely random. This
#can be tested by .....
#################### EXPLAIN IDEA OF HOW NOT EXPO = GOOD FOR ERGM.

# DATASET USED : 10 mm dyadic events (1990 - 2005), Gary King.

load("mem20.rda") #Load data from 2000-2005
load("mem95.rda") #Load data from 1995-2000
load("mem90.rda") #Load data from 1990-1995

totalvector_generator <- function(eventtype, timeframe){
  #2000-2005
  data <- subset(data20, data20$EventForm == eventtype)
  data <- subset(data, data$SrcName != data$TgtName)
  data <- subset(data, data$SrcLevel == "<CTRY>" & data$TgtLevel == "<CTRY>")
  data <- dplyr::select(data, "EventDate", "SrcName", "TgtName")
  data$EventDate <- mdy_hms(data$EventDate)
  data <- data[order(data$EventDate),]
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  
  attach(data)
  
  vec1 <- c()
  for (i in 1:(nrow(data) - 1)) {
    if (SrcName[i] != SrcName[i + 1] & SrcName[i] != TgtName[i + 1]
        &
        TgtName[i] != TgtName[i + 1] & TgtName[i] != SrcName[i + 1]) {
      j <- difftime(
        strptime(data$EventDate[i + 1], format = "%Y-%m-%d"),
        strptime(data$EventDate[i], format = "%Y-%m-%d"),
        units = timeframe
      )
      vec1 <- c(vec1, j)
    }
  }
  detach(data)
  
  vec1 <- replace(vec1, which(vec1 == 0), 0.00000001)
  
  #1995-2000
  data <- subset(data95, data95$EventForm == eventtype)
  data <- subset(data, data$SrcName != data$TgtName)
  data <-
    subset(data, data$SrcLevel == "<CTRY>" &
             data$TgtLevel == "<CTRY>")
  
  data <- dplyr::select(data, "EventDate", "SrcName", "TgtName")
  data$EventDate <- ymd(data$EventDate)
  data <- data[order(data$EventDate),]
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  
  attach(data)
  
  vec95 <- c()
  for (i in 1:(nrow(data) - 1)) {
    if (SrcName[i] != SrcName[i + 1] & SrcName[i] != TgtName[i + 1]
        &
        TgtName[i] != TgtName[i + 1] & TgtName[i] != SrcName[i + 1]) {
      j <- difftime(
        strptime(data$EventDate[i + 1], format = "%Y-%m-%d"),
        strptime(data$EventDate[i], format = "%Y-%m-%d"),
        units = timeframe
      )
      vec95 <- c(vec95, j)
    }
  }
  detach(data)
  
  vec95 <- vec95[!is.na(vec95)]
  vec95 <- replace(vec95, which(vec95 == 0), 0.00000001)
  
  #1990-1995
  data <- subset(data90, data90$EventForm == eventtype)
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  data <- subset(data, data$SrcName != data$TgtName)
  data <-
    subset(data, data$SrcLevel == "<CTRY>" &
             data$TgtLevel == "<CTRY>")
  
  data <- dplyr::select(data, "EventDate", "SrcName", "TgtName")
  data$EventDate <- mdy(data$EventDate)
  data <- data[order(data$EventDate),]
  
  attach(data)
  
  vec90 <- c()
  for (i in 1:(nrow(data) - 1)) {
    if (SrcName[i] != SrcName[i + 1] & SrcName[i] != TgtName[i + 1]
        &
        TgtName[i] != TgtName[i + 1] & TgtName[i] != SrcName[i + 1]) {
      j <- difftime(
        strptime(data$EventDate[i + 1], format = "%Y-%m-%d"),
        strptime(data$EventDate[i], format = "%Y-%m-%d"),
        units = timeframe
      )
      vec90 <- c(vec90, j)
    }
  }
  detach(data)
  
  vec90 <- replace(vec90, which(vec90 == 0), 0.00000001)
  totalvec <<- c(vec1, vec95, vec90)
}
#save(totalvector_generator, file = "totalvector_generator.rda")
#### Let's start with economic aid.
#Total for Aid
totalvector_generator("<EEAI>","weeks")
aid_totalvec <- totalvec
save(aid_totalvec, file = "EEAI_vec_pre.rda")
###############################################################

#Total for Threats.
totalvector_generator("<THRT>","weeks")
threat_totalvec <- totalvec
save(threat_totalvec, file = "THRT_vec_pre.rda")
#Analysis.....
l1 <- LK.test(aid_totalvec, "LR", nsim = 200)
l2 <- LK.test(threat_totalvec, "LR", nsim = 200)

#Graphs....
df <- as.data.frame(aid_totalvec)
df$index <- c(1:length(aid_totalvec))

x <- seq(0, 30, length.out=100)
df2 <- with(df, data.frame(x = x, y = dexp(x,l1$estimate)))

Aidplot <- ggplot(df,aes(x=aid_totalvec)) + 
  geom_histogram(aes(y=..density..),binwidth = 0.8,color = "black",fill = alpha("black",0.1)) +
  geom_line(data = df2, aes(x = x, y = y,color = "Exponential"), size = 1) +
  geom_density(aes(y=..density..),fill ="#FF6666",alpha = 0.4) +
  ylim(c(0,0.5)) +
  xlim(c(0,10)) +
  scale_x_continuous(name = "Weeks since last event") +
  scale_y_continuous(name = "Density") +
  scale_color_manual(name = "Distributions", 
                     values = c("Exponential" = "#244747")) +
  ggtitle("Economic Aid") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold.italic"))


df <- as.data.frame(threat_totalvec)
df$index <- c(1:length(threat_totalvec))

x <- seq(0, 30, length.out=100)
df2 <- with(df, data.frame(x = x, y = dexp(x,l1$estimate)))

Threatplot <- ggplot(df,aes(x=threat_totalvec)) + 
  geom_histogram(aes(y=..density..),binwidth = 0.8,color = "black",fill = alpha("black",0.1)) +
  geom_line(data = df2, aes(x = x, y = y,color = "Exponential"), size = 1) +
  geom_density(aes(y=..density..),fill ="#FF6666",alpha = 0.4) +
  ylim(c(0,0.5)) +
  xlim(c(0,70)) +
  scale_x_continuous(name = "Weeks since last event") +
  scale_y_continuous(name = "Density") +
  scale_color_manual(name = "Distributions", 
                     values = c("Exponential" = "#244747")) +
  ggtitle("Threat") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold.italic"))


grid.arrange(Aidplot,Threatplot, ncol = 2)
################# NEXT SECTION WILL USE CHI-SQ.
### AID-THREAT HYPOTHESES : DIFF IN HDI


############################### Let's graph the aid&threat network for 1994 as an
############################### illustration.

load(file = "data90.rda")
data90 <- subset(data90,data90$EventForm == "<EEAI>" | data90$EventForm == "<THRT>")
data90$EventDate <- mdy(data90$EventDate)
data90 <- subset(data90, data90$EventDate < "1994-01-01" & data90$EventDate >= "1993-01-01")
save(data90, file = "atgraph_data90.rda")
############### GRAPH WORK STARTS.

nodes90 <- as_tibble(union(levels(droplevels(data90$SrcName)),levels(droplevels(data90$TgtName))))
nodes <- unique(nodes90)

colnames(nodes) <- "label"
nodes <- nodes %>% rowid_to_column("id")

data1 <- select(data90,"SrcName","TgtName","EventForm")

attach(data1)

per_route <- data1 %>%  
  group_by(SrcName, TgtName, EventForm) %>%
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
edges <- select(edges, from, to, weight, EventForm)
edges$EventForm <- droplevels(edges$EventForm)

levels(edges$EventForm) <- c(1,2)
edges$EventForm <- as.numeric(edges$EventForm)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
set_edge_attr(routes_igraph,"EventForm",value = edges$EventForm)
deg <- degree(routes_igraph, mode="all")

V(routes_igraph)$color <- "tomato"
E(routes_igraph)$arrow.size <- .4
colorss <- c("green","red")
E(routes_igraph)$color <- colorss[E(routes_igraph)$EventForm]
save(routes_igraph,file="at_graph.rda")
plot(routes_igraph,vertex.size = 10,vertex.label.cex = 0.7,vertex.label.color = "black",
     main = "Aid and Threat Network for 1993")
legend("topleft",c("Economic Aid","Threat"),fill = colorss)
#Use zoom to see it better.

'pdf("atgraphFINAL2.pdf",10,10) # Saves the plot as a pdf.
plot(routes_igraph,vertex.size = 10,vertex.label.cex = 0.7,vertex.label.color = "black",
     main = "Aid and Threat Network for 1994")
legend("topleft",c("Economic Aid","Threat"),fill = colorss)
dev.off()'
