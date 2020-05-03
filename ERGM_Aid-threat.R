library(tidyverse)
library(lubridate)
library(dils)
library(statnet)
library(btergm)
library(plyr)
library(beepr)

load(file = "migrants_by_year.rda")
load(file = "borders.mat.rda") 
load(file = "wars_by_year.rda") 
load(file = "ally_by_year.rda")
load(file = "data90.rda")
load(file = "data95.rda") 
load(file = "data20.rda")

for (i in 1:15){
  if(i > 5){
    if(i < 11){
      data <- data95
      aid <- ea95s
    } else { aid <- ea20s
           data <- data20 }
  } else { aid <- ea90s 
         data <- data90 }
  
  data <- subset(data,data$EventForm == "<TATT>")
  data <- dplyr::select(data,"EventDate", "SrcName","TgtName")
  aid <- dplyr::select(aid,"EventDate","TgtName","SrcName")
  
  if(i > 5){
    if(i < 11){
      data$EventDate <- ymd(data$EventDate)
      aid$EventDate <- ymd(aid$EventDate)
    } else {data$EventDate <- mdy_hms(data$EventDate)
           aid$EventDate <- mdy_hms(aid$EventDate)}
  } else {data$EventDate <- mdy(data$EventDate)
         aid$EventDate <- mdy(aid$EventDate)}
  
  data <- data[order(data$EventDate),]
  aid <- aid[order(aid$EventDate),]
  
  j <- i + 1989
  k <- i + 1990
  startdate <- paste(j,"/01/01",sep = "")
  enddate <- paste(k,"/01/01",sep = "")
  data <- subset(data,data$EventDate < enddate & data$EventDate >= startdate) ###################### 1990 - 1995.
  aid  <- subset(aid,  aid$EventDate < enddate & aid$EventDate  >= startdate)
  
  data <- data[,-1]
  aid <- aid[,-1]
  
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  data$counter <- c(rep(1,nrow(data)))
  
  aid$SrcName <- as.character(aid$SrcName)
  aid$TgtName <- as.character(aid$TgtName)
  aid$counter <- c(rep(1,nrow(aid)))
  
  aid <- ddply(aid,.(TgtName,SrcName),nrow)
  
  ally1990 <- ally_by_year[[i]]
  war1990 <- wars_by_year[[i]]
  
  ally1990_1 <- ally1990
  temp <- ally1990_1[,1]
  ally1990_1[,1] <- ally1990_1[,2]
  ally1990_1[,2] <- temp
  ally1990 <- rbind(ally1990, ally1990_1) # Makes alliances edglist "complete"
  
  colnames(borders.mat)[3] <- "UK_"
  rownames(borders.mat)[3] <- "UK_"
  
  borders.mat_without_NA <- as.matrix(borders.mat[-(1:2),-(1:2)])
  
  bord <- EdgelistFromAdjacency(borders.mat_without_NA, nodelist = colnames(borders.mat_without_NA))
  
  landlock <- borders.mat[,1]
  
  m1 <- merge(war1990[,-4],ally1990,by = c("namea","nameb"),all = TRUE)
  colnames(m1)[3] <- "Wars"
  colnames(m1)[4] <- "Alliance"
  m1[,3] <- ifelse(is.na(m1[,3]), 0, m1[,3])
  m1[,4] <- ifelse(is.na(m1[,4]), 0, 1)
  
  colnames(bord)[1] <- "namea"
  colnames(bord)[2] <- "nameb"
  
  m1 <- merge(m1,bord, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[5] <- "border"
  
  colnames(aid)[1] <- "namea"
  colnames(aid)[2] <- "nameb"
  
  m1 <- merge(m1, aid, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[6] <- "Aid"
  
  colnames(data)[1] <- "namea"
  colnames(data)[2] <- "nameb"
  
  m1 <- merge(m1, data, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[ncol(m1)] <- "Threat"
  
  m1[is.na(m1)] = 0
  
  load(file = "GDPpc.mat.rda") 
  attGDPpc$country <- as.character(attGDPpc$country)
  
  attGDPpc$country[attGDPpc$country == "_UK"] <- "UK_"
  node.att.1990 <- attGDPpc[,c(1,i+1)] # Create GDP node att column
  
  #load(file = "HDI.mat.rda") 
  #attHDI$country <- as.character(attHDI$country)
  
  #attHDI$country[attHDI$country == "_UK"] <- "UK_"
  #node.att.1990 <- merge(node.att.1990, attHDI[,c(1,1+i)], by = "country") # Merging HDI node att
  #node.att.1990 <- attHDI[,c(1,i+1)]
  
  colnames(node.att.1990)[2] <- "GDP"
  #colnames(node.att.1990)[2] <- "HDI"
  
  #node.att.1990 <- node.att.1990[as.character(node.att.1990$GDP)!= "" ,]
  node.att.1990 <- na.omit(node.att.1990)
  
  edge.att.1990 <- filter(m1, is.element(m1$namea, node.att.1990$country) & is.element(m1$nameb, node.att.1990$country)) 
  node.att.1990 <- filter(node.att.1990, is.element(node.att.1990$country, edge.att.1990$namea) & is.element(node.att.1990$country, edge.att.1990$nameb)) 
  
  edge.att.1990 <- edge.att.1990[order(edge.att.1990$namea, edge.att.1990$nameb),]
  threat_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,ncol(m1))])
  
  threatnet <- network(threat_adj_1990$adjacency,directed = TRUE,matrix.type = "adjacency")
  
  network::set.vertex.attribute(threatnet, 'Per Capita Income', as.numeric(node.att.1990$GDP))
  #network::set.vertex.attribute(threatnet, 'HDI', node.att.1990$HDI)
  network::set.network.attribute(threatnet,'Wars', edge.att.1990$Wars)
  network::set.network.attribute(threatnet,'Alliance', edge.att.1990$Alliance)
  network::set.network.attribute(threatnet,'Border', edge.att.1990$border)
  network::set.network.attribute(threatnet,'Aid', edge.att.1990$Aid)
  
  q <- paste("threatnet",j,"final",".rda", sep = "")
  save(threatnet, file = q)
}


load(file = 'threatnet2000final.rda')
threatnet -> tnet2000
load(file = 'threatnet2001final.rda')
threatnet -> tnet2001
load(file = 'threatnet2002final.rda')
threatnet -> tnet2002
load(file = 'threatnet2003final.rda')
threatnet -> tnet2003
load(file = 'threatnet2004final.rda')
threatnet -> tnet2004
load(file = 'threatnet1990final.rda')
threatnet -> tnet1990
load(file = 'threatnet1991final.rda')
threatnet -> tnet1991
load(file = 'threatnet1992final.rda')
threatnet -> tnet1992
load(file = 'threatnet1993final.rda')
threatnet -> tnet1993
load(file = 'threatnet1994final.rda')
threatnet -> tnet1994
load(file = 'threatnet1995final.rda')
threatnet -> tnet1995
load(file = 'threatnet1996final.rda')
threatnet -> tnet1996
load(file = 'threatnet1997final.rda')
threatnet -> tnet1997
load(file = 'threatnet1998final.rda')
threatnet -> tnet1998
load(file = 'threatnet1999final.rda')
threatnet -> tnet1999
netlist <- list(tnet1990,tnet1991,tnet1992,tnet1993,tnet1994
,tnet1995,tnet1996,tnet1997,tnet1998,tnet1999,
tnet2000,tnet2001,tnet2002,tnet2003,tnet2004)


"model_fulltime41 <- btergm(netlist ~ edges + mutual() 
                          + nodeocov('Per Capita Income')
                          + nodeicov('Per Capita Income')
                          + edgecov('Alliance')
                          + edgecov('Border')
                          + edgecov('Aid'),
                          R = 50
)
summary(model_fulltime41)

model_fulltime42 <- btergm(netlist ~ edges + mutual() + nodeocov('Per Capita Income')
                          + nodeicov('Per Capita Income')
                          + edgecov('Alliance')
                          + edgecov('Aid'),
                          R = 50
)
summary(model_fulltime42)

model_fulltime43 <- btergm(netlist ~ edges + mutual() + nodeocov('Per Capita Income')
                          + nodeicov('Per Capita Income')
                          + edgecov('Border')
                          + edgecov('Aid'),
                          R = 50
)
summary(model_fulltime43)

model_fulltime44 <- btergm(netlist ~ edges + mutual() + nodeocov('Per Capita Income')
                          + nodeicov('Per Capita Income')
                          + edgecov('Aid'),
                          R = 50
)
summary(model_fulltime44)

model_fulltime45 <- btergm(netlist ~ edges + mutual() + nodeocov('Per Capita Income')
                          + nodeicov('Per Capita Income')
                          + edgecov('Wars')
                          + edgecov('Aid'),
                          R = 50
)
summary(model_fulltime45)
"

#save(model_fulltime41,model_fulltime42,model_fulltime43,model_fulltime44,model_fulltime45,file = "tergms_threat_41to45.rda")
