#library(tidyverse)
#library(lubridate)
#library(dils)
#library(statnet)
#library(plyr)

#load(file = "migrants_by_year.rda")
load(file = "borders.mat.rda") 
load(file = "wars_by_year.rda") 
load(file = "ally_by_year.rda")
load(file = "data90.rda")
load(file = "data95.rda") 
load(file = "data20.rda")

load(file = "civil90s.rda")
civil90 <- civil
load(file = "civil95s.rda") 
civil95 <- civil
load(file = "civil20s.rda") 
civil20 <- civil

for (i in 1:15){
  if(i > 5){
    if(i < 11){
      data <- data95
      civil <- civil95
    } else { civil <- civil20
             data <- data20 }
  } else { civil <- civil90 
           data <- data90 }
  data <- subset(data,data$EventForm == "<HIDE>")
  data <- dplyr::select(data,"EventDate", "SrcName","TgtName")
  civil <- dplyr::select(civil,"EventDate","SrcName","TgtName")
  if(i > 5){
    if(i < 11){
      data$EventDate <- ymd(data$EventDate)
      civil$EventDate <- ymd(civil$EventDate)
    } else {data$EventDate <- mdy_hms(data$EventDate)
            civil$EventDate <- mdy_hms(civil$EventDate)}
  } else {data$EventDate <- mdy(data$EventDate)
          civil$EventDate <- mdy(civil$EventDate)}

  data <- data[order(data$EventDate),]
  civil <- civil[order(civil$EventDate),]
  
  j <- i + 1989
  k <- i + 1990
  startdate <- paste(j,"/01/01",sep = "")
  enddate <- paste(k,"/01/01",sep = "")
  data <- subset(data,data$EventDate < enddate & data$EventDate >= startdate) ###################### 1990 - 1995.
  civil  <- subset(civil,  civil$EventDate < enddate & civil$EventDate  >= startdate)
  
  data <- data[,-1]
  civil <- civil[,-1]
  
  data$SrcName <- as.character(data$SrcName)
  data$TgtName <- as.character(data$TgtName)
  data$counter <- c(rep(1,nrow(data)))
  
  civil$SrcName <- as.character(civil$SrcName)
  civil$TgtName <- as.character(civil$TgtName)
  civil$counter <- c(rep(1,nrow(civil)))
  
  civil <- ddply(civil,.(SrcName,TgtName),nrow)
  civil <- civil[,-2]
  colnames(civil)[1] <- "country"

  ally1990 <- ally_by_year[[i]]
  war1990 <- wars_by_year[[i]]
  
  ally1990_1 <- ally1990
  temp <- ally1990_1[,1]
  ally1990_1[,1] <- ally1990_1[,2]
  ally1990_1[,2] <- temp
  ally1990 <- rbind(ally1990, ally1990_1) # Makes alliances edglist "complete"
  
  colnames(borders.mat)[3] <- "UK_"
  rownames(borders.mat)[3] <- "UK_"
  
  #mig1990 <- EdgelistFromAdjacency(as.matrix(migrants1990[,2:190]), nodelist = colnames(migrants1990[,2:190]))
  
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
  
  #colnames(mig1990)[1] <- "namea"
  #colnames(mig1990)[2] <- "nameb"
  
  #m1 <- merge(m1, mig1990, by = c("namea", "nameb"), all = TRUE)
  #colnames(m1)[6] <- "Mig"
  
  colnames(data)[1] <- "namea"
  colnames(data)[2] <- "nameb"
  
  m1 <- merge(m1, data, by = c("namea", "nameb"), all = TRUE)
  colnames(m1)[ncol(m1)] <- "Asylum"
  
  m1[is.na(m1)] = 0
  
  load(file = "GDPpc.mat.rda") 
  attGDPpc$country <- as.character(attGDPpc$country)
  
  attGDPpc$country[attGDPpc$country == "_UK"] <- "UK_"
  node.att.1990 <- attGDPpc[,c(1,i+1)] # Create GDP node att column
  colnames(node.att.1990)[2] <- "GDP"
  node.att.1990 <- merge(node.att.1990, civil, by = "country", all = TRUE) #Merging civil war data.
  colnames(node.att.1990)[3] <- "CivilWars"
  node.att.1990$CivilWars[is.na(node.att.1990$CivilWars)] = 0
  
  #load(file = "HDI.mat.rda") 
  #attHDI$country <- as.character(attHDI$country)
  
  #attHDI$country[attHDI$country == "_UK"] <- "UK_"
  #node.att.1990 <- merge(node.att.1990, attHDI[,c(1,i+1)], by = "country") # Merging HDI node att
  #colnames(node.att.1990)[4] <- "HDI"
  #EFI <- read.csv("EFIndex_withNA.csv") 
  #EFI <- EFI[3:19]
  #colnames(EFI)[1] <- "country"
  #EFI$country <- as.character(EFI$country)
  #node.att.1990 <- merge(node.att.1990, EFI[,c(1,i+1)], by = "country") # Merging ethnic frac node att
  #colnames(node.att.1990)[5] <- "EFI"
  
  #Religion <- read.csv("major_religion.csv", head = TRUE, sep=",") 
  #Religion <- select(Religion, "Code", "major")
  #colnames(Religion)[1] <- "country"
  #Religion$country <- as.character(Religion$country)
  #node.att.1990 <- merge(node.att.1990, Religion[,c(1,i+1)], by = "country") # Merging religion node att
  #colnames(node.att.1990)[6] <- "RELIGION"
  node.att.1990 <- node.att.1990[as.character(node.att.1990$GDP1990)!= "" ,]
  node.att.1990 <- na.omit(node.att.1990)

  edge.att.1990 <- filter(m1, is.element(m1$namea, node.att.1990$country) & is.element(m1$nameb, node.att.1990$country)) 
  node.att.1990 <- filter(node.att.1990, is.element(node.att.1990$country, edge.att.1990$namea) & is.element(node.att.1990$country, edge.att.1990$nameb)) 
  
  edge.att.1990 <- edge.att.1990[order(edge.att.1990$namea, edge.att.1990$nameb),]
  
  #war_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,3)])
  #alliance_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,4)])
  #bord_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,5)])
  #mig_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,6)])
  asylum_adj_1990 <- AdjacencyFromEdgelist(edge.att.1990[,c(1:2,ncol(m1))])
  #Adjlist <- list(war_adj_1990$adjacency,alliance_adj_1990$adjacency,bord_adj_1990$adjacency,mig_adj_1990$adjacency)
  #save(node.att.1990,file = "Asylum_Node_Attributes_1990.rda")
  #save(Adjlist,file = "Asylum_Edge_Adjacencies_1990.rda")
  #save(asylum_adj_1990,file = "Asylum_Adjacency_1990.rda")
  
  asylumnet <- network(asylum_adj_1990$adjacency,directed = TRUE,matrix.type = "adjacency")
  
  network::set.vertex.attribute(asylumnet, 'Per Capita Income', as.numeric(node.att.1990$GDP))
  network::set.vertex.attribute(asylumnet, 'Civil Conflicts', as.numeric(node.att.1990$CivilWars))
  #network::set.vertex.attribute(asylumnet, 'HDI', node.att.1990$HDI)
  #network::set.vertex.attribute(asylumnet, 'EFI', node.att.1990$EFI)
  #network::set.vertex.attribute(asylumnet, 'Religion', as.character(node.att.1990$RELIGION))
  network::set.network.attribute(asylumnet,'Wars', edge.att.1990$Wars)
  network::set.network.attribute(asylumnet,'Alliance', edge.att.1990$Alliance)
  network::set.network.attribute(asylumnet,'Border', edge.att.1990$border)
  #network::set.network.attribute(asylumnet,'Migrants', edge.att.1990$Mig)
  
  
  q <- paste("asylumnet",j,"final",".rda", sep = "")
  save(asylumnet, file = q)
}
