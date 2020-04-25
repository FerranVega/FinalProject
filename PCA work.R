rm(list=ls())

library(dils)
library(dplyr)

load("data90.rda")
load("data95.rda")
load("data20.rda")

data <- rbind(data90, data95, data20) # Bind together
data %>% select(SrcName, EventForm) -> data # Select "actors" and "actions" columns

Actions <- data %>%  
  group_by(SrcName, EventForm) %>%
  summarise(weight = n()) %>% 
  ungroup()

Actions <- tail(Actions, -32)
colnames(Actions) <- c("Countries", "Events", "Counts")

countries <- as.data.frame(sort(unique(data$SrcName)))
countries <- tail(countries, -5) # Remove actors that are not countries
events <- as.data.frame(sort(unique(data$EventForm)))

PCA_data <- expand.grid(c(countries, events))
colnames(PCA_data) <- c("Countries", "Events")
PCA_data <- PCA_data[order(PCA_data$Countries),]

PCA_data_1 <- merge(PCA_data, Actions, by = c("Countries", "Events") , all.x = TRUE)
PCA_data_1$Counts[is.na(PCA_data_1$Counts)] = 0

PCA_data_1 <- reshape(PCA_data_1, idvar = "Countries", timevar = "Events", direction = "wide")

save(PCA_data_1, file = "PCA_data.rda")

