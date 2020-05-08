d2 <- read.csv("kingdata.csv")
d9 <- read.csv('final1990.csv')
d95 <- read.csv("final1995.csv")

civil <- subset(d2,d2$EventForm == "<CLAS>" & d2$SrcName == d2$TgtName)
save(civil,file = "civil20s.rda") #These are later loaded into ERGM_asylum script. Keep
#it that way because it might mess up the if-then I put in the for loop.

civil <- subset(d9,d9$EventForm == "<CLAS>" & as.character(d9$SrcName) == as.character(d9$TgtName))
save(civil,file = "civil90s.rda")

civil <- subset(d95,d95$EventForm == "<CLAS>" & d95$SrcName == d95$TgtName)
save(civil,file = "civil95s.rda")