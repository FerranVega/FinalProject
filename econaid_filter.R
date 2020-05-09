d2000 <- read.csv("kingdata.csv")
d1995 <- read.csv("final1995.csv")
d1990 <- read.csv("final1990.csv")

d20 <- subset(d2000, d2000$EventForm == "<EEAI>")
d20 <- subset(d20, d20$SrcName != d20$TgtName)

d95 <- subset(d1995, d1995$EventForm == "<EEAI>")
d95 <- subset(d95, d95$SrcName != d95$TgtName)

d90 <- subset(d1990, d1990$EventForm == "<EEAI>")
d90 <- subset(d90, as.character(d90$SrcName) != as.character(d90$TgtName))

d20 -> ea20s
d95 -> ea95s
d90 -> ea90s

save(ea20s,ea95s,ea90s,file = "econaids.rda")
#