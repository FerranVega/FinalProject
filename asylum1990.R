library(btergm)

load(file = 'asylumnet2000final.rda')
asylumnet -> anet2000
load(file = 'asylumnet2001final.rda')
asylumnet -> anet2001
load(file = 'asylumnet2002final.rda')
asylumnet -> anet2002
load(file = 'asylumnet2003final.rda')
asylumnet -> anet2003
load(file = 'asylumnet2004final.rda')
asylumnet -> anet2004
netlist <- list(anet2000,anet2001,anet2002,anet2003,anet2004)

model_2000s <- btergm(netlist ~ edges + mutual() + nodeocov('Per Capita Income')
                + nodeicov('Per Capita Income')
                + gwidegree(1,fixed = TRUE) + gwodegree(1, fixed = TRUE)
                + edgecov('Alliance')
                + edgecov('Border') + edgecov('Migrants'),
                R = 50
                )
summary(model_2000s)
