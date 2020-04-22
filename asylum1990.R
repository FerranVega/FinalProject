library(btergm)



load(file = 'asylumnet1990final.rda')
asylumnet -> anet1990
load(file = 'asylumnet1991final.rda')
asylumnet -> anet1991
load(file = 'asylumnet1992final.rda')
asylumnet -> anet1992
load(file = 'asylumnet1993final.rda')
asylumnet -> anet1993
load(file = 'asylumnet1994final.rda')
asylumnet -> anet1994

load(file = 'asylumnet1995final.rda')
asylumnet -> anet1995
load(file = 'asylumnet1996final.rda')
asylumnet -> anet1996
load(file = 'asylumnet1997final.rda')
asylumnet -> anet1997
load(file = 'asylumnet1998final.rda')
asylumnet -> anet1998
load(file = 'asylumnet1999final.rda')
asylumnet -> anet1999

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
netlist <- list(anet1990,anet1991,anet1992,anet1993,anet1994,
                anet1995,anet1996,anet1997,anet1998,anet1999,
                anet2000,anet2001,anet2002,anet2003,anet2004)

model_2000s <- btergm(netlist ~ edges + mutual() + nodeocov('Per Capita Income')
                + nodeicov('Per Capita Income') + nodeocov('')
                + gwidegree(1,fixed = TRUE) + gwodegree(1, fixed = TRUE)
                + edgecov('Alliance')
                + edgecov('Border') + edgecov('Migrants'),
                R = 50
                )

summary(model_2000s)
