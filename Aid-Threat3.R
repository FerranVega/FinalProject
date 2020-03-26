### TERGM
library(btergm)

load(file = "mln1994.rda")
fullnet <- list(aidthreatnet1990,aidthreatnet1991,aidthreatnet1992,aidthreatnet1993,aidthreatnet1994)

btergmod1 <- btergm(fullnet ~ edges + timecov(transform = function(t)t))
summary(btergmod1)

btergmod2 <-  btergm(fullnet
                   ~ edges_layer(layer = 1) + edges_layer(layer = 2)
                   + mutual("layer.mem", diff = T)
                   + duplexdyad(c("e", "f"), layers = list(1, 2))
                   + timecov(transform = function(t)t)
                   )
summary(btergmod2)
