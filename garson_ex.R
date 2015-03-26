dat0 <- read.csv('examples.csv', header = T)

set.seed(123)
library(nnet)
# library(NeuralNetTools)
devtools::load_all('M:/docs/NeuralNetTools')

n0<-sample(1:nrow(dat0),round(nrow(dat0)*0.6))    # take 60% for the training

foo<-nnet(as.factor(y) ~ NAO + EA + WP + EP_NP + PNA + EA_WR + SCA + POL + ENSO_MEI + AO + AMO + PDO,data=dat0[n0,],decay=5e-4, maxit=100,size=15)

p1 <- garson(foo, '0')
p2 <- garson(foo, '1')
p3 <- garson(foo, '2')
p4 <- garson(foo, '3')

library(gridExtra)

pdf('plots.pdf')
grid.arrange(p1, p2, p3, p4)
dev.off()


set.seed(seed.val)
resp <- neuraldat[, grepl('^Y', names(neuraldat))] 
rand.vars <- neuraldat[, !grepl('^Y', names(neuraldat))] 
mod1<-nnet(rand.vars,resp,data=dat.in,size=10,linout=T)
plotnet(mod1)

p1 <- garson(mod1, 'Y1')
p2 <- olden(mod1, 'Y2')
grid.arrange(p1, p2)
