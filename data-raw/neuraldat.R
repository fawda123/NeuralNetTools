library(clusterGeneration)

seed.val <- 2
set.seed(seed.val)

num.vars <- 8
num.obs <- 1000

#input variables
cov.mat <- genPositiveDefMat(num.vars, covMethod = c("unifcorrmat"))$Sigma
rand.vars <- mvrnorm(num.obs, rep(0, num.vars), Sigma = cov.mat)

#output variables
parms <- runif(num.vars, -10, 10)
y1 <- rand.vars %*% matrix(parms) + rnorm(num.obs, sd = 20)
parms2 <- runif(num.vars, -10, 10)
y2 <- rand.vars %*% matrix(parms2) + rnorm(num.obs, sd = 20)

#final datasets
rand.vars <- data.frame(rand.vars)
resp <- data.frame(y1, y2)
names(resp) <- c('Y1', 'Y2')
neuraldat <- data.frame(resp, rand.vars)

save(neuraldat, file = 'data/neuraldat.RData')
