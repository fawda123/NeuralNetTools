library(dplyr)

seed.val <- 2
set.seed(seed.val)

num.vars <- 3
num.obs <- 300

#input variables
rand.vars <- matrix(rnorm(num.vars * num.obs), ncol = num.vars)

#output variables
parms <- runif(num.vars, -1, 1)
y1 <- rand.vars %*% matrix(parms)
parms2 <- runif(num.vars, -1, 1)
y2 <- rand.vars %*% matrix(parms2)

#final datasets
rand.vars <- data.frame(rand.vars)
resp <- data.frame(y1, y2)
names(resp) <- c('Y1', 'Y2')
neuraldat <- data.frame(resp, rand.vars)

# normalize all variables
norm.fun <- function(x){ 
  (x - min(x))/(max(x) - min(x)) 
}

# standardize only response variables
neuraldat <- mutate_each(neuraldat, funs(norm.fun), matches('^Y[0-9]$')) 

save(neuraldat, file = 'data/neuraldat.RData')

