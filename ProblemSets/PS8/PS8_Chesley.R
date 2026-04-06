library(nloptr)
library(tidyverse)
library(modelsummary)

#Create the data set
set.seed(100)
X <- cbind(1, matrix(rnorm(100000 * 9), nrow = 100000, ncol = 9))
eps <- rnorm(100000, mean = 0, sd = 0.5)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

Y <- X %*% beta + eps

--------------------------------------------------------------------------------
#Beta OLS estimate
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% Y

#The estimates are pretty accurate to the beta vector. Almost all of the betas
#are within 0.01 of the beta ols'.

--------------------------------------------------------------------------------
##Gradient Descent

alpha <- 0.0000003
iter <- 500

set.seed(100)
beta_gd <- floor(runif(1)*10)
beta_gd <- rep(0, 10)

for(i in 1:iter){
  gradient <- t(X) %*% (X %*% beta_gd - Y)
  beta_gd <- beta_gd - alpha * gradient
}
print(beta_gd)

--------------------------------------------------------------------------------
##L-BFGS algorithm

objfun_lb <- function(beta) {
  return( sum((Y - X %*% beta)^2))
}

# Gradient of our objective function
grad_lb <- function(beta) {
  return(t(X) %*% (X %*% beta - Y) * 2)
}

x0_lb = rep(0, 10)

# Algorithm parameters
opts_lb <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=3.0e-8)

# Find the optimum!
res_lb <- nloptr( x0=x0_lb,
                  eval_f=objfun_lb,
                  eval_grad_f=grad_lb,
                  opts=opts_lb)
print(res_lb)

#The results are still pretty close to the actual value beta vector.
--------------------------------------------------------------------------------
##Nelder-Mead

objfun_nm <- function(beta) {
  return( sum((Y - X %*% beta)^2))
}

x0_nm <- rep(0, 10)

# Algorithm parameters
opts_nm <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=3.0e-8)

# Find the optimum!
res_nm <- nloptr( x0=x0_nm,
                  eval_f=objfun_nm,
                  opts=opts_nm)
print(res_nm)
--------------------------------------------------------------------------------
##Beta MLE using nloptr

loglike_mle <- function(theta) {
  beta <- theta[1:(length(theta) - 1)]
  sig  <- theta[length(theta)]
  n    <- nrow(X)
  return(n/2 * log(2*pi) + n*log(sig) + sum((Y - X %*% beta)^2) / (2*sig^2))
}


grad_mle <- function(theta) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta) - 1)]
  sig  <- theta[length(theta)]
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

eval_mle_f <- function(theta) {
  list(objective = loglike_mle(theta),
       gradient  = grad_mle(theta))
}
K <- ncol(X)
x0_mle <- c(rep(0, K), 1)
opts_mle <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=3.0e-8,"maxeval"=1e4)

res_mle <- nloptr(x0= x0_mle,
                  eval_f = eval_mle_f,
                  opts   = opts_mle)
print(res_mle)

betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]
--------------------------------------------------------------------------------
#The easy way using lm()
ols <- lm(Y ~ X - 1)
modelsummary(ols, output = "regression_output.tex")
--------------------------------------------------------------------------------