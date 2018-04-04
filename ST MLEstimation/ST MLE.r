# MLE function

library(COUNT)
library(stats4)
library(bbmle)
data(rwm1984)
attach(rwm1984)
 
### OPTIM() ###
LogLike1 <- function(par) {
  xb <- par[1] + par[2] * outwork + par[3] * age + par[4] * female + par[5] * married 
  mu <- exp(xb)
  ll <- sum(log(exp(-mu) * (mu ^ docvis) / factorial(docvis)))
  return(-ll)
}
fit1 <- optim(rep(0, 5), LogLike1, hessian = TRUE, method = "BFGS")
std1 <- sqrt(diag(solve(fit1$hessian)))
est1 <- data.frame(beta = fit1$par, stder = stder1, z_values = fit1$par / stder1)
 
### MLE() ###
LogLike2 <- function(b0, b1, b2, b3, b4) {
  mu <- exp(b0 + b1 * outwork + b2 * age + b3 * female + b4 * married)
  -sum(log(exp(-mu) * (mu ^ docvis) / factorial(docvis)))
}
inits <- list(b0 = 0, b1 = 0, b2 = 0, b3 = 0, b4 = 0)
fit2 <- mle(LogLike2, method = "BFGS", start = inits)
std2 <- sqrt(diag(vcov(fit2)))
est2 <- data.frame(beta = coef(fit2), stder = std2, z_values = coef(fit2) / std2)
 
### BENCHMARKS ###
microbenchmark::microbenchmark(
  "optim" = {optim(rep(0, 5), LogLike1, hessian = TRUE, method = "BFGS")},
  "mle"   = {mle(LogLike2, method = "BFGS", start = inits)},
  "mle2"  = {mle2(LogLike2, method = "BFGS", start = inits)},
  times = 10
)