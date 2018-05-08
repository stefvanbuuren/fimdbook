
###------------------------MICE.IMPUTE.NORM.BB----------------------
mice.impute.norm.bb <- function(y, ry, x, ridge=0.00001, ...)
{
# mice.impute.norm.bb
# Regression imputations of y given x, with a fixed regression
# line, and with random draws of the residuals around the line.
# Bayesian bootstrap
#

  x <- cbind(1, as.matrix(x))
  xobs <- x[ry,]
  yobs <- y[ry]
  n1 <- sum(ry)
  n0 <- sum(!ry)

  # do here the Bayesian bootstap Rubin p. 124
  u <- runif(n1-1)
  u <- diff(c(0,u[order(u)],1))
  s <- sample(n1, n1, replace=TRUE, prob=u)

  dotxobs <- xobs[s,]
  dotyobs <- yobs[s]
  xtx <- t(dotxobs) %*% dotxobs
  pen <- ridge * diag(xtx)
  if (length(pen)==1) pen <- matrix(pen)
  v <- solve(xtx + diag(pen))
  coef <- t(dotyobs %*% dotxobs %*% v)
  residuals <- dotyobs - dotxobs %*% coef
  sigma <- sqrt((sum(residuals^2))/(n1-ncol(x)-1))
  parm <- list(coef, sigma)
  names(parm) <- c("beta", "sigma")
  return(x[!ry,  ] %*% parm$beta + rnorm(n0) * parm$sigma)
}

mice.impute.normdump <- function (y, ry, x, ...) 
{
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw(y, ry, x, ...)
    betadump <<- c(betadump,parm$beta) 
    return(x[!ry, ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
}

mice.impute.pmmdump <- function (y, ry, x, ...) 
{
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw(y, ry, x, ...)
    yhatobs <- x[ry, ] %*% parm$coef
    yhatmis <- x[!ry, ] %*% parm$beta
    betadump <<- c(betadump,parm$beta)
    return(apply(as.array(yhatmis), 1, .pmm.match, yhat = yhatobs, 
        y = y[ry], ...))
}




#-------------------------MICE.IMPUTE.2L.NORM.BOOT--------------------

mice.impute.2L.norm.boot <- function(y, ry, x, type, intercept=FALSE, ...)
{
#
# Bootstrap version of mice.impute.2L.norm
#
# Author: Stef van Buuren, 2011
#
  n.class <- length(unique(x[, type==(-2)]))
  s1 <- sample(n.class, n.class, replace=TRUE)
  dotx <- 
  gf.full <- factor(x[,type==(-2)], labels=1:n.class)
  gf <- gf.full[ry]
  n.g <- tabulate(gf)
  
  
  ## draw a bootstrap sample for yobs and xobs
  xobs <- x[ry,]
  yobs <- y[ry]
  n1 <- sum(ry)
  n0 <- sum(!ry)
  s <- sample(n1, n1, replace=TRUE)
  doty <- y
  doty[ry] <- yobs[s]
  dotx <- x
  dotx[ry,] <- xobs[s,]

  x <- dotx
  y <- doty
  
  ## append intercept
  if (intercept) {
    x <- cbind(1, as.matrix(x))
    type <- c(2, type)
  }

  expr <- expression(glm.fit(x[ry, ], y[ry],
      family = binomial(link = logit)))
  fit <- suppressWarnings(eval(expr))
  beta.star <- beta <- coef(fit)
  p <- 1/(1 + exp(-(x[!ry,] %*% beta.star)))
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  if (is.factor(y)) {
    vec<-factor(vec,c(0,1),levels(y))}
  return(vec)
}
