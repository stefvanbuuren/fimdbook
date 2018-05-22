## ----ch1, child = "src/ch1.Rnw"------------------------------------------

## ----init1, echo = FALSE, results = 'hide'-------------------------------
opts_chunk$set(fig.path = 'fig/ch01-', self.contained = FALSE)
suppressPackageStartupMessages(library(mice, warn.conflicts = FALSE, quietly = TRUE))
source("R/mi.hist.R")

## ----mean1---------------------------------------------------------------
y <- c(1, 2, 4)
mean(y)

## ----mean2---------------------------------------------------------------
y <- c(1, 2, NA)
mean(y)

## ----mean3---------------------------------------------------------------
mean(y, na.rm = TRUE)

## ----air2, eval = FALSE--------------------------------------------------
fit <- lm(Ozone ~ Wind, data = airquality)
# Error in na.fail.default: missing values in object}

## ----air3----------------------------------------------------------------
fit <- lm(Ozone ~ Wind, data = airquality, na.action = na.omit)

## ----air3b---------------------------------------------------------------
options(na.action = na.omit)

## ----air4, eval = FALSE--------------------------------------------------
airquality2 <- cbind(airquality, predict(fit))
# Error: arguments imply differing number of rows: 153, 116

## ----air5----------------------------------------------------------------
head(na.action(fit))

## ----air6----------------------------------------------------------------
naprint(na.action(fit))

## ----air7----------------------------------------------------------------
colSums(is.na(airquality))

## ----air8, include = FALSE-----------------------------------------------
airquality2 <- cbind(na.omit(airquality[, c("Ozone", "Wind")]),
                     predicted = predict(fit))

## ----plotair9, echo=FALSE, fig.width=4.5, fig.height=2.25, duo = TRUE----
lwd <- 0.6
plot(predicted ~ Ozone, data = airquality2, type = "n",
     lwd = lwd, ylim = c(-20, 165), xlim = c(-20, 165),
     ylab = "Ozone predicted (ppb)",
     xlab = "Ozone measured (ppb)", axes = FALSE)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
abline(0, 1, lty = 3, lwd = 0.6)
points(predicted ~ Ozone, data = airquality2,
       lwd = 1.5, col = mdc(1))
box(lwd = lwd)
fit2 <- lm(Ozone ~ Wind, data = airquality, na.action = na.exclude)
plot(predicted ~ Ozone, data = airquality2, type = "n",
     ylim = c(-20, 165), xlim = c(-20, 165),
     ylab = " Ozone predicted (ppb)",
     xlab = "Ozone measured (ppb)",
     axes = FALSE)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
abline(0, 1, lty = 3, lwd = 0.6)
pred2 <- cbind(1, airquality$Wind) %*% coef(fit)
abline(h = pred2[ici(predict(fit2))], col = mdc(2), lwd = 0.6, lty = 1)
box(lwd = lwd)

## ----airx6---------------------------------------------------------------
fit2 <- lm(Ozone ~ Wind + Solar.R, data = airquality)
naprint(na.action(fit2))

## ----pairwise1, eval=FALSE-----------------------------------------------
data <- airquality[, c("Ozone", "Solar.R", "Wind")]
mu <- colMeans(data, na.rm = TRUE)
cv <- cov(data, use = "pairwise")

## ----pairwise2, eval = FALSE---------------------------------------------
library(lavaan)
fit <- lavaan("Ozone ~ 1 + Wind + Solar.R
              Ozone ~~ Ozone",
             sample.mean = mu, sample.cov = cv,
             sample.nobs = sum(complete.cases(data)))

## ----install, eval=FALSE-------------------------------------------------
install.packages("mice")

## ----load, eval = FALSE--------------------------------------------------
library("mice")

## ----meanimp, echo=TRUE--------------------------------------------------
imp <- mice(airquality, method = "mean", m = 1, maxit = 1)

## ----plotmeanimp, duo = TRUE, echo=FALSE, fig.width=4.5, fig.height=2.25----
lwd <- 0.6
data <- complete(imp)
Yobs <- airquality[,"Ozone"]
Yimp <- data[,"Ozone"]
mi.hist(Yimp, Yobs,b=seq(-20,200,10),type="continuous",
        gray=F,lwd = lwd,
        obs.lwd=1.5, mis.lwd=1.5, imp.lwd=1.5,
        obs.col=mdc(4), mis.col=mdc(5), imp.col="transparent",
        mlt=0.08,main="",xlab="Ozone (ppb)",
        axes = FALSE)
box(lwd = 1)
plot(data[cci(imp),2:1],col=mdc(1), lwd=1.5,ylab="Ozone (ppb)",
     xlab="Solar Radiation (lang)",ylim=c(-10,170),
     axes = FALSE)
points(data[ici(imp),2:1],col=mdc(2),lwd=1.5)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
box(lwd = 1)

## ----regimp, echo=TRUE---------------------------------------------------
fit <- lm(Ozone ~ Solar.R, data = airquality)
pred <- predict(fit, newdata = ic(airquality))

## ----plotregimp, duo = TRUE, echo=FALSE, fig.width=4.5, fig.height=2.25----
lwd <- 0.6
Yobs <- airquality[,"Ozone"]
Yimp <- Yobs
Yimp[ici(airquality)] <- pred
ss <- cci(airquality$Solar.R)
data <- data.frame(Ozone=Yimp, Solar.R=airquality$Solar.R)
mi.hist(Yimp[ss], Yobs[ss],b=seq(-20,200,10),type="continuous",
        gray=F, lwd = lwd,
        obs.lwd=1.5, mis.lwd=1.5, imp.lwd=1.5,
        obs.col=mdc(4),mis.col=mdc(5), imp.col="transparent",
        mlt=0.08,main="",xlab="Ozone (ppb)", axes = FALSE)
box(lwd = 1)
plot(data[cci(imp),2:1],col=mdc(1),lwd=1.5,
     ylab="Ozone (ppb)", xlab="Solar Radiation (lang)",
     ylim=c(-10,170), axes = FALSE)
points(data[ici(imp),2:1],col=mdc(2),lwd=1.5)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
box(lwd = 1)

## ----regimp2, eval = FALSE-----------------------------------------------
data <- airquality[, c("Ozone", "Solar.R")]
imp <- mice(data, method = "norm.predict", seed = 1,
           m = 1, print = FALSE)
xyplot(imp, Ozone ~ Solar.R)

## ----sri-----------------------------------------------------------------
data <- airquality[, c("Ozone", "Solar.R")]
imp <- mice(data, method = "norm.nob", m = 1, maxit = 1,
            seed = 1, print = FALSE)

## ----plotsri, duo = TRUE, echo=FALSE, fig.width=4.5, fig.height=2.25-----
lwd <- 0.6
data <- complete(imp)
Yobs <- airquality[, "Ozone"]
Yimp <- data[, "Ozone"]
mi.hist(Yimp, Yobs,
        b = seq(-20, 200, 10), type = "continuous",
        gray = FALSE, lwd = lwd,
        obs.lwd = 1.5, mis.lwd = 1.5, imp.lwd = 1.5,
        obs.col = mdc(4),mis.col = mdc(5), imp.col = "transparent",
        mlt = 0.08, main = "", xlab = "Ozone (ppb)")
box(lwd = 1)
plot(data[cci(imp), 2:1], col = mdc(1),
     lwd = 1.5, ylab = "Ozone (ppb)",
     xlab = "Solar Radiation (lang)", ylim = c(-10, 170),
     axes = FALSE)
points(data[ici(imp), 2:1], col = mdc(2), lwd = 1.5)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
box(lwd = 1)

## ----locf1---------------------------------------------------------------
airquality2 <- tidyr::fill(airquality, Ozone)

## ----locf2,echo=FALSE,fig.width=4.5,fig.height=2.5, solo=TRUE------------
lwd <- 0.6
Oz <- airquality$Oz
Ozi <- airquality2$Oz
colvec <- ifelse(is.na(Oz), mdc(2), mdc(1))
plot(Ozi[1:80], col = colvec, type = "l",
     xlab = "Day number", ylab = "Ozone (ppb)",
     lwd = lwd, axes = FALSE)
points(Ozi[1:80], col = colvec, pch = 20)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
box(lwd = lwd)

## ----indicator1----------------------------------------------------------
imp <- mice(airquality, method = "mean", m = 1,
            maxit = 1, print = FALSE)
airquality2 <- cbind(complete(imp),
                     r.Ozone = is.na(airquality[, "Ozone"]))
fit <- lm(Wind ~ Ozone + r.Ozone, data = airquality2)

## ----airx8---------------------------------------------------------------
imp <- mice(airquality, seed = 1, m = 20, print = FALSE)
fit <- with(imp, lm(Ozone ~ Wind + Temp + Solar.R))
summary(pool(fit))

## ----airx9---------------------------------------------------------------
fit <- lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
coef(summary(fit))

## ----plotairmi, echo=FALSE, fig.width=4.5, fig.height=2.25, duo = TRUE----
lwd <- 0.6
data <- complete(imp)
Yobs <- airquality[,"Ozone"]
Yimp <- data[,"Ozone"]
mi.hist(Yimp, Yobs,b=seq(-20,200,10),type="continuous",
        gray=F,lwd = lwd,
        obs.lwd=1.5, mis.lwd=1.5, imp.lwd=1.5,
        obs.col=mdc(4),mis.col=mdc(5), imp.col="transparent",
        mlt=0.08,main="",xlab="Ozone (ppb)")
box(lwd = 1)
plot(data[cci(imp),2:1],
     col = mdc(1),lwd = 1.5,
     ylab = "Ozone (ppb)",
     xlab = "Solar Radiation (lang)",
     ylim = c(-10, 170), axes = FALSE)
points(data[ici(imp),2:1],
       col = mdc(2), lwd = 1.5)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
box(lwd = 1)

## ----air10, echo=FALSE,fig.width=4.5,fig.height=2.5, solo=TRUE-----------
Oz <- airquality$Ozone
colvec <- ifelse(is.na(Oz),mdc(2),mdc(1))

plot(Oz[1:80],col=mdc(1),type="l",xlab="Day number",ylab="Ozone (ppb)",
     axes = FALSE,lwd = lwd)
points(Oz[1:80],col=mdc(1),pch=20)
lwd <- 0.6
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
box(lwd = lwd)

idx <- ici(airquality$Ozone) & (1:153)<81
x   <- (1:153)[idx]
points(x=x,y=complete(imp,1)$Ozone[idx],col=mdc(2),lwd=lwd,pch=20)
points(x=x,y=complete(imp,2)$Ozone[idx],col=mdc(2),lwd=lwd,pch=20)
points(x=x,y=complete(imp,3)$Ozone[idx],col=mdc(2),lwd=lwd,pch=20)
points(x=x,y=complete(imp,4)$Ozone[idx],col=mdc(2),lwd=lwd,pch=20)
points(x=x,y=complete(imp,5)$Ozone[idx],col=mdc(2),lwd=lwd,pch=20)


## ----acf, echo=FALSE-----------------------------------------------------
par(mfrow=c(2,5))
acf.ozone <- with(imp, acf(Ozone, plot = FALSE))
model <- expression(acf(resid(lm(Ozone ~ Wind + Temp + Solar.R)), plot = FALSE))
acf.resid <- with(imp,model, plot = FALSE)
calcacf <- function(acf.list) {
  k <- length(acf.list)
  acc <- acf.list[[1]]$acf
  for (i in 2:k) acc <- acc + acf.list[[i]]$acf
  return(acc/k)
 }
oz <- round(calcacf(acf.ozone$analyses),2)
re <- round(calcacf(acf.resid$analyses),2)


## ----ch2, child = "src/ch2.Rnw"------------------------------------------

## ----init2, echo = FALSE, results = 'hide'-------------------------------
opts_chunk$set(fig.path = 'fig/ch02-', self.contained = FALSE)
pkg <- c("mice", "lattice")
loaded <- sapply(pkg, require, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)

## ----publications, echo=FALSE, fig.width=4.5, fig.height=2.25, solo=TRUE----
cit  <- c(     2017, 75, 381, NA,
               2016, 81, 328, NA,
               2015, 59, 306, NA,
               2014, 55, 281, NA,
               2013, 45, 233, NA,
               2012, 47, 214, NA,
               2011, 55, 181, NA,
               2010, 44, 157, NA,
               2009, 38, 111, NA,
               2008, 28, 102, NA,
               2007, 34, 113, NA,
               2006, 19, 75, NA,
               2005, 21, 63, NA,
               2004,  7, 44, NA,
               2003, 18, 38, NA,
               2002, 15, 36, NA,
               2001, 14, 35, 57,
               2000,  8, 19, 33,
               1999,  6, 18, 47,
               1998,  6, 12, 22,
               1997,  6, 16, 29,
               1996,  5, 12, 28,
               1995,  3, 5, 20,
               1994,  4, 7, 34,
               1993,  3, 6, 15,
               1992, NA, 4, NA,
               1991,  3, 4, 19,
               1990,  2, 3, 15,
               1989, NA, 2, 11,
               1988, NA, 1, 13,
               1987, NA, 3, 10,
               1986,  2, 3, 5,
               1985, NA, NA, 1,
               1984, NA, 1, 2,
               1983, NA, NA, 5,
               1982, NA, NA, 2,
               1981, NA, NA, 1,
               1980, NA, NA, 5,
               1979, NA, NA, 2,
               1978, NA, NA, 1,
               1977, NA, NA, 2)
cit <- matrix(cit, nr=2018-1977, nc=4, byrow=TRUE)
cit <- as.data.frame(cit)
names(cit) <- c("Year","Title","Abstract","All")
par(cex = 0.7, lwd = 0.5)
plot(x = cit$Year, y = cit$Abstract, type="o",log="y",
     xlim = c(1975,2017), ylim = c(1,400),
     ylab="Number of publications (log)", xlab="Year",
     pch=24, bg = "white",
     axes=FALSE)
axis(1, at = seq(1977, 2017, 5), lwd = par("lwd"))
axis(2, lwd = par("lwd"), las=1)
lines(x=cit$Year, y=cit$Title, pch=15, type="o")
lines(x=cit$Year, y=cit$All, pch=16, type="o")
legend(x=1975,y=200,legend=c('early publications',
                            '"multiple imputation" in abstract',
                            '"multiple imputation" in title'),
       pch=c(16,2,15), bty="n")

## ----mar-----------------------------------------------------------------
logistic <- function(x) exp(x) / (1 + exp(x))
set.seed(80122)
n <- 300
y <- MASS::mvrnorm(n = n, mu = c(0, 0),
                   Sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2))
r2.mcar <- 1 - rbinom(n, 1, 0.5)
r2.mar  <- 1 - rbinom(n, 1, logistic(y[, 1]))
r2.mnar <- 1 - rbinom(n, 1, logistic(y[, 2]))

## ----marplot, echo=FALSE, fig.height=4, fig.width=6, solo = TRUE---------
library(lattice)
y1 <- y[, 1]
y2 <- y[, 2]
y3 <- rbind(y,y,y)
r2 <- c(r2.mcar,r2.mar,r2.mnar)
r2 <- factor(r2, labels=c("Ymis","Yobs"))
typ <- factor(rep(3:1,each=n),labels=c("MNAR","MAR","MCAR"))
d <- data.frame(y1=y3[,1],y2=y3[,2],r2=r2,typ=typ)
trellis.par.set(box.rectangle=list(col=c(mdc(2),mdc(1)),lwd=1.2))
trellis.par.set(box.umbrella=list(col=c(mdc(2),mdc(1)),lwd=1.2))
trellis.par.set(plot.symbol=list(col=mdc(3),lwd=1))
tp <- bwplot(r2~y2|typ, data=d,
             horizontal=TRUE, layout=c(1,3),
             xlab=expression(Y[2]),
             col=c(mdc(2),mdc(1)),strip=FALSE, xlim=c(-3,3),
             strip.left = strip.custom(bg="grey95"))
print(tp)

## ----theory1-------------------------------------------------------------
library(mice)
imp <- mice(nhanes, print = FALSE, m = 10, seed = 24415)
fit <- with(imp, lm(bmi ~ age))
est <- pool(fit)
est

## ----theory2-------------------------------------------------------------
summary(est, conf.int = TRUE)

## ----lin.sim1------------------------------------------------------------
create.data <- function(beta = 1, sigma2 = 1, n = 50,
                        run = 1) {
  set.seed(seed = run)
  x <- rnorm(n)
  y <- beta * x + rnorm(n, sd = sqrt(sigma2))
  cbind(x = x, y = y)
}

## ----lin.sim2------------------------------------------------------------
make.missing <- function(data, p = 0.5){
  rx <- rbinom(nrow(data), 1, p)
  data[rx == 0, "x"] <- NA
  data
}

## ----lin.sim3------------------------------------------------------------
test.impute <- function(data, m = 5, method = "norm", ...) {
  imp <- mice(data, method = method, m = m, print = FALSE, ...)
  fit <- with(imp, lm(y ~ x))
  tab <- summary(pool(fit), "all", conf.int = TRUE)
  as.numeric(tab["x", c("estimate", "2.5 %", "97.5 %")])
}

## ----lin.sim4------------------------------------------------------------
simulate <- function(runs = 10) {
  res <- array(NA, dim = c(2, runs, 3))
  dimnames(res) <- list(c("norm.predict", "norm.nob"),
                        as.character(1:runs),
                        c("estimate", "2.5 %","97.5 %"))
  for(run in 1:runs) {
    data <- create.data(run = run)
    data <- make.missing(data)
    res[1, run, ] <- test.impute(data, method = "norm.predict",
                                 m = 2)
    res[2, run, ] <- test.impute(data, method = "norm.nob")
  }
  res
}

## ----lin.sim6a, cache=TRUE-----------------------------------------------
res <- simulate(1000)

## ----lin.sim6c-----------------------------------------------------------
apply(res, c(1, 3), mean, na.rm = TRUE)

## ----lin.bias------------------------------------------------------------
true <- 1
RB <- rowMeans(res[,, "estimate"]) - true
PB <- 100 * abs((rowMeans(res[,, "estimate"]) - true)/ true)
CR <- rowMeans(res[,, "2.5 %"] < true & true < res[,, "97.5 %"])
AW <- rowMeans(res[,, "97.5 %"] - res[,, "2.5 %"])
RMSE <- sqrt(rowMeans((res[,, "estimate"] - true)^2))
data.frame(RB, PB, CR, AW, RMSE)

## ----mse1----------------------------------------------------------------
rmse <- function(truedata, imp, v = "x") {
  mx <- is.na(mice::complete(imp, 0))[, v]
  mse <- rep(NA, imp$m)
  for (k in seq_len(imp$m)) {
    filled <- mice::complete(imp, k)[mx, v]
    true <- truedata[mx, v]
    mse[k] <- mean((filled - true)^2)
  }
  sqrt(mean(mse))
}

## ----mse2----------------------------------------------------------------
simulate2 <- function(runs = 10) {
  res <- array(NA, dim = c(2, runs, 1))
  dimnames(res) <- list(c("norm.predict", "norm.nob"),
                        as.character(1:runs),
                        "RMSE")
  for(run in 1:runs) {
    truedata <- create.data(run = run)
    data <- make.missing(truedata)
    imp <- mice(data, method = "norm.predict", m = 1,
                print = FALSE)
    res[1, run, ] <- rmse(truedata, imp)
    imp <- mice(data, method = "norm.nob", print = FALSE)
    res[2, run, ] <- rmse(truedata, imp)
  }
  res
}

## ----mse.sim6a, cache = TRUE---------------------------------------------
res2 <- simulate2(1000)
apply(res2, c(1, 3), mean, na.rm = TRUE)


## ----ch3, child = "src/ch3.Rnw"------------------------------------------

## ----init3, echo = FALSE, results = 'hide'-------------------------------
opts_chunk$set(fig.path = 'fig/ch03-', self.contained = FALSE)
suppressPackageStartupMessages(library(gamlss, warn.conflicts = FALSE, quietly = TRUE))
pkg <- c("mice", "lattice", "MASS", "ImputeRobust", "rpart")
loaded <- sapply(pkg, require, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
source("R/mice.impute.x.R")

## ----gas1,  six=TRUE, echo=FALSE, fig.width=4.5, fig.height=6.75---------
library("MASS")
data <- whiteside
lwd <- 1.5
plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd,
     xlab=expression(paste("Temperature (", degree, "C)")),
     ylab="Gas consumption (cubic feet)", cex.lab = 1.6,
     axes = FALSE)
axis(1, lwd = 0.7)
axis(2, lwd = 0.7, las = 1)
box(lwd = 0.7)
points(x=5, y=3.6, pch=4, cex=2, lwd=lwd, col=mdc(2))
legend(x="bottomleft", legend="deleted observation", pch=4, col=mdc(2),
       pt.lwd=lwd, bty="n", pt.cex=2, cex = 1.6)
text(x=9, y=6.5, label="a",cex=2)

data[47,"Gas"] <- NA
plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd,
     xlab=expression(paste("Temperature (", degree, "C)")),
     ylab="Gas consumption (cubic feet)", cex.lab = 1.6,
     axes = FALSE)
axis(1, lwd = 0.7)
axis(2, lwd = 0.7, las = 1)
box(lwd = 0.7)
abline(m1<-lm(Gas~Temp, data=data, na.action=na.omit), col=mdc(4))
points(5,4.04, lwd=lwd, col=mdc(2),pch=19)
text(x=9, y=6.5, label="b",cex=2)

plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd,
     xlab=expression(paste("Temperature (", degree, "C)")),
     ylab="Gas consumption (cubic feet)", cex.lab = 1.6,
     axes = FALSE)
axis(1, lwd = 0.7)
axis(2, lwd = 0.7, las = 1)
box(lwd = 0.7)
imp <- mice(data, m=1, maxit=0)
pred <- imp$pred
pred["Gas","Insul"] <- 0
imp <- mice(data, m=5, pred=pred, meth="norm.nob", maxit=1, print=FALSE, seed=45433)
abline(m1<-lm(Gas~Temp, data=data, na.action=na.omit), col=mdc(4))
points(rep(5,5),imp$imp$Gas, lwd=lwd, col=mdc(2),pch=19)
text(x=9, y=6.5, label="c",cex=2)

plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd,
     xlab=expression(paste("Temperature (", degree, "C)")),
     ylab="Gas consumption (cubic feet)", cex.lab = 1.6,
     axes = FALSE)
axis(1, lwd = 0.7)
axis(2, lwd = 0.7, las = 1)
box(lwd = 0.7)
imp <- mice(data, m=1, maxit=0)
pred <- imp$pred
pred["Gas","Insul"] <- 0
betadump <- vector("list", 0)
imp <- mice(data, m=5, pred=pred, meth="normdump", maxit=1, print=FALSE, seed=83126)
abline(m1<-lm(Gas~Temp, data=data, na.action=na.omit), col=mdc(4))
betadump <- matrix(betadump, nc=2, byrow=TRUE)
for (i in 1:5) abline(coef=unlist(betadump[i,]), col=mdc(5))
points(rep(5,5),imp$imp$Gas, lwd=lwd, col=mdc(2),pch=19)
text(x=9, y=6.5, label="d",cex=2)

pch <- c(rep(3,26),rep(1,30))
plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd, pch=pch,
     xlab=expression(paste("Temperature (", degree, "C)")),
     ylab="Gas consumption (cubic feet)", cex.lab = 1.6,
     axes = FALSE)
axis(1, lwd = 0.7)
axis(2, lwd = 0.7, las = 1)
box(lwd = 0.7)
imp <- mice(data, m=5, meth="norm", maxit=1, print=FALSE, seed=11727)
abline(m1<-lm(Gas~Temp, data=data, na.action=na.omit, subset=Insul=="Before"), col=mdc(4))
abline(m2<-lm(Gas~Temp, data=data, na.action=na.omit, subset=Insul=="After"), col=mdc(4))
points(rep(5,5),imp$imp$Gas, lwd=lwd, col=mdc(2),pch=19)
legend(x="bottomleft", legend=c("before insulation","after insulation"), pch=c(3,1),bty="n", pt.lwd=lwd, cex = 1.6, pt.cex = 1, y.intersp = 0.6)
text(x=9, y=6.5, label="e",cex=2)

pch <- c(rep(3,26),rep(1,30))
plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd, pch=pch,
     xlab=expression(paste("Temperature (", degree, "C)")),
     ylab="Gas consumption (cubic feet)", cex.lab = 1.6,
     axes = FALSE)
axis(1, lwd = 0.7)
axis(2, lwd = 0.7, las = 1)
box(lwd = 0.7)
betadump <- vector("list", 0)
imp <- mice(data, m=5, meth="pmmdump", maxit=1, print=FALSE, seed=68006)
betadump <- matrix(betadump, nc=3, byrow=TRUE)
m1<-lm(Gas~Temp+Insul, data=data, na.action=na.omit)
an <- coef(m1)[1]
ai <- an + coef(m1)[3]
b <- coef(m1)[2]
abline(a=ai, b=b, col=mdc(4))
abline(a=an, b=b, col=mdc(4))
# for (i in 1:1) {
#   abline(a=unlist(betadump[i,1]), b=unlist(betadump[i,2]), col=mdc(5))
#   abline(a=unlist(betadump[i,1])+unlist(betadump[i,3]), b=unlist(betadump[i,2]), col=mdc(5))
# }
# points(rep(5,5),imp$imp$Gas, lwd=lwd, col=mdc(2), pch=20)
eta <- 0.6
ylo <- ai+b*(5-eta)
yhi <- ai+b*(5+eta)
lines(x=c(5-eta,5+eta),y=c(ylo,yhi),lwd=3,col=mdc(5))
xlo <- (ylo-an)/b
xhi <- (yhi-an)/b
lines(x=c(xlo,xhi),y=c(ylo,yhi),lwd=3,col=mdc(5))

donors <- subset(data, (Insul=="After"&Temp>5-eta&Temp<5+eta)
                 |    (Insul=="Before"&Temp>xlo&Temp<xhi))
points(x=donors$Temp, y=donors$Gas, cex=1.8, col=mdc(5), lwd=lwd)
legend(x="bottomleft", legend=c("before insulation","after insulation"), pch=c(3,1),bty="n", pt.lwd=lwd, cex = 1.6, pt.cex = 1, y.intersp = 0.6)
text(x=9, y=6.5, label="f",cex=2)

## ----generate1-----------------------------------------------------------
set.seed(1)
n <- 10000
sigma <- matrix(c(1, 0.6, 0.6, 1), nrow = 2)
cmp <- MASS::mvrnorm(n = n, mu = c(5, 5), Sigma = sigma)
p2.marright <- 1 - plogis(-5 + cmp[, 1])
r2.marright <- rbinom(n, 1, p2.marright)
yobs <- cmp
yobs[r2.marright == 0, 2] <- NA

## ----generateplot1,  echo=FALSE, solo=TRUE, fig.width=4.5, fig.height=2.5----
grid <- seq(0,10,0.1)
mr <- plogis(-5 + grid)
mm <- plogis( 0.75-abs(grid-5))
mt <- plogis(-0.75+abs(grid-5))

z <- data.frame(grid, mr, mm, mt)
matplot(x=z[,1],y=z[,2:4], type="l", lty=1:3, col=mdc(5) , lwd=2,
        xlab=expression(italic(Y)[1]),
        ylab=expression(paste("Missingness in", italic(Y)[2])), las=1,
     axes = FALSE)
axis(1, lwd = 0.7)
axis(2, lwd = 0.7, las = 1)
box(lwd = 0.7)
legend(x="top", bty="n",
       legend=c("MARRIGHT","MARMID","MARTAIL"), lty=1:3, lwd=2, col=mdc(5), cex=0.8)

## ----generateplot2,  echo=FALSE, solo=TRUE, fig.width=6, fig.height=4----
library(lattice)
y <- cmp
y3 <- rbind(y,y,y)
p2.marmid <- 1 - plogis(0.75-abs(y[,1]-5))
p2.martail <- 1 - plogis(0.75+abs(y[,1]-5))
r2.marmid <- rbinom(n, 1, p2.marmid)
r2.martail <- rbinom(n, 1, p2.martail)
r2 <- c(r2.marright,r2.marmid,r2.martail)
r2 <- factor(r2, labels=c("Ymis","Yobs"))
typ <- factor(rep(3:1,each=n),labels=c("MARTAIL","MARMID","MARRIGHT"))
d <- data.frame(y1=y3[,1],y2=y3[,2],r2=r2,typ=typ)
trellis.par.set(box.rectangle=list(col=c(mdc(2),mdc(1)),lwd=1.2))
trellis.par.set(box.umbrella=list(col=c(mdc(2),mdc(1)),lwd=1.2))
trellis.par.set(plot.symbol=list(col="transparent",lwd=1))
tp <- bwplot(r2~y2|typ, data=d,
             horizontal=TRUE, layout=c(1,3),
             xlab=expression(italic(Y)[2]),
             col=c(mdc(2),mdc(1)),strip=FALSE, xlim=c(2,8),
             strip.left = strip.custom(bg="grey95"))
print(tp)

## ----ampute--------------------------------------------------------------
amp <- ampute(cmp, type = "RIGHT")
apply(amp$amp, 2, mean, na.rm = TRUE)

## ----lingamlss, echo = FALSE---------------------------------------------
mice.impute.TF <- function(y, ry, x,
                  gamlss.trace = FALSE, ...)
{
  # prepare data
  xobs <- x[ry, , drop = FALSE]
  xmis <- x[!ry, , drop = FALSE]
  yobs <- y[ry]
  n1 <- sum(ry)
  n0 <- sum(!ry)

  # draw bootstrap sample
  s <- sample(n1, n1, replace = TRUE)
  dotxobs <- xobs[s, , drop = FALSE]
  dotyobs <- yobs[s]
  row.names(dotxobs) <- row.names(dotyobs) <- NULL
  dotxy <- data.frame(dotxobs, y = dotyobs)
  xmis <- data.frame(xmis)

  # fit the gamlss model
  fit <- gamlss(y ~ ., data = dotxy, family = TF,
                trace = gamlss.trace, ...)
  yhat <- predict(fit, data = dotxy, newdata = xmis)
  sigma <- exp(coef(fit, "sigma"))
  nu <- exp(coef(fit, "nu"))

  # draw the imputations
  return(rTF(n0, yhat, sigma, nu))
}

## ----linhc0, echo=FALSE, results="hide"----------------------------------
library(gamlss)
data(db)

## ----linhc1, echo=FALSE, duo = TRUE,  fig.width=4.5, fig.height=2.25-----
#allboys <- read.table(file="~/Documents/Sync/Impute/mice/data/db7482-2.txt", header=TRUE, sep="\t")[,1:9]
if (empty_figure) {
  plot.new()
  plot.new()
  } else {
db <- as.data.frame(db)
data <- subset(db, age > 1 & age < 2, c("age","head"))
names(data) <- c("age", "hc")
truehist(data$hc, col = mdc(1), border = "white",
         xlab="Head circumference (cm)",
         ylab="Density",
         ylim=c(0,0.3), xlim=c(38,60),
         h = 0.5, axes = FALSE)
axis(1, lwd = 0.7, cex.axis = 0.7)
axis(2, lwd = 0.7, las = 1, cex.axis = 0.7)

mn <- gamlss(hc~1, data=na.omit(data), family=NO, trace=FALSE)
mu <- coef(mn)
sigma <- exp(coef(mn, "sigma"))
cmfine <- seq(38,60,0.1)
lines(cmfine, dNO(cmfine, mu, sigma), lwd=0.8, lty=2)
mt <- gamlss(hc~1, data=data, family=TF, trace=FALSE)
mu <- coef(mt)
sigma <- exp(coef(mt, "sigma"))
nu <- exp(coef(mt, "nu"))
lines(cmfine, dTF(cmfine, mu, sigma, nu), lwd=0.8, lty=1)
legend(x="right",legend=c("normal","t, df=6.7"),
       lwd=0.8, lty=c(2,1), bty="n", cex=0.7)
plot(x=data$age, y=data$hc, col=mdc(1), cex=0.3,
     xlab="Age (in years)",
     ylab="Head circumference (cm)",
     ylim=c(39,60), axes = FALSE)
axis(1, lwd = 0.7, cex.axis = 0.7)
axis(2, lwd = 0.7, las = 1, cex.axis = 0.7)
box(lwd = 0.7)
}

## ----linhc2, cache=TRUE, warning = FALSE---------------------------------
library("ImputeRobust")
library("gamlss")
data(db)
data <- subset(db, age > 1 & age < 2, c("age", "head"))
names(data) <- c("age", "hc")
synthetic <- rep(c(FALSE, TRUE), each = nrow(data))
data2 <- rbind(data, data)
row.names(data2) <- 1:nrow(data2)
data2[synthetic, "hc"] <- NA
imp <- mice(data2, m = 1, meth = "gamlssTF", seed = 88009,
            print = FALSE)
syn <- subset(mice::complete(imp), synthetic)

## ----linhc3, echo=FALSE, duo = TRUE,  fig.width=4.5, fig.height=2.25-----
if (empty_figure) {
  plot.new()
  plot.new()
  } else {
data <- syn
truehist(data$hc, col=mdc(2), border="white",
         xlab="Head circumference (cm)",
         ylab="Density",
         ylim=c(0,0.3), xlim=c(38,60),
         h=0.5, axes = FALSE)
axis(1, lwd = 0.7, cex.axis = 0.7)
axis(2, lwd = 0.7, las = 1, cex.axis = 0.7)

mn <- gamlss(hc~1, data=na.omit(data), family=NO, trace=FALSE)
mu <- coef(mn)
sigma <- exp(coef(mn, "sigma"))
cmfine <- seq(38,60,0.1)
lines(cmfine, dNO(cmfine, mu, sigma), lwd=0.8, lty=2)
mt <- gamlss(hc~1, data=data, family=TF, trace=FALSE)
mu <- coef(mt)
sigma <- exp(coef(mt, "sigma"))
nu <- exp(coef(mt, "nu"))
lines(cmfine, dTF(cmfine, mu, sigma, nu), lwd=0.8, lty=1)
legend(x="right",legend=c("normal",paste("t, df=",round(nu,1),sep="")),
       lwd=0.8, lty=c(2,1), bty="n", cex=0.7)
plot(x=data$age, y=data$hc, col=mdc(2), cex=0.3,
     xlab="Age (in years)",
     ylab="Head circumference (cm)",
     ylim=c(39,60), axes = FALSE)
axis(1, lwd = 0.7, cex.axis = 0.7)
axis(2, lwd = 0.7, las = 1, cex.axis = 0.7)
box(lwd = 0.7)
}

## ----misspecify, echo=FALSE, duo = TRUE,  fig.height=2.25, fig.width=4.5----
data <- boys[boys$age<=2,c("age","bmi")]
set.seed(87120)
data[sample(92:136,10),"bmi"] <- NA
imp <- mice(data, meth="norm", m=1, seed=32212, print=FALSE)
cd1 <- mice::complete(imp)
imp <- mice(data, m=1, seed=32212, print=FALSE)
cd2 <- mice::complete(imp)
r <- !is.na(data$bmi)
plot(data, col=mdc(1), xlab="Age", ylab="BMI", cex = 0.7, axes = FALSE)
axis(1, lwd = 0.7, cex.axis = 0.7)
axis(2, lwd = 0.7, las = 1, cex.axis = 0.7)
box(lwd = 0.7)
points(cd1[!r,], col=mdc(2), pch=19, cex=0.7)
plot(data, col=mdc(1), xlab="Age", ylab="BMI", cex = 0.7, axes = FALSE)
axis(1, lwd = 0.7, cex.axis = 0.7)
axis(2, lwd = 0.7, las = 1, cex.axis = 0.7)
box(lwd = 0.7)
points(cd2[!r,], col=mdc(2), pch=19, cex=0.7)

## ----pmmfigure, echo=FALSE, solo=TRUE, fig.height=3.5, fig.width=3.5-----
data <- whiteside
lwd <- 1.5
data[47,"Gas"] <- NA

pch <- c(rep(3,26),rep(1,30))
plot(x=data$Temp, y=data$Gas, col=mdc(1), lwd=lwd, pch=pch,
     xlab=expression(paste("Temperature (", degree, "C)")),
     ylab="Gas consumption (cubic feet)", axes = FALSE)
axis(1, lwd = 0.7, cex.axis = 1)
axis(2, lwd = 0.7, las = 1, cex.axis = 1)
box(lwd = 0.7)
betadump <- vector("list", 0)
imp <- mice(data, m=5, meth="pmmdump", maxit=1, print=FALSE, seed=68006)
betadump <- matrix(unlist(betadump), nc=3, byrow=TRUE)
m1<-lm(Gas~Temp+Insul, data=data, na.action=na.omit)
an <- coef(m1)[1]
ai <- an + coef(m1)[3]
b <- coef(m1)[2]
abline(a=ai, b=b, col=mdc(4))
abline(a=an, b=b, col=mdc(4))
# for (i in 56:56) {
#    abline(a=unlist(betadump[i,1]), b=unlist(betadump[i,2]), col=mdc(5))
#    abline(a=unlist(betadump[i,1])+unlist(betadump[i,3]), b=unlist(betadump[i,2]), col=mdc(5))
# }
# points(rep(5,5),imp$imp$Gas, lwd=lwd, col=mdc(2), pch=20)
eta <- 0.6
ylo <- ai+b*(5-eta)
yhi <- ai+b*(5+eta)
lines(x=c(5-eta,5+eta),y=c(ylo,yhi),lwd=3,col=mdc(4))
an <- 7.05; ai<-an-1.7; b <- -0.38
xlo1 <- (ylo-ai)/b
xhi1 <- (yhi-ai)/b
xlo2 <- (ylo-an)/b
xhi2 <- (yhi-an)/b
abline(a=an, b=b, col=mdc(5))
abline(a=ai, b=b, col=mdc(5))
lines(x=c(xlo1,xhi1),y=c(ylo,yhi),lwd=3,col=mdc(5))
lines(x=c(xlo2,xhi2),y=c(ylo,yhi),lwd=3,col=mdc(5))
abline(v=c(5-eta,5+eta),h=c(ylo,yhi),col=mdc(4),lty=3)
rect(xlo1,0,xhi1,8,col=hcl(0,100,40,0.05),border=NA)
rect(xlo2,0,xhi2,8,col=hcl(0,100,40,0.05),border=NA)

donors <- subset(data, (Insul=="After"&Temp>xlo1&Temp<xhi1)
                 |    (Insul=="Before"&Temp>xlo2&Temp<xhi2))
points(x=donors$Temp, y=donors$Gas, cex=1.8, col=mdc(5), lwd=lwd)
legend(x="bottomleft", legend=c("before insulation","after insulation"), pch=c(3,1),bty="n", pt.lwd=lwd)


## ----pmmsim, eval=FALSE, echo=FALSE--------------------------------------
simulate <- function(nsim=10, seed=41872){
  set.seed(seed)
  res <- array(NA,dim=c(3, nsim, 6))
  for(i in 1:nsim){
    data <- createdata()
    data <- makemissing(data)
    res[1,i,] <- test.impute(data, m=5)
    res[2,i,] <- test.impute(data, m=5, donors=1)
    res[3,i,] <- test.impute(data, m=5, donors=10)
  }
  return(res)
}
res.pmm <- simulate(10000)
res <- res.pmm

## ----cart, echo=FALSE,  duo=TRUE, fig.width=4.5, fig.height=2.25---------
library("rpart")

fit <- rpart(Gas ~ Temp + Insul, data=whiteside)
plot(fit, branch=0, margin=0.15)
text(fit, use=T,pretty=0,dig=3,cex=0.8)

leaf <- row.names(fit$frame)[fit$where]
label <- factor(leaf, labels=c(2,3,1,4,5))
plot(x=whiteside$Temp, y=whiteside$Gas, type="n",
     xlab=expression(paste("Temperature (", degree, "C)")),
     ylab="Gas consumption (cubic feet)", axes = FALSE)
axis(1, lwd = 0.7, cex.axis = 0.8)
axis(2, lwd = 0.7, las = 1, cex.axis = 0.8)
box(lwd = 0.7)
text(x=whiteside$Temp, y=whiteside$Gas, label=label, col=mdc(4), cex=0.6)

## ----echo=FALSE, eval=FALSE----------------------------------------------
# library(survival)
# library(foreign)
# library(car)
# file <- "~/Documents/Sync/Impute/ice/datasets/breastcancer.dta"
# breastcancer <- read.dta(file)
#
# data <- breastcancer
# nstage <- recode(data$nodes, '0=0;1:3=1;4:9=2;10:hi=3;else=NA')
# lonodes <- 0.5*(nstage==1) + 3.5*(nstage==2) + 9.5*(nstage==3)
# upnodes <- 3.5*(nstage==1) + 9.5*(nstage==2) + 55*(nstage==3)
# llnodes <- log(lonodes)
# lunodes <- log(upnodes)
# lnodes <- as.numeric(NA)
# data <- data.frame(data[,c("size","gradd1","gradd2","nodes")], nstage, lonodes, upnodes, lnodes)
# imp <- mice(data)
#
# fit <- survreg(Surv(lonodes, upnodes, rep(3, nrow(data)),type="interval") ~ size + gradd1 + gradd2, data=data)
#

## ----c85sensfig, duo = TRUE, echo=FALSE, fig.width=4.5, fig.height=2.25----
s <- c(
       100, 0.015, 0.058, 0.02, 0.35,
       110, 0.024, 0.074, 0.03, 0.30,
       120, 0.043, 0.103, 0.05, 0.25,
       130, 0.091, 0.164, 0.10, 0.20,
       140, 0.145, 0.185, 0.15, 0.15,
       150, 0.307, 0.247, 0.30, 0.10,
       160, 0.157, 0.099, 0.15, 0.08,
       170, 0.107, 0.049, 0.10, 0.06,
       180, 0.055, 0.016, 0.05, 0.04,
       190, 0.033, 0.005, 0.03, 0.02,
       200, 0.023, 0.000, 0.02, 0.00,
       210, 0.023, 0.000, 0.02, 0.00
       )
sm <- matrix(s, nr=12, nc=5, byrow=TRUE)
snug <- 1.5
xx <- cbind(sm[,1]-5+snug,
        sm[,1]-5-snug,
        sm[,1]-5)
matplot(x=xx[,3], y=cbind(1-sm[,5]),
        col=mdc(1), type="p", lwd=2, lty=1, pch=20,
        xlab="Systolic BP (mmHg)",
        ylab="Observation probability",
        axes = FALSE)
axis(1, lwd = 0.7, cex.axis = 0.8)
axis(2, lwd = 0.7, las = 1, cex.axis = 0.8)
box(lwd = 0.7)
matplot(x=xx, y=sm[,2:4], type="s",
        col=c(mdc(4),mdc(5),mdc(6)), lwd=2, lty=1,
        xlab="Systolic BP (mmHg)",
        ylab="Density", axes = FALSE)
axis(1, lwd = 0.7, cex.axis = 0.8)
axis(2, lwd = 0.7, las = 1, cex.axis = 0.8)
box(lwd = 0.7)


## ----ch4, child = "src/ch4.Rnw"------------------------------------------

## ----init4, echo = FALSE, results = 'hide'-------------------------------
opts_chunk$set(fig.path = 'fig/ch04-', self.contained = FALSE)
pkg <- c("mice", "lattice")
loaded <- sapply(pkg, require, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
suppressPackageStartupMessages(library(gam, warn.conflicts = FALSE, quietly = TRUE))
source("R/functions.R")

## ----patterns, echo=FALSE------------------------------------------------
data <- matrix(sample(1:100,4*8*3,replace=TRUE),nrow=8*4,
                dimnames=list(NULL,c("A","B","C")))
data <- as.data.frame(data)
data[c(31:32),"A"] <- NA
data[c(15:16,22:24,30:32),"B"] <- NA
data[c(6:8,12:16,17:21,27:29),"C"] <- NA
mdpat <- cbind(expand.grid(rec = 8:1, pat = 1:4, var = 1:3), r=as.numeric(as.vector(is.na(data))))
pattern1 <- data[1:8,]
pattern2 <- data[9:16,]
pattern3 <- data[17:24,]
pattern4 <- data[25:32,]
types <-  c("Univariate","Monotone","File matching","General")
levelplot(r~var+rec|as.factor(pat), data=mdpat,
            as.table=TRUE, aspect="iso",
            shrink=c(0.9),
            col.regions = mdc(1:2),
            colorkey=FALSE,
            scales=list(draw=FALSE),
            xlab="", ylab="",
            between = list(x=1,y=0),
            strip = strip.custom(bg = "grey95", style = 1,
                                 factor.levels = types))

## ----pattern-------------------------------------------------------------
md.pattern(pattern4, plot = FALSE)

## ----mdpairs-------------------------------------------------------------
p <- md.pairs(pattern4)
p

## ----puc-----------------------------------------------------------------
p$mr/(p$mr+p$mm)

## ----outbound------------------------------------------------------------
p$rm/(p$rm+p$rr)

## ----figfourflux, echo=FALSE,  four=TRUE, results='hide', fig.width=4.5, fig.height=4.5----
library("MASS")
fluxplot(pattern1, main="", xlim=c(-0.1,1.1), ylim=c(-0.1,1.1), lwd = 0.8)
text(x=0.5,y=1,label="Univariate")
fluxplot(pattern2, main="", xlim=c(-0.1,1.1), ylim=c(-0.1,1.1), lwd = 0.8)
text(x=0.5,y=1,label="Monotone")
fluxplot(pattern3, main="", xlim=c(-0.1,1.1), ylim=c(-0.1,1.1), lwd = 0.8)
text(x=0.5,y=1,label="File matching")
fluxplot(pattern4, main="", xlim=c(-0.1,1.1), ylim=c(-0.1,1.1), lwd = 0.8)
text(x=0.5,y=1,label="General")

## ----flux----------------------------------------------------------------
flux(pattern4)[,1:3]

## ----mon-----------------------------------------------------------------
data <- nhanes2[, 1:3]
md.pattern(data, plot = FALSE)
imp <- mice(data, visit = "monotone", maxit = 1, m = 2,
            print = FALSE)

## ----monda---------------------------------------------------------------
where <- make.where(nhanes2, "none")
where[6, "hyp"] <- TRUE
where[c(3, 6), "bmi"] <- TRUE
imp1 <- mice(nhanes2, where = where, method = "sample",
             seed = 21991, maxit = 1, print = FALSE)
data <- mice::complete(imp1)
imp2 <- mice(data, maxit = 1, visitSequence = "monotone",
             print = FALSE)

## ----slow1---------------------------------------------------------------
generate <- function(n = c(1000, 4500, 4500, 0),
                     cor = matrix(c(1.0, 0.9, 0.9,
                                    0.9, 1.0, 0.7,
                                    0.9, 0.7, 1.0), nrow = 3)) {
  require(MASS)
  nt <- sum(n)
  cs <- cumsum(n)
  data <- mvrnorm(nt, mu = rep(0,3), Sigma = cor)
  dimnames(data) <- list(1:nt, c("X", "Y1", "Y2"))
  if (n[2] > 0) data[(cs[1]+1):cs[2],"Y1"] <- NA
  if (n[3] > 0) data[(cs[2]+1):cs[3],"Y2"] <- NA
  if (n[4] > 0) data[(cs[3]+1):cs[4],c("Y1","Y2")] <- NA
  return(data)
}

## ----slow2---------------------------------------------------------------
impute <- function(data, m = 5, method = "norm",
                   print = FALSE, maxit = 10, ...) {
  statistic <- matrix(NA, nrow = maxit, ncol = m)
  for (iter in 1:maxit) {
    if (iter==1) imp <- mice(data, m = m, method = method,
                             print = print, maxit = 1, ...)
    else imp <- mice.mids(imp, maxit = 1, print = print, ...)
    statistic[iter, ] <- unlist(with(imp, cor(Y1, Y2))$analyses)
  }
  return(list(imp = imp, statistic = statistic))
}

## ----slow3---------------------------------------------------------------
simulate <- function(
  ns = matrix(c(1000, 500, 250, 100, 50, 0,
                rep(c(4500, 4750, 4875, 4950, 4975, 5000), 2),
                rep(0, 6)), nrow = 6),
  m = 5, maxit = 10, seed = 1, ...) {
  if (!missing(seed)) set.seed(seed)
  s <- cbind(rep(1:nrow(ns), each = maxit * m),
             apply(ns, 2, rep, each = maxit * m),
             rep(1:maxit, each = m), 1:m, NA)
  colnames(s) <- c("k", "n111", "n101", "n110", "n100",
                   "iteration", "m", "rY1Y2")
  for (k in 1:nrow(ns)) {
    data <- generate(ns[k, ], ...)
    r <- impute(data, m = m, maxit = maxit, ...)
    s[s[,"k"] == k, "rY1Y2"] <- t(r$statistic)
  }
  return(data.frame(s))
}

## ----slow4, cache = TRUE-------------------------------------------------
slow.demo <- simulate(maxit = 150, seed = 62771)

## ----slowplot, echo=FALSE, fig.height=5----------------------------------
labels <- c("90% missing", "95% missing", "97.5% missing",
            "99% missing", "99.5% missing", "100% missing")
xyplot(rY1Y2 ~ iteration | as.factor(k), group = m,
       data = slow.demo, layout = c(3,2),
       type="l", as.table = TRUE,
       ylab = "Correlation between Y1 and Y2",
       xlab = "Iteration", col = mdc(3),
       scales = list(y = list(alternating = 1, tck = c(1, 0))),
       strip = strip.custom(bg = "grey95", style = 1,
         factor.levels = labels))

## ----pubimpjm, eval=TRUE, cache=TRUE-------------------------------------
select <- with(boys, age >= 8 & age <= 21.0)
djm <- boys[select, -4]
djm$gen <- as.integer(djm$gen)
djm$phb <- as.integer(djm$phb)
djm$reg <- as.integer(djm$reg)
dfcs <- boys[select, -4]

## impute under jm and fcs
jm <- mice(djm, method = "norm", seed = 93005, m = 10,
           print = FALSE)
pmm <- mice(djm, method = "pmm", seed = 71332, m = 10,
            print = FALSE)
fcs <- mice(dfcs, seed = 81420, m = 10, print = FALSE)

## ----pubfigjm,  echo=TRUE, fig.width=6, fig.height=5--------------------
xyplot(jm, gen ~ age | as.factor(.imp), subset = .imp < 6,
       xlab = "Age", ylab = "Genital stage", col = mdc(1:2),
       ylim = c(0, 6))

## ----pubfigfcs,  echo=FALSE, fig.width=6, fig.height=5-------------------
xyplot(fcs, gen ~ age | as.factor(.imp), subset = .imp < 6,
       xlab = "Age", ylab = "Genital stage", col = mdc(1:2),
       ylim = c(0, 6))

## ----pubfig2,  echo=FALSE, cache=TRUE------------------------------------
drawgen <- function(imp, var="gen", doround=FALSE) {
  m <- imp$m
  ires <- vector("list", m)
  for (i in 1:m){
    cda <- mice::complete(imp,i)
    if (doround) cda[,var] <- round(squeeze(cda[,var],c(1,5)))
    count.boys(cda)
    ires[[i]] <-fitTannerStages(data=cda,var=var,nstages=5,counts=boy.count.gen)
  }

  ## plot the fitted models for the CCA and imputed analyses
  plot(c(8,21),c(0,1),type="n",xlab="Age (years)",
       ylab="Proportion",lab=c(10,12,5), las=1,cex=1,tck=-0.01)
  abline(0.1,0,lty=2)
  abline(0.5,0,lty=2)
  abline(0.9,0,lty=2)
  x <- seq(8.75,20.5,0.25)
  for (s in 1:4) {
    lines(x=x,y=boys.gen.cc[[s]][[2]]$predicted,lwd=6,col=mdc(1))
    for (i in 1:m) {
      lines(x=x,y=ires[[i]][[s]][[3]]$predicted,lwd=1,col=mdc(2))
    }
  }
}

four.gen <- function(formula=gen~age|reorder(factor(c("JM: multivariate normal","JM: rounded","FCS: predictive mean matching","FCS: proportional odds")),1:4),
                     data=list(jm=jm, jmr=jm, pmm=pmm, fcs=fcs),
                     doround=FALSE, layout=c(1,4),
                     xlab="Age (years)", ylab="Proportion",
                     as.table=TRUE,
                     ...) {

  prepanel.gen <- function(x, y, ...){
    list(xlim=c(9,20), ylim=c(0,1))
  }

  panel.gen <- function(x, y, ...){
    pan <- panel.number()
    doround <- ifelse(pan==2, TRUE, FALSE)
    imp <- data[[pan]]
    m <- imp$m
    ires <- vector("list", m)
    for (i in 1:m){
      cda <- mice::complete(imp,i)
      if (doround) cda[,"gen"] <- round(squeeze(cda[,"gen"],c(1,5)))
      count.boys(cda)
      ires[[i]] <-fitTannerStages(data=cda,var="gen",nstages=5,counts=boy.count.gen)
    }
    panel.grid(h=-1, v=-1)
    # panel.abline(0.1,0,lty=2,...)
    # panel.abline(0.5,0,lty=2,...)
    # panel.abline(0.9,0,lty=2,...)
    x <- seq(8.75,20.5,0.25)
    for (s in 1:4) {
      panel.lines(x=x,y=boys.gen.cc[[s]][[2]]$predicted,lwd=4,col=mdc(1))
      for (i in 1:m) {
        panel.lines(x=x,y=ires[[i]][[s]][[3]]$predicted,lwd=1,col=mdc(2))
      }
    }
  }


  gen <- 0; age <- 0; x <- 0

  ## complete cases
  idat <- data[[1]]$data
  idat <- idat[cci(idat),]
  count.boys(idat)
  boys.gen.cc <-fitTannerStages(data=idat,var="gen",nstages=5,counts=boy.count.gen)

  tp <- xyplot(formula, data, layout=layout,
               prepanel=prepanel.gen, panel=panel.gen,
               xlab=xlab, ylab=ylab, as.table=as.table,
               ...)
  return(tp)
}

tp<-four.gen(layout=c(2,2),ylab="",strip=function(...,bg) strip.default(..., bg="grey95"))
print(tp)

## ----ex.slow1, eval=FALSE------------------------------------------------
slow2 <- simulate(ns = ns2, maxit = 50, seed = 62771)

## ----ch5, child = "src/ch5.Rnw"------------------------------------------

## ----init5, echo=FALSE, results='hide'-----------------------------------
opts_chunk$set(fig.path = 'fig/ch05-', self.contained = FALSE)
pkg <- c("mice", "magrittr", "purrr", "dplyr")
loaded <- sapply(pkg, require, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)

## ----workflow1, warning = FALSE------------------------------------------
# mids workflow using saved objects
library(mice)
imp <- mice(nhanes, seed = 123, print = FALSE)
fit <- with(imp, lm(chl ~ age + bmi + hyp))
est1 <- pool(fit)

## ----workflow2-----------------------------------------------------------
# mids workflow using pipes
library(magrittr)
est2 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  with(lm(chl ~ age + bmi + hyp)) %>%
  pool()

## ----workflow3-----------------------------------------------------------
# mild workflow using base::lapply
est3 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("all") %>%
  lapply(lm, formula = chl ~ age + bmi + hyp) %>%
  pool()

## ----workflow4-----------------------------------------------------------
# mild workflow using pipes and base::Map
est4 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("all") %>%
  Map(f = lm, MoreArgs = list(f = chl ~ age + bmi + hyp)) %>%
  pool()

## ----workflow5, warning = FALSE------------------------------------------
# mild workflow using purrr::map
library(purrr)
est5 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("all") %>%
  map(lm, formula = chl ~ age + bmi + hyp) %>%
  pool()

## ----workflow6-----------------------------------------------------------
# long workflow using base::by
est6 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("long")  %>%
  by(as.factor(.$.imp), lm, formula = chl ~ age + bmi + hyp) %>%
  pool()

## ----workflow7-----------------------------------------------------------
# long workflow using a dplyr list-column
library(dplyr)
est7 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("long") %>%
  group_by(.imp) %>%
  do(model = lm(formula = chl ~ age + bmi + hyp, data = .)) %>%
  as.list() %>%
  .[[-1]] %>%
  pool()

## ----workflow8-----------------------------------------------------------
# incorrect workflow: averaging data, no pooling
ave <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("long") %>%
  group_by(.id) %>%
  summarise_all(.funs = mean) %>%
  select(-.id, -.imp)
est8 <- lm(formula = chl ~ age + bmi + hyp, data = ave)

## ----corave--------------------------------------------------------------
cor(ave)

## ----corimp--------------------------------------------------------------
cor <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("all") %>%
  lapply(cor)
Reduce("+", cor) / length(cor)

## ----stack---------------------------------------------------------------
est9 <- nhanes2 %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("long") %>%
  lm(formula = chl ~ age + bmi + hyp)

## ----repeatedanalyses----------------------------------------------------
fit <- with(imp, lm(chl~bmi+age))
coef(fit$analyses[[1]])
coef(fit$analyses[[2]])

## ----poolstandard--------------------------------------------------------
est <- pool(fit)
summary(est)

## ----frequency-----------------------------------------------------------
expr <- expression(freq <- table(hyp), freq[1] - freq[2])
fit <- with(imp, eval(expr))
unlist(fit$analyses)

## ----d1-1----------------------------------------------------------------
imp <- mice(nhanes2, m = 10, print = FALSE, seed = 71242)
m2 <- with(imp, lm(chl ~ age + bmi))
pool(m2)

## ----d1-2----------------------------------------------------------------
m1 <- with(imp, lm(chl ~ bmi))
summary(D1(m2, m1))

## ----d1-3----------------------------------------------------------------
D2(m2, m1)

## ----d3-1----------------------------------------------------------------
D3(m2, m1)

## ----select3, cache = TRUE, results = 'hide'-----------------------------
data <- boys[boys$age >= 8, -4]
imp <- mice(data, seed = 28382, m = 10, print = FALSE)
scope <- list(upper = ~ age + hgt + wgt + hc + gen + phb + reg,
              lower = ~1)
expr <- expression(f1 <- lm(tv ~ 1),
                   f2 <- step(f1, scope = scope))
fit <- with(imp, expr)

## ----select3votes--------------------------------------------------------
formulas <- lapply(fit$analyses, formula)
terms <- lapply(formulas, terms)
votes <- unlist(lapply(terms, labels))
table(votes)

## ----select5-------------------------------------------------------------
fit.without <- with(imp, lm(tv ~ age + gen + reg + phb))
fit.with <- with(imp, lm(tv ~ age + gen + reg + phb + hgt))
D1(fit.with, fit.without)

## ----select6-------------------------------------------------------------
fit.without <- with(imp, lm(tv ~ age + gen + reg))
fit.with <- with(imp, lm(tv ~ age + gen + reg + phb))
D1(fit.with, fit.without)


## ----ch6, child = "src/ch6.Rnw"------------------------------------------


## ----init6, echo = FALSE, results = 'hide'-------------------------------
opts_chunk$set(fig.path = 'fig/ch6-', self.contained = FALSE)
pkg <- c("mice", "lattice", "gamlss", "smcfcs")
loaded <- sapply(pkg, require, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
suppressPackageStartupMessages(library(AGD, warn.conflicts = FALSE, quietly = TRUE))
source("R/mi.hist.R")

## ----pred1---------------------------------------------------------------
imp <- mice(nhanes, print = FALSE)
imp$predictorMatrix

## ----multi---------------------------------------------------------------
imp <- mice(cbind(nhanes, chl2 = 2 * nhanes$chl),
            print = FALSE, maxit = 1, m = 3, seed = 1)
imp$loggedEvents

## ----multi2--------------------------------------------------------------
imp <- mice(cbind(nhanes, chl2 = 2 * nhanes$chl),
            print = FALSE, maxit = 1, m = 3, seed = 1,
            remove.collinear = FALSE)
imp$loggedEvents

## ----chlchl,  solo = TRUE, fig.width =7, echo = FALSE, fig.height = 7----
xyplot(imp, chl2 ~ chl | as.factor(.imp), pch = 21)

## ----derived1, cache=TRUE------------------------------------------------
data <- boys[, c("age", "hgt", "wgt", "hc", "reg")]
imp <- mice(data, print = FALSE, seed = 71712)
long <- mice::complete(imp, "long", include = TRUE)
long$whr <- with(long, 100 * wgt / hgt)
imp.itt <- as.mids(long)

## ----derived2, cache = TRUE----------------------------------------------
data$whr <- 100 * data$wgt / data$hgt
imp.jav1 <- mice(data, seed = 32093, print = FALSE)

## ----derived3------------------------------------------------------------
tail(imp.jav1$loggedEvents, 3)

## ----derivedjavpred------------------------------------------------------
pred <- make.predictorMatrix(data)
pred[c("wgt", "whr"), c("wgt", "whr")] <- 0
pred[c("hgt", "whr"), c("hgt", "whr")] <- 0
pred
imp.jav2 <- mice(data, pred = pred, seed = 32093, print = FALSE)

## ----derived5, cache = TRUE----------------------------------------------
data <- boys[, c("age", "hgt", "wgt", "hc", "reg")]
data$whr <- 100 * data$wgt / data$hgt
meth <- make.method(data)
meth["whr"] <- "~I(100 * wgt / hgt)"
pred <- make.predictorMatrix(data)
pred[c("wgt", "hgt"), "whr"] <- 0
imp.pas <- mice(data, meth = meth, pred = pred,
                print = FALSE, seed = 32093)

## ----passive,  echo=FALSE, fig.width=7,fig.height=3.5------------------
if (empty_figure) {
  plot.new()
  } else {
c0 <- cbind(model = "Impute, then transform", mice::complete(imp.itt))
c1 <- cbind(model = "JAV", mice::complete(imp.jav2))
c2 <- cbind(model = "Passive imputation", mice::complete(imp.pas))
cd <- rbind(c0, c1, c2)
trellis.par.set(mice.theme())
tp <- xyplot(I(wgt / hgt * 100) ~ whr | model, data = cd, layout = c(3, 1),
             groups = rep(is.na(imp.itt$data$whr), 3), pch = c(21, 20), cex = c(0.3, 1),
             ylab = "Calculated Weight/Height (kg/m)", xlab = "Imputed Weight/Height (kg/m)")
print(tp)
}

## ----simratio------------------------------------------------------------
pop <- na.omit(boys[, c("age", "hgt", "wgt", "hc", "reg")])
pop$whr <- with(pop, 100 * wgt / hgt)
broom::tidy(lm(hc ~ age + hgt + wgt + whr, data = pop))

## ----ratiofcs, eval = FALSE----------------------------------------------
library(smcfcs)
data <- pop
data[sample(nrow(data), size = 100), "wgt"] <- NA
data[sample(nrow(data), size = 100), "hgt"] <- NA
data$whr <- 100 * data$wgt / data$hgt
meth <- c("", "norm", "norm", "", "", "norm")
imps <- smcfcs(originaldata = data, meth = meth, smtype = "lm",
               smformula = "hc ~ age + hgt + wgt + whr")
fit <- lapply(imps$impDatasets, lm,
              formula = hc ~ age + hgt + wgt + whr)
summary(pool(fit))

## ----inter1, cache = TRUE------------------------------------------------
expr <- expression((wgt - 40) * (hc - 50))
boys$wgt.hc <- with(boys, eval(expr))
meth <- make.method(boys)
meth["wgt.hc"] <- paste("~I(", expr, ")", sep = "")
meth["bmi"] <- ""
pred <- make.predictorMatrix(boys)
pred[c("wgt", "hc"), "wgt.hc"] <- 0
imp.int <- mice(boys, m = 1, meth = meth, pred = pred,
                print = FALSE, seed = 62587, maxit = 10)

## ----interaction, echo=FALSE, duo=TRUE, fig.width=4.5, fig.height=2.25--------
if (empty_figure) {
  plot.new()
  } else {
cd <- mice::complete(imp.int)

lwd <- 0.6
miss <- is.na(imp.int$data$wgt.hc)
plot(x=cd[!miss,c("wgt.hc","wgt")],col=mdc(1),lwd=0.8,cex=0.3,
     ylab="Weight (kg)", xlab="Interaction",pch=1,
     axes = FALSE)
points(x=cd[miss,c("wgt.hc","wgt")],col=mdc(2),lwd=0.8,cex=0.8,
       pch=20)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
box(lwd = lwd)

plot(x=cd[!miss,c("wgt.hc","hc")],col=mdc(1),lwd=0.8,cex=0.3,
     ylab="Head circumference (cm)", xlab="Interaction",
     axes = FALSE)
points(x=cd[miss,c("wgt.hc","hc")],col=mdc(2),lwd=0.8,cex=0.8,
       pch=20)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
box(lwd = lwd)
}

## ----compos1-------------------------------------------------------------
set.seed(43112)
n <- 400
Y1 <- sample(1:10, size = n, replace = TRUE)
Y2 <- sample(1:20, size = n, replace = TRUE)
Y3 <- 10 + 2 * Y1 + 0.6 * Y2 + sample(-10:10, size = n,
                                      replace = TRUE)
Y <- data.frame(Y1, Y2, Y3)
Y[1:100, 1:2] <- NA
md.pattern(Y, plot = FALSE)

## ----compos2-------------------------------------------------------------
Y123 <- Y1 + Y2 + Y3
Y12 <- Y123 - Y[,3]
P1 <- Y[,1] / Y12
data <- data.frame(Y, Y123, Y12, P1)

## ----compos3-------------------------------------------------------------
meth <- make.method(data)
meth["Y1"]  <- "~ I(P1 * Y12)"
meth["Y2"]  <- "~ I((1 - P1) * Y12)"
meth["Y12"] <- "~ I(Y123 - Y3)"
pred <- make.predictorMatrix(data)
pred["P1", ] <- 0
pred[c("P1"), c("Y12", "Y3")] <- 1
imp1 <- mice(data, meth = meth, pred = pred, m = 10,
             print = FALSE)

## ----compos4-------------------------------------------------------------
round(summary(pool(with(imp1, lm(Y3 ~ Y1 + Y2))))[, 1:2], 2)

## ----composition, echo=FALSE, fig.width=7, fig.height=4---------------------
meth <- make.method(data)
meth["Y1"] <- "~ I(P1 * Y12)"
meth["Y2"] <- "~ I((1 - P1) * Y12)"
meth["Y12"] <- "~ I(Y123 - Y3)"
pred <- make.predictorMatrix(data)
pred["P1", ] <- 0
pred[c("P1"), c("Y12", "Y3")] <- 1
imp1 <- mice(data, meth = meth, pred = pred, m = 1,
             print = FALSE)
pred["P1", "Y3"] <- 0
imp2 <- mice(data, meth = meth, pred = pred, m = 1,
             print = FALSE)
imp <- rbind(imp1, imp2)
model <- rep(c("Y12 and Y3", "Y12 only"),each=n)
tp <-xyplot(imp, P1~Y12|model, pch=c(1,19),
            xlab=expression(italic(Y)[1] + italic(Y)[2]),
            ylab = expression(italic(P)[1]),
            col=mdc(1:2),
            strip=strip.custom(factor.levels=c(expression(italic(Y)[12] + italic(Y)[3]),
                                               expression(italic(Y)[12]))))
plot(tp)

## ----squeezeprep, eval=TRUE, echo=TRUE, results='hide'-----------------------
data <- airquality[, 1:2]
post <- make.post(data)
post["Ozone"] <-
  "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(1, 200))"
imp <- mice(data, method = "norm.nob", m = 1,
            maxit = 1, seed = 1, post = post)

## ----squeeze, echo=FALSE, duo=TRUE, fig.width=4.5, fig.height=2.25----
lwd <- 1.5
data <- mice::complete(imp)
Yobs <- airquality[, "Ozone"]
Yimp <- data[, "Ozone"]
mi.hist(Yimp, Yobs, b=seq(-20, 200, 10), type = "continuous",
        gray = FALSE,
        obs.lwd = lwd, mis.lwd = lwd, imp.lwd = lwd,
        obs.col = mdc(4),mis.col = mdc(5), imp.col = "transparent",
        mlt = 0.08, main = "", xlab = "Ozone (ppb)")
box(lwd = 1)
plot(data[cci(imp),2:1],col=mdc(1),lwd=lwd,cex=0.75,
     ylab="Ozone (ppb)", xlab="Solar Radiation (lang)",ylim=c(-15,170),
     axes=FALSE)
points(data[ici(imp),2:1],col=mdc(2),lwd=lwd,cex=0.75)
axis(1, lwd = 1)
axis(2, lwd = 1, las = 1)
box(lwd = 1)

## ----doif----------------------------------------------------------------
post["Ozone"] <- "ifdo(c(Ozone < 1, Ozone > 200), c(1, 200))"

## ----post2, eval=TRUE, echo=FALSE, cache = TRUE--------------------------
post <- make.post(boys)
post["gen"] <-
  "imp[[j]][data$age[!r[, j]] < 8, i] <- levels(boys$gen)[1]"
post["phb"] <-
  "imp[[j]][data$age[!r[, j]] < 8, i] <- levels(boys$phb)[1]"
post["tv"]  <- "imp[[j]][data$age[!r[, j]] < 8, i] <- 1"
free <- mice(boys, m = 1, seed = 85444, print = FALSE)
restricted <- mice(boys, m = 1, post = post, seed = 85444,
                   print = FALSE)

## ----plotgen,  echo=FALSE, fig.width=7, fig.height=4---------------------
imp3 <- rbind(free, restricted)
model <- rep(c("Free","Restricted"),each=nrow(boys))
tp <-xyplot(imp3, gen~age|model, pch=c(3,1), cex=c(3,1.5),
            ylab="Genital development", xlab="Age (years)",
            col=mdc(1:2))
print(tp)

## ----visit1--------------------------------------------------------------
imp.int$visitSequence

## ----visit2a, echo=FALSE-------------------------------------------------
rm(boys)

## ----visit2--------------------------------------------------------------
vis <- c("hgt", "wgt", "hc", "wgt.hc", "gen", "phb",
         "tv", "reg")
expr <- expression((wgt - 40) * (hc - 50))
boys$wgt.hc <- with(boys, eval(expr))
imp.int2 <- mice(boys, m = 1, maxit = 1, visitSequence = vis,
                 meth = imp.int$meth, pred = imp.int$pred,
                 seed = 23390)

## ----visit3--------------------------------------------------------------
imp.int2 <- mice(boys, m = 1, maxit = 1, vis =  "monotone",
                 meth = imp.int$meth, pred = imp.int$pred,
                 seed = 23390)

## ----convergence1, echo = FALSE, cache = TRUE-----------------------------
imp <- mice(nhanes, seed = 62006, maxit = 20, print = FALSE)
print(plot(imp, lwd = 1))

## ----convergence0, eval = FALSE, echo = TRUE---------------------------------
imp <- mice(nhanes, seed = 62006, maxit = 20, print = FALSE)
plot(imp)

## ----convergence2, cache = TRUE------------------------------------------
meth <- make.method(boys)
meth["bmi"] <- "~I(wgt / (hgt / 100)^2)"
imp.bmi1 <- mice(boys, meth = meth, maxit = 20,
                 print = FALSE, seed = 60109)

## ----convergence3,  echo=FALSE-------------------------------------------
print(plot(imp.bmi1, c("hgt", "wgt", "bmi")))

## ----convergence4, cache = TRUE------------------------------------------
pred <- make.predictorMatrix(boys)
pred[c("hgt", "wgt"), "bmi"] <- 0
imp.bmi2 <- mice(boys, meth = meth, pred = pred, maxit = 20,
                 print = FALSE, seed = 60109)

## ----convergence5,  echo=FALSE-------------------------------------------
print(plot(imp.bmi2, c("hgt", "wgt", "bmi")))

## ----wormplot5, echo=FALSE, results = 'hide'-----------------------------
set.seed(24417)
rm(boys)
boys$wgt[sample(1:nrow(boys), nrow(boys) / 2)] <- NA
imp <- mice(boys, m = 1, seed = 53882, print = FALSE)
cd <- mice::complete(imp)[, -4]
isobs <- !is.na(boys$wgt)
cdobs <- cd[isobs, ]
cdmis <- cd[!isobs, ]
obs <- gamlss(wgt ~ age + hgt + hc + gen + phb + tv + reg, data = cdobs)
mis <- gamlss(wgt ~ age + hgt + hc + gen + phb + tv + reg, data = cdmis)

## ----wormplot6, solo=TRUE, echo=FALSE, fig.width = 4.4, fig.height=4.4----
wp.twin(obs, mis, xvar = NULL, xvar.column = 2, n.inter = 9,
        col1 = mdc(4), col2 = mdc(5), ylim = 0.9, cex = 0.7, pch = 1)

## ----stripplot, echo=FALSE, fig.width = 7, fig.height=5----------------
imp <- mice(nhanes, seed = 29981, print = FALSE)
stripplot(imp, pch = c(21, 20), cex = c(1, 1.5))

## ----stripplot1, eval=FALSE, echo=TRUE-----------------------------------
imp <- mice(nhanes, seed = 29981)
stripplot(imp, pch = c(21, 20), cex = c(1, 1.5))

## ----densityplot1, eval=FALSE, echo=TRUE---------------------------------
densityplot(imp, layout = c(3, 1))

## ----densityplot, solo=TRUE, echo=FALSE, fig.width = 7, fig.height=3----
densityplot(imp, layout = c(3, 1), col = c(mdc(4), rep(mdc(5), 5)))

## ----propensityplot1, eval = FALSE---------------------------------------
fit <- with(imp, glm(ici(imp) ~ age + bmi + hyp + chl,
                     family = binomial))
ps <- rep(rowMeans(sapply(fit$analyses, fitted.values)),
          imp$m + 1)
xyplot(imp, bmi ~ ps | as.factor(.imp),
       xlab = "Probability that record is incomplete",
       ylab = "BMI", pch = c(1, 19), col = mdc(1:2))

## ----propensityplot, solo=TRUE, echo=FALSE, fig.width = 7, fig.height=5----
fit <- with(imp, glm(ici(imp)~age+bmi+hyp+chl,family=binomial))
ps <- rep(rowMeans(sapply(fit$analyses, fitted.values)),imp$m+1)
tp <- xyplot(imp, bmi~ps|as.factor(.imp), pch=c(1,19),
             xlab="Probability that record is incomplete",
             ylab="BMI", col=mdc(1:2))
print(tp)

## ----ch7, child = "src/ch7.Rnw"------------------------------------------

## ----init7, echo = FALSE, results = 'hide'-------------------------------
opts_chunk$set(fig.path = 'fig/ch7-', self.contained = FALSE)
pkg <- c("mice", "micemd", "lme4", "tidyr",
         "dplyr", "purrr", "gridExtra")
loaded <- sapply(pkg, require, character.only = TRUE,
                 warn.conflicts = FALSE, quietly = TRUE)
suppressPackageStartupMessages(library(miceadds, warn.conflicts = FALSE, quietly = TRUE))
suppressPackageStartupMessages(library(DPpackage, warn.conflicts = FALSE, quietly = TRUE))
suppressPackageStartupMessages(library(mitml, warn.conflicts = FALSE, quietly = TRUE))


## ----mla.data0-----------------------------------------------------------
library(mice)
data("brandsma", package = "mice")
d <- brandsma[, c("sch", "lpo", "sex", "den")]

## ----mla.data1-----------------------------------------------------------
d <- brandsma[, c("sch", "lpo")]

## ----mla.data2-----------------------------------------------------------
md.pattern(d, plot = FALSE)

## ----mla.empty1, cache=TRUE----------------------------------------------
library(miceadds)
methods <- c("sample", "pmm", "2l.pan", "2l.norm", "2l.pmm")
result <- vector("list", length(methods))
names(result) <- methods
for (meth in methods) {
  d <- brandsma[, c("sch", "lpo")]
  pred <- make.predictorMatrix(d)
  pred["lpo", "sch"] <- -2
  result[[meth]] <- mice(d, pred = pred, meth = meth,
                         m = 10, maxit = 1,
                         print = FALSE, seed = 82828)
}

## ----mlsd, fig.width=4.5, fig.height=2.25, solo=TRUE, echo = FALSE----
group_by(d, sch) %>%
  summarise(sdg = sd(lpo, na.rm = TRUE)) %>%
  pull(sdg) %>%
  hist(main = "", xlab = "SD(language score) per school")

## ----mldist, echo = FALSE, fig.width = 6, fig.height = 8, solo = TRUE----
imp <- result[[1]]
imp <- cbind(imp, mis = is.na(imp$data$lpo))
yobs <- mice::complete(imp, "long") %>%
  filter(.imp == 1) %>%
  group_by(sch) %>%
  mutate(
    nm = sum(mis),
    ng = cut(nm, c(0:2, 4, 100), right = FALSE,
             labels = c("0", "1", "2-3", ">3")),
    method = "observed") %>%
  filter(!mis) %>%
  ungroup()

dl <- vector("list", length(methods))
names(dl) <- methods
for (meth in methods) {
  imp <- result[[meth]]
  imp <- cbind(imp, mis = is.na(imp$data$lpo))
  ym <- mice::complete(imp, "long") %>%
    group_by(sch, .imp) %>%
    mutate(
      nm = sum(mis),
      ng = cut(nm, c(0:2, 4, 100), right = FALSE,
               labels = c("0", "1", "2-3", ">3")),
      method = meth) %>%
    filter(mis) %>%
    ungroup() %>%
    mutate(
      sch = as.integer(sch)
    )
  dl[[meth]] <- ym
}
ymis <- do.call(rbind.data.frame, dl)
yom <- rbind.data.frame(yobs, ymis)
yom$method <- factor(yom$method,
                     levels = c("observed", "sample", "pmm",
                                "2l.pan", "2l.norm", "2l.pmm"))

trellis.par.set(box.rectangle = list(col = c(rep(mdc(1), 4), mdc(2)),lwd = 0))
trellis.par.set(box.umbrella  = list(col = c(mdc(3)), lwd = 1.2))
trellis.par.set(plot.symbol   = list(col = "white", lwd = 1))
colors <- c(mdc(4), rep(mdc(5), 5))
bwplot(ng ~ lpo | method, data = yom, do.out = FALSE,
       as.table = TRUE, layout = c( 1, 6),
       xlim = c(16, 64),
       xlab = "Language score (grade 8)",
       ylab = "Number of missing values per school",
       col = colors,
       pch = "|",
       panel = function(x, y, col = col, ...) {
         panel.grid(v = -1, h = 0)
         panel.bwplot(x, y, col = col, fill = col[packet.number()], ...)
         meds <- tapply(x, y, median)
         ylocs <- seq_along(meds)
         panel.segments(meds, ylocs - 1/4,
                        meds, ylocs + 1/4,
                        lwd = 2, col = "white")
         }
       )

## ----mldens, echo = FALSE, fig.width = 6, fig.height = 6, solo = TRUE----
imp <- result[["2l.pan"]]
imp2 <- cbind(imp, mis = is.na(imp$data$lpo))
ym <- mice::complete(imp2, "long", include = TRUE) %>%
    group_by(sch, .imp) %>%
    mutate(
      nm = sum(mis),
      ng = cut(nm, c(0:2, 4, 100), right = FALSE,
               labels = c("0", "1", "2-3", ">3")),
      method = meth) %>%
    ungroup()
imp3 <- as.mids(cbind(mice::complete(imp2, "long", include = TRUE), ng = ym$ng))
p.pan <- densityplot(imp3, ~ lpo | ng, subset = ng %in% c("1", "2-3"),
            xlim = c(16, 64), ylim = c(0, 0.06),
            xlab = "Language score (grade 8)",
            ylab = "Density (2l.pan)")

imp <- result[["2l.pmm"]]
imp2 <- cbind(imp, mis = is.na(imp$data$lpo))
ym <- mice::complete(imp2, "long", include = TRUE) %>%
    group_by(sch, .imp) %>%
    mutate(
      nm = sum(mis),
      ng = cut(nm, c(0:2, 4, 100), right = FALSE,
               labels = c("0", "1", "2-3", ">3")),
      method = meth) %>%
    ungroup()
imp3 <- as.mids(cbind(mice::complete(imp2, "long", include = TRUE), ng = ym$ng))
p.pmm <- densityplot(imp3, ~ lpo | ng, subset = ng %in% c("1", "2-3"),
            xlim = c(16, 64), ylim = c(0, 0.06),
            xlab = "Language score (grade 8)",
            ylab = "Density (2l.pmm)")
grid.arrange(p.pan, p.pmm)

## ----toenail.2l.3, cache=TRUE--------------------------------------------
library(tidyr)
data("toenail", package = "DPpackage")
data <- tidyr::complete(toenail, ID, visit) %>%
  tidyr::fill(treatment) %>%
  dplyr::select(-month)
table(data$outcome, useNA = "always")

## ----toenail.2l.4, cache=TRUE--------------------------------------------
pred <- make.predictorMatrix(data)
pred["outcome", "ID"] <- -2
imp <- mice(data, method = "2l.bin", pred = pred, seed = 12102,
            maxit = 1, m = 5, print = FALSE)
table(mice::complete(imp)$outcome, useNA = "always")

## ----toenailprofiles, echo = FALSE, solo = TRUE, fig.width = 6, fig.height = 6----
trellis.par.set(strip.background=list(col="grey95"))
ids <- data %>%
  group_by(ID) %>%
  summarize(n = sum(is.na(outcome))) %>%
  filter(n >= 4) %>% pull(ID)
long <- mice::complete(imp, "long", include = TRUE)
miss <- is.na(long[long$.imp == 0, "outcome"])
miss <- ifelse(miss, 2, 1)
long <- cbind(long, miss = miss)
long <- long[long$.imp != 0, ]
ids <- c(9, 12, 13, 117, 51, 168, 188, 230, 21, 31, 214, 309, 41, 45, 48, 99)
tp1 <- xyplot(outcome ~ visit | factor(ID),
              data = long, type = "p",
              groups = miss,
              subset = ID %in% ids,
              col = mdc(1:2),
              ylab = "Severe infection (Y/N)",
              xlab = "Visit",
              pch = 19, cex = 0.9, as.table = TRUE)
fit <- with(imp,
            lme4::glmer(outcome ~ treatment * visit + (1 | ID),
                        family = binomial))
prob <- imp %>%
  mice::complete("all") %>%
  purrr::map(lme4::glmer,
              formula = outcome ~ treatment * visit + (1 | ID),
              family = binomial) %>%
  purrr:::map(predict, type = "response") %>%
  unlist()

long$prob <- prob
tp2 <- xyplot(prob ~ visit | factor(ID),
              data = long, type = "l",
              lwd = 1.5,
              groups = factor(.imp),
              subset = ID %in% ids,
              col = c(mdc(3), mdc(3), "#009E73", mdc(3), mdc(3)),
              ylab = "Severe infection (Y/N)",
              xlab = "Visit", as.table = TRUE)
print(tp2)
print(tp1, newpage = FALSE)

## ----toenail.est, cache=TRUE---------------------------------------------
library(purrr)
mice::complete(imp, "all") %>%
  purrr::map(lme4::glmer,
             formula = outcome ~ treatment * visit + (1 | ID),
             family = binomial) %>%
  pool() %>%
  summary()

## ----mla.data3-----------------------------------------------------------
data("brandsma", package = "mice")
dat <- brandsma[, c("sch", "pup", "lpo",
                    "iqv", "ses", "ssi")]

## ----mdp, echo = FALSE, solo = TRUE, fig.height=5------------------
z <- md.pattern(dat)

## ----mla.empty10, cache=TRUE---------------------------------------------
d <- brandsma[, c("sch", "lpo")]
pred <- make.predictorMatrix(d)
pred["lpo", "sch"] <- -2
imp <- mice(d, pred = pred, meth = "2l.pmm", m = 10, maxit = 1,
            print = FALSE, seed = 152)

## ----mla.empty5----------------------------------------------------------
library(lme4)
fit <- with(imp, lmer(lpo ~ (1 | sch), REML = FALSE))
summary(pool(fit))

## ----mla.empty6----------------------------------------------------------
library(mitml)
testEstimates(as.mitml.result(fit), var.comp = TRUE)$var.comp

## ----mla.ri1, cache=TRUE-------------------------------------------------
d <- brandsma[, c("sch", "lpo", "iqv")]
pred <- make.predictorMatrix(d)
pred["lpo", ] <- c(-2, 0, 3)
pred["iqv", ] <- c(-2, 3, 0)
imp <- mice(d, pred = pred, meth = "2l.pmm", seed = 919,
            m = 10, print = FALSE)

## ----mla.ri1b, eval = FALSE----------------------------------------------
d$lpo <- as.vector(scale(d$lpo, scale = FALSE))

## ----mla.ri2, cache = TRUE-----------------------------------------------
fm1 <- lpo + iqv ~ 1 + (1 | sch)
mit <- mitml::panImpute(data = d, formula = fm1, m = 5,
                        silent = TRUE)

## ----mla.ri3, cache = TRUE-----------------------------------------------
blk <- make.blocks(d, "collect")
fm2 <- list(collect = fm1)
imp2 <- mice(d, meth = "panImpute", blocks = blk, form = fm2,
             print = FALSE, seed = 711)

## ----mla.ri4-------------------------------------------------------------
fit <- with(imp, lmer(lpo ~  iqv + (1 | sch), REML = FALSE))
summary(pool(fit))
testEstimates(as.mitml.result(fit), var.comp = TRUE)$var.comp

## ----mla.riwb1-----------------------------------------------------------
res <- mice::complete(imp, "long") %>%
  group_by(sch, .imp) %>%
  mutate(iqm = mean(iqv)) %>%
  group_by(.imp) %>%
  do(model = lmer(lpo ~ iqv + iqm + (1 | sch),
                  REML = FALSE, data = .)) %>%
  as.list() %>% .[[-1]]
summary(pool(res))
testEstimates(res, var.comp = TRUE)$var.comp

## ----mla.ril2p1----------------------------------------------------------
d <- brandsma[, c("sch", "lpo", "iqv", "den")]
meth <- make.method(d)
meth[c("lpo", "iqv", "den")] <- c("2l.pmm", "2l.pmm",
                                  "2lonly.pmm")
pred <- make.predictorMatrix(d)
pred["lpo", ] <- c(-2, 0, 3, 1)
pred["iqv", ] <- c(-2, 3, 0, 1)
pred["den", ] <- c(-2, 1, 1, 0)
imp <- mice(d, pred = pred, meth = meth, seed = 418,
            m = 10, print = FALSE)

## ----mla.ril2p3, cache = TRUE--------------------------------------------
d$den <- as.factor(d$den)
fml <- list(lpo + iqv ~ 1 + (1 | sch), den ~ 1)
mit <- mitml::jomoImpute(data = d, formula = fml, m = 10,
                         silent = TRUE)

## ----mla.ril2p4, cache = TRUE--------------------------------------------
blk <- make.blocks(d, "collect")
fm2 <- list(collect = fml)
imp2 <- mice(d, meth = "jomoImpute", blocks = blk, form = fm2,
             print = FALSE, seed = 418, maxit = 1,
             m = 10, n.burn = 100)

## ----mladens, echo = FALSE, solo = TRUE, fig.height=6, fig.width=6----
# convert den back to numeric for plotting
z <- mice::complete(imp2, "long", include = TRUE)
z$den <- as.numeric(levels(z$den))[z$den]
imp3 <- as.mids(z)
p.jomo <- densityplot(imp3, ~ lpo + den, ylab = "jomoImpute", ylim = list(c(0, 0.05), c(0, 1.5)))
p.pmm <- densityplot(imp, ~ lpo + den, ylab = "2l.pmm", ylim = list(c(0, 0.05), c(0, 1.5)))
p.jomo2 <- update(p.jomo, xlim = list(c(16, 64), c(0, 5)))
p.pmm2 <- update(p.pmm, xlim = list(c(16, 64), c(0, 5)))
grid.arrange(p.jomo2, p.pmm2)

## ----mla.ril2p6----------------------------------------------------------
fit <- with(imp, lmer(lpo ~ 1 + iqv + as.factor(den)
                      + (1 | sch), REML = FALSE))
summary(pool(fit))
testEstimates(as.mitml.result(fit), var.comp = TRUE)$var.comp

## ----mla.int1------------------------------------------------------------
d <- brandsma[, c("sch", "lpo", "iqv", "sex", "den")]
d <- data.frame(d, lpm = NA, iqm = NA, sxm = NA,
                iqd = NA, lpd = NA,
                iqd.sex = NA, lpd.sex = NA, iqd.lpd = NA,
                iqd.den = NA, sex.den = NA, lpd.den = NA,
                iqm.den = NA, sxm.den = NA, lpm.den = NA)

## ----mla.int2------------------------------------------------------------
# level-1 variables
meth <- make.method(d)
meth[c("lpo", "iqv", "sex")] <- "2l.pmm"

pred <- make.predictorMatrix(d)
pred[,] <- 0
pred[, "sch"] <- -2
codes <- c(3, 3, rep(1, 6))
pred["lpo", c("iqv", "sex", "iqd.sex", "sex.den", "iqd.den",
              "den", "iqm.den", "sxm.den")] <- codes
pred["iqv", c("lpo", "sex", "lpd.sex", "sex.den", "lpd.den",
              "den", "lpm.den", "sxm.den")] <- codes
pred["sex", c("lpo", "iqv", "iqd.lpd", "lpd.den", "iqd.den",
              "den", "iqm.den", "lpm.den")] <- codes

## ----mla.int2b-----------------------------------------------------------
# level-2 variables
meth["den"] <- "2lonly.pmm"
pred["den", c("lpo", "iqv", "sex",
              "iqd.sex", "lpd.sex", "iqd.lpd")] <- 1

## ----mla.int2c, echo = FALSE---------------------------------------------
t(pred[c("lpo", "iqv", "sex", "den"), c("sch", "lpo", "iqv", "sex", "den", "iqd.sex", "lpd.sex", "iqd.lpd", "iqd.den", "sex.den", "lpd.den", "iqm.den", "sxm.den", "lpm.den")])

## ----mla.int4------------------------------------------------------------
# derive group means
meth[c("iqm", "sxm", "lpm")] <- "2l.groupmean"
pred[c("iqm", "sxm", "lpm"), c("iqv", "sex", "lpo")] <- diag(3)

# derive deviations from cluster mean
meth["iqd"] <- "~ I(iqv - iqm)"
meth["lpd"] <- "~ I(lpo - lpm)"

## ----mla.int4a-----------------------------------------------------------
# derive interactions
meth["iqd.sex"] <- "~ I(iqd * sex)"
meth["lpd.sex"] <- "~ I(lpd * sex)"
meth["iqd.lpd"] <- "~ I(iqd * lpd)"
meth["iqd.den"] <- "~ I(iqd * den)"
meth["sex.den"] <- "~ I(sex * den)"
meth["lpd.den"] <- "~ I(lpd * den)"
meth["iqm.den"] <- "~ I(iqm * den)"
meth["sxm.den"] <- "~ I(sxm * den)"
meth["lpm.den"] <- "~ I(lpm * den)"

## ----mla.int5, cache=TRUE------------------------------------------------
visit <- c("lpo", "lpm", "lpd",
           "lpd.sex", "iqd.lpd", "lpd.den", "lpm.den",
           "iqv", "iqm", "iqd",
           "iqd.sex", "iqd.lpd", "iqd.den", "iqm.den",
           "sex", "sxm",
           "iqd.sex", "lpd.sex", "sex.den", "sxm.den",
           "den", "iqd.den", "sex.den", "lpd.den",
           "iqm.den", "sxm.den", "lpm.den")

imp <- mice(d, pred = pred, meth = meth, seed = 188,
            visit = visit, m = 10, print = FALSE,
            allow.na = TRUE)

## ----mla.int7------------------------------------------------------------
long <- mice::complete(imp, "long", include = TRUE)
long$den <- as.factor(long$den)
imp2 <- as.mids(long)
fit <- with(imp2, lmer(lpo ~ 1 + iqv*sex + iqm*den + sex*den
                      + (1 | sch), REML = FALSE))
summary(pool(fit))

## ----mla.rs1, cache=TRUE-------------------------------------------------
d <- brandsma[, c("sch", "lpo", "iqv")]
d$lpo <- as.vector(scale(d$lpo, scale = FALSE))
pred <- make.predictorMatrix(d)
pred["lpo", ] <- c(-2, 0, 4)
pred["iqv", ] <- c(-2, 4, 0)
pred
imp <- mice(d, pred = pred, meth = "2l.pmm", seed = 441,
            m = 10, print = FALSE, maxit = 20)

## ----mla.rs2-------------------------------------------------------------
imp2 <- mice::complete(imp, "long", include = TRUE) %>%
  group_by(sch) %>%
  mutate(iqm = mean(iqv, na.rm = TRUE),
         lpo = lpo + mean(brandsma$lpo, na.rm = TRUE)) %>%
  as.mids()
fit <- with(imp2, lmer(lpo ~  iqv + iqm + (1 + iqv | sch),
                       REML = FALSE))
summary(pool(fit))
testEstimates(as.mitml.result(fit), var.comp = TRUE)$var.comp

## ----mla.rs3-------------------------------------------------------------
d <- brandsma[, c("sch", "lpo", "iqv", "ses")]
d$lpo <- as.vector(scale(d$lpo, scale = FALSE))
d <- data.frame(d,
                iqv.ses = NA, ses.lpo = NA, iqv.lpo = NA,
                lpm = NA, iqm = NA, sem = NA,
                iqv.iqm = NA, ses.sem = NA, lpo.lpm = NA,
                iqv.sem = NA, iqv.lpm = NA,
                ses.iqm = NA, ses.lpm = NA,
                lpo.iqm = NA, lpo.sem = NA,
                iqm.sem = NA, lpm.sem = NA, iqm.lpm = NA)

## ----mla.rs4, echo = FALSE-----------------------------------------------
pred <- make.predictorMatrix(d)
meth <- make.method(d)

# level-1 variables
meth[c("lpo", "iqv", "ses")] <- "2l.pmm"
pred["lpo", ] <- c(-2, 0, 3, 3, 1, 0, 0, 0, 0, 0, 1, 1, 0,
                    1, 0, 1, 0, 0, 0, 1, 0, 0)
pred["iqv", ] <- c(-2, 3, 0, 3, 0, 1, 0, 0, 0, 0, 0, 1, 1,
                    0, 0, 0, 1, 0, 1, 0, 1, 0)
pred["ses", ] <- c(-2, 3, 3, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1,
                    0, 1, 0, 0, 1, 0, 0, 0, 1)
t(pred[c("lpo", "iqv", "ses"), -(8:10)])

## ----mla.rs6-------------------------------------------------------------
meth[c("iqm", "sem", "lpm")] <- "2l.groupmean"
pred[c("iqm", "sem", "lpm"), ] <- 0
pred["iqm", c("sch", "iqv")] <- c(-2, 1)
pred["sem", c("sch", "ses")] <- c(-2, 1)
pred["lpm", c("sch", "lpo")] <- c(-2, 1)

## ----mla.rs5-------------------------------------------------------------
meth["iqv.ses"] <- "~ I(iqv * ses)"
meth["iqv.lpo"] <- "~ I(iqv * lpo)"
meth["ses.lpo"] <- "~ I(ses * lpo)"

## ----mla.rs7, echo = FALSE-----------------------------------------------
meth["iqv.iqm"] <- "~ I(iqv * iqm)"
meth["ses.sem"] <- "~ I(ses * sem)"
meth["lpo.lpm"] <- "~ I(lpo * lpm)"
meth["iqv.sem"] <- "~ I(iqv * sem)"
meth["iqv.lpm"] <- "~ I(iqv * lpm)"
meth["ses.iqm"] <- "~ I(ses * iqm)"
meth["ses.lpm"] <- "~ I(ses * lpm)"
meth["lpo.iqm"] <- "~ I(lpo * iqm)"
meth["lpo.sem"] <- "~ I(lpo * sem)"
meth["iqm.sem"] <- "~ I(iqm * sem)"
meth["iqm.lpm"] <- "~ I(iqm * lpm)"
meth["lpm.sem"] <- "~ I(lpm * sem)"

## ----mla.rs8, cache=TRUE-------------------------------------------------
visit <- c("lpo", "iqv.lpo", "ses.lpo",
           "lpm", "lpo.lpm", "iqv.lpm", "ses.lpm",
           "lpo.iqm", "lpo.sem", "iqm.lpm", "lpm.sem",
           "iqv", "iqv.ses", "iqv.lpo",
           "iqm", "iqv.iqm", "iqv.sem", "iqv.lpm",
           "ses.iqm", "lpo.iqm", "iqm.sem", "iqm.lpm",
           "ses", "iqv.ses", "ses.lpo",
           "sem", "ses.sem", "iqv.sem", "ses.iqm",
           "ses.lpm", "lpo.sem", "iqm.sem", "lpm.sem")

imp <- mice(d, pred = pred, meth = meth, seed = 211,
            visit = visit, m = 10, print = FALSE, maxit = 10,
            allow.na = TRUE)

## ----mla.rs9, warning=FALSE----------------------------------------------
fit <- with(imp, lmer(lpo ~ iqv * ses + iqm * sem +
                  iqv * iqm + iqv * sem +
                  ses * iqm + ses * sem + (1 + ses + iqv | sch),
                  REML = FALSE))

## ----mla.rs10------------------------------------------------------------
summary(pool(fit))
testEstimates(as.mitml.result(fit), var.comp = TRUE)$var.comp


## ----ch8, child = "src/ch8.Rnw"------------------------------------------

## ----init8, echo = FALSE, results = 'hide'-------------------------------
opts_chunk$set(fig.path = 'fig/ch8-', self.contained = FALSE)
pkg <- c("mice", "dplyr", "gridExtra")
loaded <- sapply(pkg, require, character.only = TRUE,
                 warn.conflicts = FALSE, quietly = TRUE)

## ----ice.definedata------------------------------------------------------
ideal <- data.frame(
  x = c(68, 76, 66, 81, 70, 72, 81, 72),
  y1 = c(14, 0, 1, 2, 3, 1, 10, 9),
  y0 = c(13, 6, 4, 5, 6, 6, 8, 8),
  row.names = c("John", "Caren", "Joyce", "Robert",
                "Ruth", "Nick", "Peter", "Torey")
)

# assign first three units to trt
data <- ideal
data[1:3, "y0"] <- NA
data[4:8, "y1"] <- NA

## ----ice.firsttry--------------------------------------------------------
library(mice)
data2 <- data[, -1]
imp <- mice(data2, method = "norm", seed = 188, print = FALSE)

## ----iceplot1, fig.width=7, fig.height=3.5, echo=FALSE, solo=TRUE-------
trellis.par.set(list(layout.heights = list(strip = 1.3)))
stripplot(imp, y1 + y0 ~ as.factor(.imp), pch = c(1, 19),
          col = c(mdc(1:2)), ylim = c(-100, 100))

## ----ice.corr1-----------------------------------------------------------
sapply(mice::complete(imp, "all"), function(x) cor(x$y1, x$y0))

## ----ice.makeprior-------------------------------------------------------
set.seed(84409)
rho <- 0.9
mu <- mean(unlist(data2[, c("y1", "y0")]), na.rm = TRUE)
sigma2 <- var(unlist(data2), na.rm = TRUE)
cv <- rho * sigma2
s2 <- matrix(c(sigma2, cv, cv, sigma2), nrow = 2)
prior <- data.frame(MASS::mvrnorm(n = 100, mu = rep(mu, 2),
                                  Sigma = s2))
names(prior) <- c("y1", "y0")

## ----ice.stack-----------------------------------------------------------
# combine data and prior
stacked <- dplyr::bind_rows(prior, data2, .id = "d")
stacked$d <- as.numeric(stacked$d) - 1

## ----ice.impute----------------------------------------------------------
stacked$tau <- stacked$y1 - stacked$y0
pred <- make.predictorMatrix(stacked)
pred[, "tau"] <- 0
meth <- c("", "norm", "norm", "~ I(y1 - y0)")
imp <- mice(stacked, maxit = 100, pred = pred,
            meth = meth, print = FALSE)

## ----icetrace, fig.width=7, fig.height=5, echo=FALSE, solo=TRUE----
plot(imp)

## ----iceplot2, fig.width=7, fig.height=3.5, echo=FALSE, solo=TRUE-------
stripplot(imp, y1 + y0 ~ as.factor(.imp), pch = c(1, 19),
          col = c("grey70", mdc(2)), ylim = c(-5, 15))

## ----ice.corr2-----------------------------------------------------------
sapply(mice::complete(imp, "all"), function(x) {
  x <- x[x$d == 1, ]; cor(x$y0, x$y1)})

## ----potential1, fig.height = 5, echo = FALSE---------------------------
xyplot(imp, y1 ~ y0 | as.factor(.imp), layout = c(3, 2),
       groups = d, col = c("grey70", mdc(2)), pch = c(1, 19))

## ----potential1.show, eval = FALSE--------------------------------------
xyplot(imp, y1 ~ y0 | as.factor(.imp), layout = c(3, 2),
       groups =  d, col = c("grey70", mdc(2)), pch = c(1, 19))

## ----ice.iceplotfunction, echo = FALSE-----------------------------------
iceplot <- function(data, rho, ynames = c("y1", "y0"),
                    maxit = 100, m =100, seed = NULL) {
  # no covariates
  if (!is.null(seed)) set.seed(seed)

  # define prior dataset
  mu <- mean(unlist(data[, ynames]), na.rm = TRUE)
  s2 <- matrix(c(1, rho,  rho, 1), nrow = 2)
  prior <- data.frame(MASS::mvrnorm(n = 100, mu = rep(mu, 2), Sigma = s2))
  names(prior) <- ynames

  # combine data and prior
  stacked <- dplyr::bind_rows(prior, data[, ynames], .id = "d")
  stacked$d <- as.numeric(stacked$d) - 1
  stacked$tau <- stacked[, ynames[1]] - stacked[, ynames[2]]

  # define imputation model
  pred <- make.predictorMatrix(stacked)
  pred[, "tau"] <- 0
  meth <- c("", "norm", "norm", "~ I(y1 - y0)")
  imp <- mice(stacked, maxit = maxit, pred = pred,
              meth = meth, m = m, print = FALSE)

  long <- mice::complete(imp, "long")
  long <- long[long$d == 1, ]
  long$pat <- factor(long$.id, labels = row.names(data))
  p <- parallelplot(~ long[, 4:5] | pat, data = long,
                    horizontal.axis = FALSE, common.scale = TRUE,
                    layout = c(8, 1), as.table = TRUE,
                    between = list(x = 0.8, y = 0.5),
                    par.settings = mice.theme(), col = mdc(2), lwd = 1,
                    varnames = c("1", "0"),
                    scales = list(cex = 0.8))
  p
}

## ----ice.calciceplots, cache=TRUE, echo = FALSE--------------------------
p50 <- iceplot(data, rho = 0.5, maxit = 10, seed = 1)
p90 <- iceplot(data, rho = 0.9, seed = 1)
p99 <- iceplot(data, rho = 0.99, seed = 1)

## ----fanplot, fig.width=7, fig.height=10, solo = TRUE, echo = FALSE----
library(gridExtra, warn.conflicts = FALSE)
if (empty_figure) {
  plot.new()
  } else {
grid.arrange(p50, p90, p99, nrow = 3)
  }


## ----ch9, child = "src/ch9.Rnw"------------------------------------------

## ----init9, echo = FALSE, results = 'hide'-------------------------------
opts_chunk$set(fig.path = 'fig/ch9-', self.contained = FALSE)
pkg <- c("mice", "MASS", "foreign")
loaded <- sapply(pkg, require, character.only = TRUE,
                 warn.conflicts = FALSE, quietly = TRUE)

## ----c85imputeblind, eval=FALSE------------------------------------------
library(mice)
## DO NOT DO THIS
imp <- mice(data)     # not recommended

## ----c85readdata1--------------------------------------------------------
library(foreign)
file.sas <- "data/c85/master85.xport"
original.sas <- read.xport(file.sas)
names(original.sas) <- tolower(names(original.sas))
dim(original.sas)

## ----c85readdata2--------------------------------------------------------
# remove 15 columns (text, administrative)
all <- names(original.sas)
drop <- c(3, 22, 58, 162:170, 206:208)
keep <- !(1:length(all) %in% drop)
leiden85 <- original.sas[original.sas$abr == "1", keep]
data <- leiden85

## ----c85inspect1---------------------------------------------------------
ini <- mice(data, maxit = 0)   # recommended
table(ini$nmis)

## ----c85inspect2---------------------------------------------------------
table(data$beroep1, useNA = "always")

## ----c85inspect4a--------------------------------------------------------
v1 <- names(ini$nmis[ini$nmis == 0])
outlist1 <- v1[c(1, 3:5, 7:10, 16:47, 51:60, 62, 64:65, 69:72)]
length(outlist1)

## ----c85flux,  solo=TRUE, echo=FALSE, fig.height=4, fig.width=4----------
if (empty_figure) {
   fx <- fluxplot(data, main=NULL, cex=0.7, lwd = 0.6, col = mdc(4), plot = FALSE)
  plot.new()
  } else {
 fx <- fluxplot(data, main=NULL, cex=0.7, lwd = 0.6, col = mdc(4))
  }

## ----c85inspect6---------------------------------------------------------
outlist2 <- row.names(fx)[fx$outflux < 0.5]
length(outlist2)

## ----c85inspect7, eval=TRUE----------------------------------------------
data2 <- data[, !names(data) %in% outlist2]
fx2 <- flux(data2)
outlist3 <- row.names(fx2)[fx2$outflux < 0.5]

## ----c85inspect8---------------------------------------------------------
head(ini$loggedEvents, 2)
tail(ini$loggedEvents, 2)

## ----c85inspect9---------------------------------------------------------
outlist4 <- as.character(ini$loggedEvents[, "out"])

## ----c85inspect10--------------------------------------------------------
outlist <- unique(c(outlist1, outlist2, outlist4))
length(outlist)

## ----c85inspect11--------------------------------------------------------
data2 <- data[, !names(data) %in% outlist]

## ----c85quickpred--------------------------------------------------------
inlist <- c("sex", "lftanam", "rrsyst", "rrdiast")
pred <- quickpred(data2, minpuc = 0.5, include = inlist)

## ----c85predinspect1-----------------------------------------------------
table(rowSums(pred))

## ----c85predinspect2-----------------------------------------------------
rowSums(pred[c("rrsyst", "rrdiast"),])

## ----c85predinspect3, eval=FALSE-----------------------------------------
names(data2)[pred["rrsyst", ] == 1]

## ----c85predinspect4, eval=FALSE-----------------------------------------
vname <- "rrsyst"
y <- cbind(data2[vname], r =! is.na(data2[, vname]))
vdata <- data2[,pred[vname,] == 1]
round(cor(y = y, x = vdata, use = "pair"), 2)

## ----c85fetch, echo=FALSE------------------------------------------------
load("data/c85/imp.blind")
load("data/c85/imp.smart")
imp.qp <- imp.smart
imp <- imp.blind
rm(imp.blind, imp.smart)

## ----c85imputesmart, eval=FALSE------------------------------------------
imp.qp <- mice(data2, pred = pred, seed = 29725)

## ----blindvsquickpred,  echo=FALSE, fig.width=6, fig.height=4--------------
vnames <- c("rrsyst", "rrdiast")
cd1 <- mice::complete(imp)[, vnames]
cd2 <- mice::complete(imp.qp)[, vnames]
typ <- factor(rep(c("blind imputation","quickpred"), each = nrow(cd1)))
mis <- ici(data2[,vnames])
mis <- is.na(imp$data$rrsyst)|is.na(imp$data$rrdiast)
cd <- data.frame(typ=typ,mis=mis,rbind(cd1, cd2))
if (empty_figure) {
  plot.new()
  } else {
tp <- xyplot(jitter(rrdiast,10) ~ jitter(rrsyst,10) | typ, data = cd, groups = mis,
             xlab = "Systolic BP (mmHg)", ylab = "Diastolic BP (mmHg)",
             col=c(mdc(1),mdc(2)), pch=c(1,19),type=c("g","p"),
             strip = strip.custom(bg="grey95"),
             scales = list(alternating=1, tck=c(1,0)))
print(tp)
}

## ----c85trellis, eval=FALSE----------------------------------------------
vnames <- c("rrsyst", "rrdiast")
cd1 <- mice::complete(imp)[, vnames]
cd2 <- mice::complete(imp.qp)[, vnames]
typ <- factor(rep(c("blind imputation", "quickpred"),
                  each = nrow(cd1)))
mis <- ici(data2[, vnames])
mis <- is.na(imp$data$rrsyst) | is.na(imp$data$rrdiast)
cd <- data.frame(typ = typ, mis = mis, rbind(cd1, cd2))
xyplot(jitter(rrdiast, 10) ~ jitter(rrsyst, 10) | typ,
       data = cd, groups = mis,
       col = c(mdc(1), mdc(2)),
       xlab = "Systolic BP (mmHg)",
       type = c("g","p"), ylab = "Diastolic BP (mmHg)",
       pch = c(1, 19),
       strip = strip.custom(bg = "grey95"),
       scales = list(alternating = 1, tck = c(1, 0)))

## ----c85nelson-----------------------------------------------------------
dat <- cbind(data2, dead = 1 - data2$dwa)
hazard <- nelsonaalen(dat, survda, dead)

## ----c85nelsoncorrelation, eval=FALSE, echo=FALSE------------------------
tmp <- data.frame(hazard, t = data2$survda,
                  logt = log(data2$survda),
                  SBP = data2$rrsyst, DBP = data2$rrdiast)
round(cor(tmp, use = "pair"), 3)

## ----c85km, echo=FALSE, fig.height=4-------------------------------------
library(survival)
fit <- survfit(Surv(survda / 365, 1 - dwa) ~ is.na(rrsyst),
               data = data2)
lwd <- 0.6
plot(fit, lty = 1, lwd = 0.9,
     xlab = "Years since Intake",
     ylab = "K-M Survival Probability",
     col = c(mdc(4), mdc(5)), mark.time = FALSE,
     axes = FALSE)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
box(lwd = lwd)
text(4, 0.7, "BP measured")
text(2, 0.3, "BP missing")

## ----c85undamped, eval=FALSE---------------------------------------------
delta <- c(0, -5, -10, -15, -20)
post <- imp.qp$post
imp.all.undamped <- vector("list", length(delta))

for (i in 1:length(delta)) {
  d <- delta[i]
  cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] +", d)
  post["rrsyst"] <- cmd
  imp <- mice(data2, pred = pred, post = post, maxit = 10,
              seed = i * 22)
  imp.all.undamped[[i]] <- imp
}


## ----c85damper, eval=FALSE-----------------------------------------------
cmd <- paste("fit <- lm(y ~ as.matrix(x));
              damp <- sqrt(1 - summary(fit)$r.squared);
              imp[[j]][, i] <- imp[[j]][, i] + damp * ", d)

## ----c85readimp, echo=FALSE----------------------------------------------
load("data/c85/imp.all.damper2")
imp.all.damped <- imp.all.damper2
rm(imp.all.damper2)

## ----c85cda--------------------------------------------------------------
cda <- expression(
    sbpgp <- cut(rrsyst, breaks = c(50, 124, 144, 164, 184, 200,
                                    500)),
    agegp <- cut(lftanam, breaks = c(85, 90, 95, 110)),
    dead  <- 1 - dwa,
    coxph(Surv(survda, dead)
          ~ C(sbpgp, contr.treatment(6, base = 3))
          + strata(sexe, agegp)))
imp <- imp.all.damped[[1]]
fit <- with(imp, cda)

## ----c85hazardratio------------------------------------------------------
as.vector(exp(summary(pool(fit))[, 1]))

## ----readdata7a, echo=FALSE----------------------------------------------
library(mice)
bmi <- function(h, w) w / (h / 100)^2
krul <- selfreport[selfreport$src == "krul",]
males <- krul[krul$sex=="Male",]

## ----plotbmi,  fig.width=4.5, fig.height=3,solo=TRUE,echo=FALSE----------
lwd <- 0.6
xy <- xy.coords(krul$bm, krul$br-krul$bm)
if (empty_figure) {
  plot.new()
  } else {
plot(xy, col = mdc(1),
     xlab = "Measured BMI", ylab = "Self-Reported - Measured BMI",
     xlim = c(17, 45), ylim = c(-5,5), type = "n",lwd = lwd,
     axes = FALSE)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
polygon(x = c(30, 20, 30), y = c(0, 10, 10),
        col = "grey95", border = NA)
polygon(x = c(30, 40, 30), y = c(0, -10, -10),
        col = "grey95", border = NA)
abline(0, 0, lty = 2, lwd = lwd)
points(xy, col = mdc(1),cex = 0.7)
lines(lowess(xy), lwd = 1.5, col = mdc(4))
text(1:4, x = c(40, 28, 20, 32), y = c(4, 4, -4, -4), cex = 3)
box(lwd = lwd)
}

## ----plotexplain,fig.height=3, fig.width=3,solo=TRUE,echo=FALSE----------
lwd = 0.6
fit <- lm(bm~br,data=males)
plot(x=males$br,y=males$bm,
     xlim=c(26,34),ylim=c(26,34),
     xlab="Self-Reported BMI", ylab="Measured BMI",col=mdc(1),
     cex.lab=1, cex = 0.7, bty = "n", axes = FALSE)
axis(side = 1, lwd = lwd)
axis(side = 2, lwd = lwd, las = 1)
box(lwd = lwd)
abline(h=30,v=30,lty=2, lwd = lwd)
abline(coef(fit),col=mdc(4), lwd = lwd)
abline(v=(30-coef(fit)[1])/coef(fit)[2],col=mdc(4), lwd = lwd)
text(1:4,x=c(33.8,33.8,26.2,26.2),y=c(33.8,26.2,26.2,33.8),cex=2.5)
text(c("a","b"),x=c(29.1,29.7,29.1,29.7),y=c(34.1,34.1,26.4,26.4),cex=1.8,adj=c(0.5,1))

## ----srcpattern----------------------------------------------------------
data <- selfreport[, c("age", "sex", "hm", "hr", "wm", "wr")]
md.pattern(data, plot = FALSE)


## ----imputebmi, cache=TRUE-----------------------------------------------
bmi <- function(h, w) w / (h / 100)^2
meth <- make.method(selfreport)
meth[c("prg", "edu", "etn")] <- ""
meth["bm"] <- "~ bmi(hm, wm)"
pred <- make.predictorMatrix(selfreport)
pred[, c("src", "id", "pop", "prg", "edu", "etn",
         "web", "bm", "br")] <- 0
imp <- mice(selfreport, pred = pred, meth = meth, m = 10,
            seed = 66573, maxit = 20, print = FALSE)

## ----plotimpbmi,  fig.width=4.5, fig.height=3,solo=TRUE,echo=FALSE-------
lwd <- 0.6
cd <- mice::complete(imp, 1)
xy <- xy.coords(cd$bm, cd$br - cd$bm)
if (empty_figure) {
  plot.new()
  } else {
plot(xy, col = mdc(2),
     xlab = "Measured BMI", ylab = "Self-Reported - Measured BMI",
     xlim = c(17, 45), ylim = c(-5, 5), type = "n", lwd = lwd,
     axes = FALSE)
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
polygon(x = c(30, 20, 30), y = c(0, 10, 10),
        col = "grey95", border = NA)
polygon(x = c(30, 40, 30), y = c(0, -10, -10),
        col = "grey95", border = NA)
abline(0, 0, lty = 2, lwd = lwd)

idx <- cd$src == "krul"
xyc <- xy; xyc$x <- xy$x[idx]; xyc$y <- xy$y[idx]
xys <- xy; xys$x <- xy$x[!idx]; xys$y <- xy$y[!idx]
points(xyc, col = mdc(1), cex=0.7)
points(xys, col = mdc(2), cex=0.7)
lines(lowess(xyc), col = mdc(4), lwd = 1.5)
lines(lowess(xys), col = mdc(5), lwd = 1.5)
text(1:4, x = c(40, 28, 20, 32), y = c(4, 4, -4, -4), cex = 3)
box(lwd = lwd)
}

## ----walkinginit, echo=FALSE---------------------------------------------
library(mice)
library(foreign)
dataproject <- "Data/wad"
.store <- file.path(dataproject,"R/store/")

file <- file.path(dataproject, "original/walklink.sav")
original <- read.spss(file=file, to.data.frame=TRUE)
data <- original
names(data) <- c("src","sex","age","sip1","haq8","grs9")
data$src <- factor(data$src, levels=2:3, labels=c("Ergo","Euri"))
data$sip1 <- as.factor(data$sip1)
data$haq8 <- as.ordered(data$haq8)
data$grs9 <- as.ordered(data$grs9)

## ----walkingimpute1------------------------------------------------------
fA <- c(242, 43, 15, 0, 6)
fB <- c(145, 110, 29, 8)
YA <- rep(ordered(c(0:3, NA)), fA)
YB <- rep(ordered(c(0:3)), fB)
Y <- rbind(data.frame(YA, YB = ordered(NA)),
           data.frame(YB, YA = ordered(NA)))

## ----walkingpattern, echo=FALSE, fig.height=2.5--------------------------
z <- md.pattern(Y)

## ----walkingimpute2------------------------------------------------------
ra <- function(x, simplify = FALSE) {
  if (!is.mira(x)) return(NULL)
  ra <- x$analyses
  if (simplify) ra <- unlist(ra)
  return(ra)
}
micemill <- function(n) {
  for (i in 1:n) {
    imp <<- mice.mids(imp, print = FALSE)
    cors <- with(imp, cor(as.numeric(YA), as.numeric(YB),
                          method = "kendall"))
    tau <<- rbind(tau, ra(cors, simplify = TRUE))
  }
}

## ----walkingimpute3, cache = TRUE----------------------------------------
tau <- NULL
imp <- mice(Y, maxit = 0, m = 10, seed = 32662, print = FALSE)
micemill(50)

## ----walkingimpute5a, eval = FALSE---------------------------------------
plotit <- function()
  matplot(x = 1:nrow(tau), y = tau,
          ylab = expression(paste("Kendall's ", tau)),
          xlab = "Iteration", type = "l", las = 1)
plotit()

## ----walkingimpute5b, solo=TRUE, echo=FALSE, fig.width = 6, fig.height = 3----
plotit2 <- function(lwd = 0.6) {
  matplot(x = 1:nrow(tau), y = tau,
          ylab = expression(paste("Kendall's ", tau)),
          xlab = "Iteration", type = "l", axes = FALSE,
          lwd = lwd, cex.lab = 1.25)
  axis(1, lwd = lwd)
  axis(2, lwd = lwd, las = 1)
  box(lwd = lwd)
}
plotit2()

## ----walkingimpute5d, fig.height=3, echo=FALSE---------------------------
z <- md.pattern(walking)

## ----wasimpute7a, cache = TRUE-------------------------------------------
tau <- NULL
pred <- make.predictorMatrix(walking)
pred[, c("src", "age", "sex")] <- 0
imp <- mice(walking, maxit = 0, m = 10, pred = pred,
            seed = 92786, print = FALSE)
micemill(20)

## ----walkingimpute7b,  echo=FALSE, fig.height=3, fig.width=6, solo=TRUE----
plotit2()

## ----walkingimpute8, eval = FALSE----
props <- with(imp, mean(YB[src == "A"] == '0'))
thetaAB <<- rbind(thetaAB, ra(props, simplify = TRUE))

## ----walkingimpute9, echo=FALSE------------------------------------------
micemill <- function(n) {
  for (i in 1:n) {
    imp <<- mice.mids(imp, print = FALSE)
    cors <- with(imp, cor(as.numeric(YA[src=="A"]),
                         as.numeric(YB[src=="A"]),
                         method="kendall"))
    tau <<- rbind(tau, ra(cors, s=TRUE))  # global assignment
    means <- with(imp, mean(as.numeric(YA[src=="A"]), na.rm=TRUE))
    thetaBA <<- rbind(thetaBA, ra(means, s=TRUE)-1)
    props <- with(imp, mean(YB[src=="A"]=='0'))
    thetaAB <<- rbind(thetaAB, ra(props, s=TRUE))
    tabs <- with(imp, ftable(addmargins(
                      table(YA[src=="A"],YB[src=="A"],
                            useNA="ifany", dnn=c("YA","YB")))))
  }
}
thetaBA <- NULL

## ----walkingimpute10, cache = TRUE---------------------------------------
tau <- NULL
thetaAB <- NULL
pred2 <- pred1 <- make.predictorMatrix(walking)
pred1[, c("src", "age", "sex")] <- 0
pred2[, "src"] <- 0
imp <- mice(walking, maxit = 0, m = 10, pred = pred1,
            seed = 99786)
micemill(20)
imp <- mice(walking, maxit = 0, m = 10, pred = pred2)
micemill(20)

## ----walkingplotthetaAB,  fig.height=3, fig.width=6, solo=TRUE, echo=FALSE----
lwd <- 0.6
matplot(x = 1:nrow(thetaAB), y = thetaAB,
        xlab = "Iteration",
        ylab = expression(hat(theta)[AB]),
        type = "l", axes = FALSE,
        lwd = lwd, cex.lab = 1.25,
        ylim = c(0.42, 0.67))
axis(1, lwd = lwd)
axis(2, lwd = lwd, las = 1)
box(lwd = lwd)
abline(h = 0.497, v = 20, lty = 2, lwd = lwd, col = "grey")
text(x = 5, y = 0.488, expression(hat(theta)[BB]), adj = 0)
text(x = 5, y = 0.430, "Without covariates", adj = 0)
text(x = 25, y = 0.430, "With covariates", adj = 0)
arrows(x0 = 4.5, y0 = 0.488, x1 = 0, y1 = 0.495,
       length = 0.1, angle = 20)
points(x = -0.4, y = 0.497, pch = 20)


## ----ch10, child = "src/ch10.Rnw"----------------------------------------

## ----init10, echo = FALSE, results = 'hide'------------------------------
opts_chunk$set(fig.path = 'fig/ch10-', self.contained = FALSE)
pkg <- c("mice", "haven")
loaded <- sapply(pkg, require, character.only = TRUE,
                 warn.conflicts = FALSE, quietly = TRUE)

## ----popreaddata, echo=FALSE---------------------------------------------
## Incomplete POPS-19 data (n=959, nvar=88)
file.spss <- file.path("data/pop/incompleet_v3.sav")
pops <- read_spss(file=file.spss)
pops <- as_factor(pops)
data <- pops[,-(87:88)]

## Read predictor matrix
file.pred <- file.path("data/pop/predictor matrix_v3.txt")
pred <- read.table(file=file.pred, sep="\t")
pred <- as.matrix(pred[-c(87:89)])

## Label data
vnames <- tolower(names(data))
names(data) <- vnames
dimnames(pred) <- list(vnames, vnames)

## some edits
pred[1,] <- 0
pred[,1] <- 0


## ----popimpute, cache=TRUE-----------------------------------------------
imp1 <- mice(data, pred = pred, maxit = 20,
             seed = 51121, print = FALSE)

## ----popstrace, echo=FALSE, fig.height=4-------------------------------
trellis.par.set(list(layout.heights = list(strip = 3)))
plot(imp1, c("a10u"), layout = c(2, 1))
trellis.par.set(list(layout.heights = list(strip = 1.3)))

## ----popsbwplot1, echo=FALSE, fig.height=4-------------------------------
thickBoxSettings <- list(box.rectangle=list(lwd=1.5), box.umbrella=list(lwd=1.5))
bwplot(imp1, iq + coping ~ .imp,
       par.settings = thickBoxSettings)

## ----impute2, cache=TRUE-------------------------------------------------
pred[61:86, 61:86] <- 0
imp2 <- mice(data, pred = pred, maxit = 20,
             seed = 51121, print = FALSE)

## ----popsbwplot2, echo=FALSE, fig.height=4-------------------------------
bwplot(imp2, iq + coping ~ .imp,
       par.settings = thickBoxSettings)


## ----vlginit, echo=FALSE-------------------------------------------------
dataproject <- "Data/vlg"
.store <- file.path(dataproject,"R/store/")

## ----vlgaugment----------------------------------------------------------
nimp <- c(400, 600, 75, 300, 200, 400)
regcat <- c("North", "City", "North", "East", "North", "City")
reg <- rep(regcat, nimp)
nimp2 <- floor(rep(nimp, each = 2)/2)
nimp2[5:6] <- c(38, 37)
sex <- rep(rep(c("boy", "girl"), 6), nimp2)
minage <- rep(c(0, 0, 10, 10, 14, 14), nimp)
maxage <- rep(c(10, 10, 14, 14, 21, 21), nimp)
set.seed(42444)
age <- runif(length(minage), minage, maxage)
id <- 600001:601975
data("fdgs", package = "mice")
pad <- data.frame(id, reg, age, sex,
                  hgt = NA, wgt = NA, hgt.z = NA, wgt.z = NA)
data <- rbind(fdgs, pad)

## ----sdsbyregion, echo=FALSE, fig.height=4-------------------------
## inspect the age by region pattern
means <- aggregate(data$hgt.z, by=list(reg=data$reg,
             age=floor(data$age)), mean, na.rm=TRUE)
col <- c("skyblue", "green", "red", "brown", "grey")
tp <- xyplot(x~age, means, group=reg, type=c("g","l"),
             lwd = 1.3,
             lty=1:5, col = col, pch = 19,
             xlim=c(-1,22), ylim=c(-0.6, 0.8),
             ylab="Height (SDS)", xlab="Age (years)",
             key=list(
               text=list(levels(means$reg)),
               lines=list(lty=1:5, col = col),
               x=0.1, y=0.98, background="white",
               columns=3, between.columns=0),
             scales=list(x=list(tck=c(1,0)),
               y=list(tck=c(1,0))))
print(tp)

## ----vlgimpute, cache = TRUE---------------------------------------------
form <- list(hgt.z ~ reg + age + sex + wgt.z +
               I((age - 10) * wgt.z) + age * reg,
             wgt.z ~ reg + age + sex + hgt.z +
               I((age - 10) * hgt.z) + age * reg)
imp <- mice(data, meth = "norm", form = form, m = 10,
            maxit = 20, seed = 28107, print = FALSE)

## ----vlginspect, echo=FALSE, fig.height=4--------------------------------
cda <- mice::complete(imp, "long", include=TRUE)
means2 <- aggregate(cda$hgt.z, by=list(reg=cda$reg, age=floor(cda$age), imp=cda$.imp), mean, na.rm=TRUE)
xyplot(x~age|reg,means2, group=imp, subset=(reg=="North"|reg=="City"),
              type=c("g","l"),lwd=c(4, rep(1,imp$m)),
              lty=1:5, col=c(mdc(4), rep(mdc(6),imp$m)),
              ylab="Height (SDS)", xlab="Age (years)",
              ylim=c(-0.5,0.8), xlim=c(-2,23),
              scales=list(x=list(alternating=FALSE, tck=c(1,0)),
                y=list(tck=c(1,0))),
                strip = strip.custom(bg="grey95"))

## ----finalheight, echo=FALSE, fig.height=4------------------------
load("data/vlg/boyslms")
load("data/vlg/girlslms")

## plot the mcurves for 16+
lms <- boyslms
mcurvesm <- data.frame(sex="Boys", imp=rep(0:10,each=nrow(lms[[1]])),
                      age=rep(lms[[1]]$age,11),
                      m=c(lms[[1]]$m, lms[[2]]$m, lms[[3]]$m, lms[[4]]$m, lms[[5]]$m,
                      lms[[6]]$m, lms[[7]]$m, lms[[8]]$m, lms[[9]]$m, lms[[10]]$m,
                      lms[[11]]$m))
lms <- girlslms
mcurvesf <- data.frame(sex="Girls", imp=rep(0:10,each=nrow(lms[[1]])),
                      age=rep(lms[[1]]$age,11),
                      m=c(lms[[1]]$m, lms[[2]]$m, lms[[3]]$m, lms[[4]]$m, lms[[5]]$m,
                      lms[[6]]$m, lms[[7]]$m, lms[[8]]$m, lms[[9]]$m, lms[[10]]$m,
                      lms[[11]]$m))
mcurves <- rbind(mcurvesm, mcurvesf)

tpm <- xyplot(m~age|sex, groups=imp, data=mcurves, xlab="Age (years)", ylab="Height (cm)",
       col=c(mdc(4),rep(mdc(6),10)), lwd=c(5,rep(0.8,10)), lty=c(2,rep(1,10)),
       type=c("l"),
       xlim=c(15.8,21.2),
       ylim=list(c(178,185),c(165,172)),
       scales=list(y = list(relation="free",
                     at=list(seq(from=178,to=185,2), seq(165,172,2))),
                   x = list(alternating=FALSE), tck=c(1,0)),
       panel = function(...) {
         panel.abline(h=165:185, v=16:21, col="grey90")
         panel.xyplot(...)
       },
       strip = strip.custom(bg="grey95"))
print(tpm)


## ----ch11, child = "src/ch11.Rnw"----------------------------------------

## ----init11, echo = FALSE, results = 'hide'------------------------------
opts_chunk$set(fig.path = 'fig/ch11-', self.contained = FALSE)
pkg <- c("mice", "haven", "lme4", "splines")
loaded <- sapply(pkg, require, character.only = TRUE,
                 warn.conflicts = FALSE, quietly = TRUE)

## ----fddpattern, duo = TRUE, fig.height=4, echo=FALSE--------------------
yvars <- c("yc1", "yc2", "yc3", "yp1", "yp2", "yp3")
z <- md.pattern(fdd[fdd$pp == "Y", yvars])
z <- md.pattern(fdd[fdd$pp == "N", yvars])

## ----fddpred-------------------------------------------------------------
vars <-c("ypa1", "ypb1", "ypc1",
         "ypa2", "ypb2", "ypc2",
         "ypa3", "ypb3", "ypc3")
fdd.pred[vars[1:3], vars]

## ----fddimpute, cache = TRUE, warning = FALSE----------------------------
meth <- make.method(fdd)
meth["yc1"] <- "~I(yca1 + ycb1 + ycc1)"
meth["yc2"] <- "~I(yca2 + ycb2 + ycc2)"
meth["yc3"] <- "~I(yca3 + ycb3 + ycc3)"
meth["yp1"] <- "~I(ypa1 + ypb1 + ypc1)"
meth["yp2"] <- "~I(ypa2 + ypb2 + ypc2)"
meth["yp3"] <- "~I(ypa3 + ypb3 + ypc3)"
imp <- mice(fdd, pred = fdd.pred, meth = meth, maxit = 20,
            seed = 54434, print = FALSE)

## ----fddreshape----------------------------------------------------------
lowi <- mice::complete(imp, "long", inc=TRUE)
lowi <- data.frame(lowi,cbcl2=NA, cbin2=NA,cbex2=NA)
lolo <- reshape(lowi, idvar = 'id',
                varying = 11:ncol(lowi),
                direction = "long",
                new.row.names = 1:(nrow(lowi)*3),
                sep="")
lolo <- lolo[order(lolo$.imp, lolo$id, lolo$time),]
row.names(lolo) <- 1:nrow(lolo)

## ----fddplotimp, echo = FALSE--------------------------------------------
iv <- is.na(lolo[lolo$.imp==0,]$yp)
ivn <- ifelse(iv,1,0)
col12  <- c("grey80","grey80",
            mdc(2),mdc(1),
            mdc(2),"transparent",
            mdc(2),"transparent",
            mdc(2),"transparent",
            mdc(2),"transparent")

ic <- unique(lolo$id[iv])
ss <- lolo$id %in% ic

grp <- 2*as.integer(lolo$.imp) - ivn
loloss <- data.frame(lolo, grp=grp)
trellis.par.set(strip.background=list(col="grey95"))
trellis.par.set(list(layout.heights = list(strip = 1)))
tp1 <- xyplot(yp~time|factor(id), data=loloss, type="l",
              layout = c(4,4),
      groups=factor(.imp), col="grey80", subset=ss,
       ylab="UCLA-RI Parent Score", pch=19, cex=1,
       xlab="Time", xlim=c("T1","T2","T3"),
             as.table=TRUE)
print(tp1)
tp2 <- xyplot(yp~time|factor(id), data=loloss, type="p",
              layout = c(4,4),
       groups=grp, col=col12, subset=ss,
       ylab="UCLA-RI Parent Score",
       pch=19,
       cex=0.8,
       xlab="Time", xlim=c("T1","T2","T3"),
       as.table=TRUE)
print(tp2, newpage=FALSE)

## ----fddplotparent, fig.height=3.5, echo=FALSE---------------------------
means <- aggregate(lolo$yp, list(lolo$.imp!=0,lolo$trt,lolo$time), mean, na.rm=TRUE)
names(means) <- c(".imp","trt","time","yp")
levels(means$trt) <- c("EMDR","CBT")
tp <- xyplot(yp~time|trt, data=means, type="o", pch=19,
      groups=factor(.imp), col=c(mdc(4), mdc(6)),
       ylab="UCLA-RI Parent Score", lwd=2,
       xlab="Time", xlim=c("T1","T2","T3"))
print(tp)

## ----fddplotchild, fig.height=3.5, echo=FALSE----------------------------
means <- aggregate(lolo$yc, list(lolo$.imp!=0,lolo$trt,lolo$time), mean, na.rm=TRUE)
names(means) <- c(".imp","trt","time","yc")
levels(means$trt) <- c("EMDR","CBT")
tp <- xyplot(yc~time|trt, data=means, type="o", pch=19,
      groups=factor(.imp), col=c(mdc(4),mdc(6)),
       ylab="UCLA-RI Child Score", lwd=2,
       xlab="Time", xlim=c("T1","T2","T3"))
print(tp)

## ----tbcinit, echo=FALSE-------------------------------------------------
load("data/tbc/imp.1745")
load("data/tbc/fit.hgt")
load("data/tbc/fit.wgt")
load("data/tbc/fit.bmi")

original <- read_spss(file = "data/tbc/long_spss2splus.sav")
names(original) <- c("id", "age", "occ", "nocc", "sex",
                    "hgt",  "wgt", "bmi",
                    "hgt.z", "wgt.z", "bmi.z", "wfh.z", "sys140")
broad <- read_spss(file = "data/tbc/broad_spss2splus.sav")
broad <- as_factor(broad)
names(broad) <- tolower(names(broad))
target <- data.frame(id=broad$koppel, ao = broad$ovjv, bmi.z.jv = broad$b_sdjv)

# merge the target variables
original <- merge(original, target, all.x = TRUE)

### define subsets
subset <- !is.na(original$age) & !(is.na(original$hgt.z) & is.na(original$wgt.z) & is.na(original$bmi.z))
tbc <- original[subset, c("id","occ","nocc","age","sex",
                          "hgt.z","wgt.z","bmi.z","ao")]
tbc <- tbc[order(tbc$id, tbc$age),]

### define useful administrative variables
ord <- tbc$id
first <- diff(ord)>=1
typ <- factor(rep("obs",nrow(tbc)),levels=c("obs","sup","pred"))
tbc <- data.frame(tbc[,1:3],first=c(TRUE,first),typ,tbc[4:ncol(tbc)])

### make small dataset
six <- c(1259,7019,2447,7460,8046,7646)
set.seed(23221)
sample <- unique(c(six,sample(unique(tbc$id),300)))
idx <- (tbc$id) %in% sample
data <- tbc[idx,]

### select all data
data <- tbc

### remove those with nocc 1 or 2 or 3
data <- data[data$nocc >= 3,]

### missing data heat map
tbc <- data

## ----tbcspline-----------------------------------------------------------
library(splines)
data <- tbc

### specify break ages
brk <- c(0, 8/365, 1/3, 1, 2, 6, 10, 18, 29)
k <- length(brk)

### calculate B-spline
X <- bs(data$age,
        knots = brk,
        B = c(brk[1], brk[k] + 0.0001),
        degree = 1)
X <- X[,-(k + 1)]
dimnames(X)[[2]] <- paste("x", 1:ncol(X), sep = "")
data <- cbind(data, X)
round(head(X, 3), 2)

## ----tbcfitstick, eval=FALSE---------------------------------------------
library(lme4)
fit <- lmer(wgt.z ~ 0 + x1 + x2 + x3 + x4 + x5 +
              x6 + x7 + x8 + x9 + (0 + x1 + x2 +
              x3 + x4 + x5 + x6 + x7 + x8 + x9 | id),
            data = data)

### calculate size and increment per person
tsiz <- t(ranef(fit)$id) + fixef(fit)
tinc <- diff(tsiz)

round(head(t(tsiz)), 2)

## ----tbctimewarp, echo=FALSE---------------------------------------------
warp.setup <- data.frame(age = brk,
                         age2 = seq(0, 29, length.out=k))
warp.model <- lm(
  age2 ~ bs(age, knots = brk[c(-1, -k)], degree = 1) - 1,
  data = warp.setup, x = TRUE, y = TRUE)
warped.knots <- warp.model$y
maxage <- max(warped.knots)
age2  <- predict(warp.model, newdata = data)
data  <- cbind(data, age2 = age2)
rm(age2)
id <- unique(data$id)
data2 <- appendbreak(data, brk, id = id,
                     warp.model = warp.model, typ = "sup")
table(data2$typ)

## ----tbcdecimal, echo=FALSE----------------------------------------------
options(digits = 2)

## ----tbcappenddata-------------------------------------------------------
head(data2)

## ----tbcimpmeth----------------------------------------------------------
Y <- c("hgt.z", "wgt.z", "bmi.z")
meth <- make.method(data2)
meth[1:length(meth)] <- ""
meth[Y] <- "2l.pan"

## ----tbcimppred----------------------------------------------------------
pred <- make.predictorMatrix(data2)
pred[1:nrow(pred), 1:ncol(pred)] <- 0
pred[Y, "id"] <- (-2)
pred[Y, "sex"] <- 1
pred[Y, paste("x", 2:9, sep = "")] <- 1
pred[Y[1], Y[2]] <- 1
pred[Y[2], Y[1]] <- 1
pred[Y[3], Y[1:2]] <- 1

## ----tbcimpvis, echo=FALSE-----------------------------------------------
vis <- c("hgt.z","wgt.z","bmi.z")

## ----tbcimpmice, eval=FALSE----------------------------------------------
imp.1745 <- mice(data2, meth = meth, pred = pred, m = 10,
                 maxit = 10, seed = 52711, print = FALSE)

## ----tbcplotimp,  echo=FALSE---------------------------------------------
cd <- mice::complete(imp.1745, "long")
sup <- cd[cd$typ=="sup", ]
# sup <- cd[cd$typ=="imp",-c(2,10:12)]
# sup$ao <- NA
data3 <- data.frame(.imp=0,data2)
data3 <- rbind(data3, sup[,-2])

# prepare for plotting
idx <- data3$id %in% six
pd  <- data3[idx,]
pd$id <- as.factor(pd$id)
pd$grp <- pd$.imp
pd$grp[pd$grp==0] <- NA
pd$grp[pd$typ=="obs"] <- 11
pd$grp <- reorder(pd$grp, as.numeric(pd$grp))

# now plot
tbcimp <- xyplot(wgt.z~age2|id, data=pd, xlim=c(-1,maxage+1), ylim=c(-4,4),
       as.table = TRUE,
       scales = list(x=list(at=warped.knots,
              labels=c("0","8d","4m","1y","2y","6y","10y","18y","29y"))),
       xlab = "Age", ylab = "Weight SDS",
       groups=grp, layout = c(2,3),
       pch=c(rep(20,10), 19),
       type=c(rep("l",10),"p"), lwd=c(rep(1,10),1),
       col=c(rep(mdc(5),10),mdc(1)), distribute.type=TRUE,
       panel = function(...) {
         panel.abline(v=warped.knots, lty=2, col="grey80")
         panel.abline(h=c(-2,0,2), lty=1, col="grey80")
         panel.xyplot(...)
       },
       strip=strip.custom(bg="grey95")
       )

print(tbcimp)


## ----tbchw, eval=TRUE,  echo=FALSE---------------------------------------
### prepare for plotting
cd <- mice::complete(imp.1745, 1)
idx <- (cd$id) %in% sample
cd <- cd[idx,]

shingle <- cut(cd$age,breaks=c(brk,29.01),right=FALSE, inc=TRUE,
               labels=c("0d-8d","8d-4m","4m-1y","1y-2y","2y-6y",
                 "6y-10y","10y-18y","18y-29y","29y"))
if (empty_figure) {
  plot.new()
  } else {
tbchw <- xyplot(wgt.z~hgt.z|shingle,data=cd,
       xlim=c(-4,4), ylim=c(-4,4), type=c("p","g"),
       group=(typ=="sup"),pch=c(1,20),
       col=c(mdc(1:2)),
       xlab="Height SDS", ylab="Weight SDS",pty="s",
       strip=strip.custom(bg="grey95"))
print(tbchw)
}

## ----tbccomplete,echo=FALSE,eval=FALSE-----------------------------------
imp <- imp.1745
cd <- mice::complete(imp,"long")
sup <- cd[cd$typ=="sup",]

sup$age <- round(sup$age,2)
sup$hgt.z <- round(sup$hgt.z,2)
sup$wgt.z <- round(sup$wgt.z,2)
sup$bmi.z <- round(sup$bmi.z,2)

lowi <- reshape(sup, idvar=c('id','.imp'), timevar = 'age',
                v.names=c('hgt.z','wgt.z','bmi.z'),
                direction="wide",
                drop=c(".id","occ","first","typ","hgt","wgt","bmi",
                  paste("x",1:9,sep=""),"age2"))
hsiz <- lowi[,c(".imp","id","nocc","sex",
                "hgt.z.0","hgt.z.0.02","hgt.z.0.33",
                "hgt.z.1","hgt.z.2","hgt.z.6","hgt.z.10","hgt.z.18",
                "hgt.z.29")]
wsiz <- lowi[,c(".imp","id","nocc","sex",
                "wgt.z.0","wgt.z.0.02","wgt.z.0.33",
                "wgt.z.1","wgt.z.2","wgt.z.6","wgt.z.10","wgt.z.18",
                "wgt.z.29")]
bsiz <- lowi[,c(".imp","id","nocc","sex",
                "bmi.z.0","bmi.z.0.02","bmi.z.0.33",
                "bmi.z.1","bmi.z.2","bmi.z.6","bmi.z.10","bmi.z.18",
                "bmi.z.29")]
# merge outcome data
bsiz <- merge(bsiz, target,all.x=TRUE)


hinc <- cbind(hsiz[,1:4],t(diff(t(hsiz[,-1:-4]))))
winc <- cbind(wsiz[,1:4],t(diff(t(wsiz[,-1:-4]))))
binc <- cbind(bsiz[,1:4],t(diff(t(bsiz[,-1:-4]))))

# merge outcome data
binc <- merge(binc, target,all.x=TRUE)

bmi.z.0.02 <- by(binc, binc$.imp, function(x) lm(bmi.z.0.02~ao, data = x, na.action='na.omit'))
bmi.z.0.33 <- by(binc, binc$.imp, function(x) lm(bmi.z.0.33~ao, data = x, na.action='na.omit'))
bmi.z.1    <- by(binc, binc$.imp, function(x) lm(bmi.z.1   ~ao, data = x, na.action='na.omit'))
bmi.z.2    <- by(binc, binc$.imp, function(x) lm(bmi.z.2   ~ao, data = x, na.action='na.omit'))
bmi.z.6    <- by(binc, binc$.imp, function(x) lm(bmi.z.6   ~ao, data = x, na.action='na.omit'))
bmi.z.10   <- by(binc, binc$.imp, function(x) lm(bmi.z.10  ~ao, data = x, na.action='na.omit'))
bmi.z.18   <- by(binc, binc$.imp, function(x) lm(bmi.z.18  ~ao, data = x, na.action='na.omit'))
bmi.z.29   <- by(binc, binc$.imp, function(x) lm(bmi.z.29  ~ao, data = x, na.action='na.omit'))

tab.bmi.z.0.02 <- summary(pool(as.mira(bmi.z.0.02)))
tab.bmi.z.0.33 <- summary(pool(as.mira(bmi.z.0.33)))
tab.bmi.z.1    <- summary(pool(as.mira(bmi.z.1)))
tab.bmi.z.2    <- summary(pool(as.mira(bmi.z.2)))
tab.bmi.z.6    <- summary(pool(as.mira(bmi.z.6)))
tab.bmi.z.10   <- summary(pool(as.mira(bmi.z.10)))
tab.bmi.z.18   <- summary(pool(as.mira(bmi.z.18)))
tab.bmi.z.29   <- summary(pool(as.mira(bmi.z.29)))

(bmi.z.0.33 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.0.33+bmi.z.0.02, data = x, na.action='na.omit'))))))
(bmi.z.1 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.1+bmi.z.0.33, data = x, na.action='na.omit'))))))
(bmi.z.2 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.2+bmi.z.1, data = x, na.action='na.omit'))))))
(bmi.z.6 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.6+bmi.z.2, data = x, na.action='na.omit'))))))
(bmi.z.10 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.10+bmi.z.6, data = x, na.action='na.omit'))))))
(bmi.z.18 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.18+bmi.z.10, data = x, na.action='na.omit'))))))

(bmi.z.6 <- summary(pool(as.mira(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.6+bmi.z.2, data = x, na.action='na.omit'))))))
summary(by(bsiz, bsiz$.imp, function(x) lm(bmi.z.jv~bmi.z.6+bmi.z.2, data = x, na.action='na.omit'))[[2]])


# repeat the analysis for the broken stick
bsiz.bs <- as.data.frame(t(t(ranef(fit.bmi)[[1]]) + fixef(fit.bmi)))
dimnames(bsiz.bs)[[2]] <- names(bsiz)[5:13]
binc.bs <- t(diff(t(bsiz.bs)))
bsiz.bs <- data.frame(id=bsiz[bsiz$.imp==1,"id"], bsiz.bs)
binc.bs <- data.frame(id=bsiz[bsiz$.imp==1,"id"], binc.bs)
bsiz.bs <- merge(bsiz.bs, target)
binc.bs <- merge(binc.bs, target)

bmi.z.0.02.bs <- lm(bmi.z.0.02~ao, data = binc.bs, na.action='na.omit')
bmi.z.0.33.bs <- lm(bmi.z.0.33~ao, data = binc.bs, na.action='na.omit')
bmi.z.1.bs    <- lm(bmi.z.1   ~ao, data = binc.bs, na.action='na.omit')
bmi.z.2.bs    <- lm(bmi.z.2   ~ao, data = binc.bs, na.action='na.omit')
bmi.z.6.bs    <- lm(bmi.z.6   ~ao, data = binc.bs, na.action='na.omit')
bmi.z.10.bs   <- lm(bmi.z.10  ~ao, data = binc.bs, na.action='na.omit')
bmi.z.18.bs   <- lm(bmi.z.18  ~ao, data = binc.bs, na.action='na.omit')

summary(bmi.z.0.02.bs)
summary(bmi.z.0.33.bs)
summary(bmi.z.1.bs)
summary(bmi.z.2.bs)
summary(bmi.z.6.bs)
summary(bmi.z.10.bs)
summary(bmi.z.18.bs)

a<-round(cor(bsiz[,-(1:4)],use="pair"),2)
b<-round(cor(bsiz.bs,use="pair"),2)

a2<-round(cor(bsiz[!is.na(bsiz$ao),-(1:4)],use="complete.obs"),2)
b2<-round(cor(bsiz.bs[!is.na(bsiz$ao),],use="complete.obs"),2)

## ----ch12, child = "src/ch12.Rnw"----------------------------------------

## ----init12, echo = FALSE, results = 'hide'------------------------------
opts_chunk$set(fig.path = 'fig/ch12-', self.contained = FALSE)

