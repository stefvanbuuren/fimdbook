# functions.ssc

count.boys <- function(data)
{
  ## produces tables for boys
  ## resets objects boy.count.pubic and boy.count.scrotum

  x <- cut(data$age, breaks = seq(8.75, 20.5, 0.25))
 
  z <- as.data.frame(by.to.matrix(by.data.frame(data, x, count.phb)))
  z <- cbind(levels(x),z)
  names(z) <- c("age","mean", "rec", "NA", "n", "P1", "P2", "P3", "P4", "P5", "P6")
  boy.count.phb <<- z
  
  z <- as.data.frame(by.to.matrix(by.data.frame(data, x, count.gen)))
  z <- cbind(levels(x),z)
  names(z) <- c("age","mean", "rec", "NA", "n", "G1", "G2", "G3", "G4", "G5")
  boy.count.gen <<- z
} 

count.phb <- function(data) {
  ntot <- nrow(data)
  empty <- sum(is.na(data$phb))
  sub <- data[!is.na(data$phb),]
  n <- nrow(sub)
  age <- mean(sub$age)
  if(n > 0.) {
    codes <- as.integer(sub$phb)
    P1 <- sum(codes >= 1)
    P2 <- sum(codes >= 2)
    P3 <- sum(codes >= 3)
    P4 <- sum(codes >= 4)
    P5 <- sum(codes >= 5)
    P6 <- sum(codes >= 6)
  }
  else {
    P1 <- P2 <- P3 <- P4 <- P5 <- P6 <- NA
  }
  c(age, ntot, empty, n, P1, P2, P3, P4, P5, P6)
}

count.gen <- function(data) {
  ntot <- nrow(data)
  empty <- sum(is.na(data$gen))
  sub <- data[!is.na(data$gen),]
  n <- nrow(sub)
  age <- mean(sub$age)
  if(n > 0.) {
    codes <- as.integer(sub$gen)
    G1 <- sum(codes >= 1)
    G2 <- sum(codes >= 2)
    G3 <- sum(codes >= 3)
    G4 <- sum(codes >= 4)
    G5 <- sum(codes >= 5)
  }
  else {
    G1 <- G2 <- G3 <- G4 <- G5 <- NA
  }
  c(age, ntot, empty, n, G1, G2, G3, G4, G5)
}


dose.p.gam <- function(model, p = c(0.1, 0.5, 0.9)) {
  age <- seq(8.75, 20.5, 0.25)
  z <- predict(model, newdata = as.data.frame(age), type = "response", safeEvalFrame = sys.parent(1))
  return(approx(x = z, y = age, xout = p))
}

fitTannerStages <- function(data, var = "phg", nstages = 6, counts=boy.count.phg, plot=FALSE, print=FALSE)
{
  ## fitTannerStages
  ## Fits probit, logistic, and several GAM model to Tanner category

   x <- seq(8.75,20.5,0.25)
  ## eliminate uninformative records
  data <- data[!is.na(data[,var]),]
	
  ## create ouput list
  result <- vector("list",nstages-1)
	
  ## loop over categories
  for (stage in 2:nstages)
    {
      if (print) cat("\n --- STAGE ",stage," ---\n")
      
      ## define binary dependent variable
      y <- (as.integer(data[,var]) >= stage)
      data2 <- cbind(data, y=y)
	
      ## calculate raw probabilities (for plotting)
      rawprob <- list(x=counts[,2], y=counts[,5+stage]/counts[,5])
      
      ## fit logistic en probit regression and GAM models
      logit  <- glm(y ~ age, data=data2, family = binomial, na.action = na.omit)
      gam.df2     <- gam(y ~ s(age,df=2.), data=data2, family = binomial, na.action = na.omit)
      gam.df3     <- gam(y ~ s(age,df=3.), data=data2, family = binomial, na.action = na.omit)
      gam.default <- gam(y ~ s(age),       data=data2, family = binomial, na.action = na.omit)

      ## plot fitted curves
      logit.y   <- predict(logit,  data.frame(age=x),type="response")
      gam.df2.y <- predict(gam.df2, data.frame(age=x), type="response")
      gam.df3.y <- predict(gam.df3, data.frame(age=x), type="response")
      gam.default.y <- predict(gam.default, data.frame(age=x), type="response")
      if (plot){
        plot(c(8,21),c(0,1),type="n",xlab="Age",ylab="Proportion")
        title(main=paste("Variable: ", var, "  stage: ",stage),cex=1)
        points(x=rawprob$x, y=rawprob$y)
        x <- seq(8.75, 20.5, 0.25)
        lines(x, logit.y, lty=2)
        lines(x, gam.df2.y, lty=1)
        lines(x, gam.df3.y, lty=1)
        lines(x, gam.default.y, lty=1)
        abline(0.1,0)
        abline(0.5,0)
        abline(0.9,0)
      }
      
      ## calculate P10, P50 and P90 for each model
      logit.p <- format(dose.p.gam(logit)$y,dig=4)
      gam.df2.p <- format(dose.p.gam(gam.df2)$y,dig=4)
      gam.df3.p <- format(dose.p.gam(gam.df3)$y,dig=4)
      gam.default.p <- format(dose.p.gam(gam.default)$y,dig=4)
      if (print) {
        cat("\n")
        cat("\n Percentile estimates\n")
        cat("Logit   P10, P50, P90:", logit.p, "\n")
        cat("Gam df2 P10, P50, P90:", gam.df2.p, "\n")
        cat("Gam df3 P10, P50, P90:", gam.df3.p, "\n")
        cat("Gam dft P10, P50, P90:", gam.default.p, "\n")
        cat("\n")
      }
      ## test models against each other
      ## deviance <- anova(logit, gam.df2, gam.df2.5,gam.df3, gam.default, test = "Chisq")
      deviance <- anova(logit, gam.df2, gam.df3, gam.default, test = "Chisq")
      if (print) print(deviance)
      
      ## store 
      result[[stage-1]] <- list(logit=list(predicted=logit.y,percentiles=logit.p),
                                gam.df2=list(predicted=gam.df2.y,percentiles=gam.df2.p),
                                gam.df3=list(predicted=gam.df3.y,percentiles=gam.df3.p),
                                gam.def=list(predicted=gam.default.y,percentiles=gam.default.p),
                                rawprob=rawprob)										
    }
  return(result)
}


plotTannerStages <- function(fittedModel=boy.phb.result, choice=c(4,4,4,4), title, counts=boy.count.phb) {
  ## plots the final model choice
  
  ## plot frame
  plot(c(8,21),c(0,1),type="n",xlab="Age (years)",ylab="Proportion",lab=c(10,12,5),
       las=1,cex=1,tck=-0.01)
  title(main=title,cex=0.8)
  abline(0.1,0,lty=2)
  abline(0.5,0,lty=2)
  abline(0.9,0,lty=2)
  
  ## plot raw data and fitted curves
  x <- seq(8.75, 20.5, 0.25)
  for (i in 1:length(fittedModel))
    {			
      raw <- length(fittedModel[[i]])
      select <- fittedModel[[i]][[raw]]$y > 0.05 & fittedModel[[i]][[raw]]$y < 0.95
      tx <- fittedModel[[i]][[raw]]$x[select]
      ty <- fittedModel[[i]][[raw]]$y[select]
      ## text(x=tx, y=ty ,labels=i+1,cex=0.5)
      ## lines(x=tx,y=ty, lty=2)
			
      lines(x=x,y=fittedModel[[i]][[choice[i]]]$predicted)
    }
}

by.to.matrix <- function(x)
{
  ## maakt van de gelijksoortige vectors van componenten in de lijst
  ## een matrix. Vooral handig om het resultaat van de by-functie
  ## naar een matrix te converteren.
  ## x: 'list' van klasse 'by'
  ##cat(nrow(x), "\n")
  ##cat(length(x[[1]]), "\n")
  z <- matrix(nrow = nrow(x), ncol = max(unlist(lapply(x,length))))
  dimnames(z) <- list(names(x), names(x[[1.]]))
  for(i in 1.:nrow(x))
    z[i,  ] <- x[[i]]
  z
}
