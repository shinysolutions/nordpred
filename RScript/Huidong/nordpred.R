# library(statmod)
nordpred <- function(cas, pyr, periods = NULL, startAge = NULL, linkfunc = "power5") {
  
  ## Trim datasets;
  cas <- cas[, intersect(names(cas), names(pyr))]
  pyr <- pyr[, which(names(pyr) == names(cas)[1]):ncol(pyr)]
  
  ## Dimension of data frames;
  dcas <- dim(cas)
  dpyr <- dim(pyr)
  
  ## Meet requirments;
  if (dcas[1]!=18 || dpyr[1]!=18) {
    stop("\"cas\" and \"pyr\" must have data for 18 age groups!")	
  }
  if (dcas[2] < 3) {
    stop("Too few periods in \"cas\", or the names of \"cas\" and \"pyr\" don't match! ")
  }
  if (dcas[2] == dpyr[2]) {
    stop("\"pyr\" must include future periods (more periods than \"cas\" for predicting)!")	
  }
  if ((dpyr[2] - dcas[2]) > 5) {
    stop("Package can not predict more than 5 periods (given by sizes of \"pyr\" and \"cas\")")	
  }
  
  ## Age-Period-Cohort data.frame;
  APCdata <- data.frame(y = c(as.matrix(pyr[, 1:dcas[2]])))
  APCdata$Age    <- rep(1:dcas[1], dcas[2])
  APCdata$Period <- rep(1:dcas[2], each = dcas[1])
  APCdata$Cohort <- dcas[1] - APCdata$Age + APCdata$Period
  APCdata$Cases  <- c(as.matrix(cas))
  
  ## Periods
  if (is.null(periods)) {
    periods <- rev(c(min(dcas[2], 4):min(dcas[2], 6)))
  }
  # Creation of power link:
  power5 <- poisson()
  power5$link <- "0.2 root link Poisson family"
  power5$linkfun <- function(mu)  { (mu)^0.2 }
  power5$linkinv <- function(eta) { eta^5 }
  power5$mu.eta <- function(eta)  { 5*eta^4 }
  
  mod <- NULL
  for (p in periods) {
    if (is.null(mod)) {
      apcdata <- APCdata[which(APCdata$Period > (dcas[2]- p)), ]
      if (is.null(startAge)) {
        startAge <- which(sapply(1:18, function(x) {min(cas[x, ])}) >= 10)[1]
      }
      apcdata <- apcdata[which(apcdata$Age >= startAge), ]
      
      ## Model fitting;
      if (linkfunc=="power5") {
        mod.cand <- glm(Cases ~ as.factor(Age) + Period + as.factor(Period) + as.factor(Cohort) + offset((y)^.2) - 1, family = power5,  data = apcdata)
      } else {
        mod.cand <- glm(Cases ~ as.factor(Age) + Period + as.factor(Period) + as.factor(Cohort) + offset(log(y)) - 1, family = poisson, data = apcdata)
      } 
      pvalue <- 1 - pchisq(mod.cand$deviance, mod.cand$df.residual)
      
      ## Suggest for "Recent";
      mod1 <- glm(Cases ~ as.factor(Age) + Period + as.factor(Cohort) + offset(log(y)) -1,family=poisson, data=apcdata)
      mod2 <- glm(Cases ~ as.factor(Age) + Period + I(Period^2) + as.factor(Cohort) + offset(log(y)) -1,family=poisson, data=apcdata)
      pdiff <- anova(mod1, mod2, test="Chisq")[["Pr(>Chi)"]][2]
      recent <- (pdiff < 0.05)
      
      ## selected model
      if (pvalue < 0.05 | length(periods) == 1) {
        mod <- mod.cand
        P <- p
      }
    }
  }
  
  # Set class and return results
  res <- list(mod      = mod,
              pvalue   = pvalue,
              recent   = recent,
              cas      = cas,
              pyr      = pyr,
              periods  = P,
              startAge = startAge,
              apcdata  = apcdata,
              APCdata  = APCdata)
  
  class(res) <- "nordpred"
  return(res)
}

