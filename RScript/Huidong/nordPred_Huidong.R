# # Reading data (Colon cancer for Norwegian males)
# cases  <- read.table("colon-men-Norway.txt",header =T,sep=",",row.names=1)
# inpop1 <- read.table("men-Norway.txt",header =T,sep=",",row.names=1)
# inpop2 <- read.table("men-Norway-pred.txt",header =T,sep=",",row.names=1)
# pyr <- cbind(inpop1,inpop2)
# # Get results with standardisation:
# wstand <- c(0.12, 0.1, 0.09, 0.09, 0.08, 0.08, 0.06, 0.06, 0.06, 0.06,0.05, 
#             0.04, 0.04, 0.03, 0.02, 0.01, 0.005, 0.005)

#########################################################
nordpred <- function(cases, pyr, startestage = 4, noperiods = NULL, recent = NULL, linkfunc = "power5") {
  
  ## Dimension of data frames;
  dim.cas <- dim(cases)
  dim.pyr <- dim(pyr)
  
  ## Meet requirments;
  if (dim.cas[2] < 3) {
    stop('Too few periods in "cases"')
  }
  if (dim.cas[1]!=18 || dim.pyr[1]!=18) {
    stop("\"cases\" and \"pyr\" must have data for 18 age groups")	
  }
  if (dim.cas[2] > dim.pyr[2]) {
    stop("\"pyr\" must include information about all periods in \"cases\"")	
  }
  if (dim.pyr[2] == dim.cas[2]) {
    stop("\"pyr\" must include information on future rates (E.g. must be greater than \"cases\")")	
  }
  if ((dim.pyr[2]-dim.cas[2]) > 5) {
    stop("Package can not predict more than 5 periods (given by sizes of \"pyr\" and \"cases\")")	
  }
#   if (dim.cas[2] < noperiod) {
#     stop("More periods specified in \"noperiod\" than columns in \"cases\"")   	
#   }
#   if (noperiod < 3) {
#     stop("\"noperiod\" must be 3 or larger to get enough for estimating")   	
#   }  
  
  ## 0.2 root link Poisson family;
  power5 <- poisson()
  power5$link <- "^0.2"
  power5$linkfun <- function(mu)  { mu^0.2 }
  power5$linkinv <- function(eta) { eta^5 }
  power5$mu.eta  <- function(eta) { 5*eta^4 }
  
  
  ## Age-Period-Cohort data.frame;
  APCdata <- data.frame(Cases = c(as.matrix(cases)))
  APCdata$Age <- rep(1:dim.cas[1], dim.cas[2])
  APCdata$Period <- rep(1:dim.cas[2], each = dim.cas[1])
  APCdata$Cohort <- dim.cas[1] - APCdata$Age + APCdata$Period
  APCdata$y <- c(as.matrix(pyr[,which(names(pyr) %in% names(cases))]))
  
  
  # Setting contrast:
  options(contrasts=c("contr.treatment","contr.poly"))
  
  # Candidate periods in prediction base;
  if (is.null(noperiods)) {
    noperiods <- min(dim.cas[2], 4):min(dim.cas[2], 6)
  }
  noperiods <- sort(noperiods)

  ## Choose number of periods;
  while(length(noperiods)>1) {
    # Selecting data for regression:
    apcdata <- APCdata[which(APCdata$Age >= startestage & APCdata$Period > (dim.cas[2] - max(noperiods))),]
    
    # Setting contrast:
    options(contrasts=c("contr.treatment","contr.poly"))
    
    ## Model fitting;
    if (linkfunc=="power5") {
      mod <- glm(Cases ~ as.factor(Age) + Period + as.factor(Period) + as.factor(Cohort) + offset(y^0.2) - 1, 
                 family = power5, data = apcdata)
    } else {
      mod <- glm(Cases ~ as.factor(Age) + Period + as.factor(Period) + as.factor(Cohort) + offset(log(y)) - 1, 
                 family = poisson, data = apcdata)
    } 
    ## p-value;
    pvalue<- 1 - pchisq(mod$deviance, mod$df.residual)
    if (pvalue >= 0.01) {
      print(pvalue)
      noperiods <- noperiods[-length(noperiods)]
    } else {
      noperiods <- max(noperiods)
    }
  }      
  noperiod <- noperiods
  
  # Basis for setting suggestion for 'recent' (whether to use recent trend or average trend)
  mod1 <- glm(Cases ~ as.factor(Age) + Period + as.factor(Cohort) + offset(log(y)) - 1,
              family = poisson, data = apcdata)
  mod2 <- glm(Cases ~ as.factor(Age) + Period + I(Period^2) + as.factor(Cohort) + offset(log(y)) -1,
              family = poisson, data = apcdata)
  pdiff <- anova(mod1,mod2,test="Chisq")$"Pr(>Chi)"[2] 
  
  # use last two periods (5years*2) to predict if period square is significant, else use whole drift
  suggestionrecent <- ifelse(pdiff < 0.05, TRUE, FALSE)
  
  # Set class and return results
  res <- list(mod = mod,
              cases = cases,
              data = apcdata,
              pyr = pyr,
              noperiod = noperiod, 
              pvalue = pvalue,
              startestage = startestage,
              suggestionrecent = suggestionrecent, 
              pdiff = pdiff,
              linkfunc = linkfunc)
  class(res) <- "nordpred"
  return(res)
}

object <- nordpred(cases = cases, pyr = pyr, startestage = 4)
mod <- object$mod



predict.nordpred <- function(object, type = c("link", "response", "terms"),
                             startuseage = 4,recent = NULL,cuttrend=c(0,.25,.5,.75,.75),
                             incidence=T,standpop=NULL,excludeobs=F,byage =T,agegroups="all") {
  
  ## Requirment;
  if (class(object)!="nordpred") {
    stop("Variable \"object\" must be of type \"nordpred.estimate\"")	
  } 
  if (object$startestage>startuseage) {
    stop("\"startuseage\" is set to high compared to \"startestage\" in \"object\"") 	
  }
  if (length(cuttrend)<(dim(object$pyr)[2]-dim(object$cases)[2])) {
    stop("\"cuttrend\" must always be at least the same length as \n the number of periods with population forecasts")
  }

  ## Retrive dataset;
  cases <-  object$cases
  pyr	  <-  object$pyr
  data	  <-  object$data
  dim.cas <- dim(cases)
  dim.pyr <- dim(pyr)
  
  
  ## New data
  APCdata <- data.frame(y = c(as.matrix(pyr)))
  APCdata$Age <- rep(1:dim.pyr[1], dim.pyr[2])
  APCdata$Period <- rep(1:dim.pyr[2], each = dim.pyr[1])
  APCdata$Cohort <- dim.pyr[1] - APCdata$Age + APCdata$Period
  
  APCdata$Period[which(APCdata$Period > noobsper)] <- noobsper
  APCdata$Cohort[which(APCdata$Cohort > max(data$Cohort))] <- max(data$Cohort)
  
  apcdata <- APCdata[which(APCdata$Age >= 4 & APCdata$Period > (dim.cas[2] - max(noperiods))),]
  
  
  apcdata$Cases <- predict(mod, type = "response", newdata = apcdata)
  
  # Calculate predictions in number of cases:
  for (age in 1:(startuseage-1)) {
    # For agegroups with litle data, we use mean incidence for last two periods:
    obsinc <- cases[age,(noobsper-1):noobsper]/pyr[age,(noobsper-1):noobsper]
    if (sum(is.na(obsinc))) {
      obsinc[is.na(obsinc)] <- 0 	
    }
    datatable[age,(noobsper+1):nototper] <- ((obsinc[,1]+obsinc[,2])/2)*pyr[age,(noobsper+1):nototper]
  }

  
}
  
