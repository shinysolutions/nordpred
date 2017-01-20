if (!exists("R.Version")) {
  cat("Remark for S-PLUS users:\n")
  cat("Powerlink is made via a special modification in S-PLUS.\n")
  cat("This works fine for the point estimates, but the variance\n")
  cat("estimates found via the glm-objects are wrong.\n")
  cat("For variance estimates, we would rather recommend using R.\n")
}

nordpred <- function(cases,pyr,startestage,startuseage,noperiods=NULL,recent=NULL,
                     cuttrend=c(0,.25,.5,.75,.75),linkfunc="power5") {
  # Nordpred:   R (www.r-project.org) & S-PLUS (www.insightful.com) functions 
  #             for prediction of cancer incidence (as used in the Nordpred project).
  # Written by:	Bjørn Møller and Harald Fekjaer <hf@kreftregisteret.no>, 2000-2003
  # Version 1.3. updated 2015-28-01 for compabilty with newer R versions 
  # See also:	http://www.kreftregisteret.no/software/nordpred/
  # License:	GNU version 2 

  Rplatform <- exists("R.Version")
  
  # Seting default and checking data:
  percases <- dim(cases)[2]
    # Number of periods in prediction base (observed periods)
  
  if (percases<3) {
    stop("Too few periods in \"cases\"")	
  }
   
  if (is.null(noperiods)) {
    noperiods <- c(min(percases,4):min(percases,6))
      # List possible candidates for number of periods 
      # to base predictions on. Default is 4:6 if available
  }
 
  # Choose number of periods by cutting stepwise execution of the
  # highest candidate number (i.e. cutting the most ancient periods)
  noperiods <- sort(noperiods)
  while(length(noperiods)>1) {
    maxnoperiod <- max(noperiods)
    glm<-nordpred.estimate(cases,pyr,maxnoperiod,startestage)$glm
    if (Rplatform) {
      pvalue<-1-pchisq(glm$deviance,glm$df.residual)
    } else {
      pvalue<-1-pchisq(glm$deviance,glm$df)
    }
    if (pvalue < 0.01) {
      noperiods <- noperiods[1:(length(noperiods)-1)]
    } else {
      noperiods <- maxnoperiod
    }
  }      
  noperiod <- noperiods
  
  # Set status for recent (whether to use recent trend or average trend)
  if (is.null(recent)) {
    recent <- nordpred.estimate(cases,pyr,noperiod,startestage)$suggestionrecent
  }
  
  # Perform estimation and prediction:
  est  <- nordpred.estimate(cases=cases,pyr=pyr,noperiod=noperiod,startestage=startestage,
                            linkfunc=linkfunc)
  pred <- nordpred.prediction(nordpred.estimate.object=est,startuseage=startuseage,
                              recent=recent,cuttrend=cuttrend)
  return(pred)
}

nordpred.estimate <- function(cases,pyr,noperiod,startestage,linkfunc="power5") {
  # Nordpred:   R (www.r-project.org) & S-PLUS (www.insightful.com) functions 
  #             for prediction of cancer incidence (as used in the Nordpred project).
  # Written by:	Bjørn Møller and Harald Fekjaer <hf@kreftregisteret.no>, 2000-2002
  # See also:	http://www.kreftregisteret.no/software/nordpred/
  # License:	GNU version 2 

  Rplatform <- exists("R.Version")

  if ( dim(cases)[1]!=18 || dim(pyr)[1]!=18 ) {
    stop("\"cases\" and \"pyr\" must have data for 18 age groups")	
  }

  if ( dim(cases)[2]>dim(pyr)[2]) {
    stop("\"pyr\" must include information about all periods in \"cases\"")	
  }
  
  if (dim(pyr)[2]==dim(cases)[2]) {
    stop("\"pyr\" must include information on future rates (E.g. must be greater than \"cases\")")	
  }

  if ((dim(pyr)[2]-dim(cases)[2])>5) {
    stop("Package can not predict more than 5 periods (given by sizes of \"pyr\" and \"cases\")")	
  }

  if ((dim(cases)[2]-noperiod)<0) {
    stop("More periods specified in \"noperiod\" than columns in \"cases\"")   	
  }

  if (noperiod<3) {
    stop("\"noperiod\" must be 3 or larger to get enough for estimating")   	
  }  

  # Setting internal variables:
  dnoperiods <- dim(cases)[2]
  dnoagegr   <- 18

  # Transform dataformat:
  ageno    <- rep(1:dnoagegr,dnoperiods)
  periodno <- sort(rep(1:dnoperiods,dnoagegr))
  cohort   <- max(ageno)-ageno+periodno
  y        <- c(as.matrix(pyr[,1:dnoperiods]))
  apcdata <- data.frame(Cases= c(as.matrix(cases)),Age=ageno,Cohort=cohort,Period=periodno,y=y)

  # Selecting data for regression:
  apcdata <- apcdata[apcdata$Age>=startestage,]
  apcdata <- apcdata[apcdata$Period>(dnoperiods-noperiod),]

  
  # Creation of power link:
  if (Rplatform) {
    # Sett variable for use in estimation
    y <- apcdata$y
    # Make link function:
    power5link <- poisson()
    power5link$link <- "0.2 root link Poisson family"
    power5link$linkfun <- function(mu)  { (mu/y)^0.2 }
    power5link$linkinv <- function(eta) { pmax(.Machine$double.eps, y*eta^5) }
    power5link$mu.eta <- function(eta)  { pmax(.Machine$double.eps, 5*y*eta^4) }
  } else {
    # Sett variable for use in estimation
    y <<- apcdata$y
      # Must be sett on top level
    # Make link function:
    power5link         <- poisson()
    power5link$link    <- function(mu)  { (mu/y)^0.2 }
    power5link$inverse <- function(eta) { y*eta^(1/0.2) }
    power5link$deriv   <- function(mu)  { 0.2*(1/y)*(mu/y)^(0.2 - 1) }
  }

  # Setting contrast:
  options(contrasts=c("contr.treatment","contr.poly"))
  
  # Estimation:
  if (linkfunc=="power5") {
    res.glm <- glm(Cases~as.factor(Age)+Period+as.factor(Period)+as.factor(Cohort) -1,family=power5link,data=apcdata)
  } else  if (linkfunc=="poisson") {
    res.glm <- glm(Cases~as.factor(Age)+Period+as.factor(Period)+as.factor(Cohort)+ offset(log(y)) -1,family=poisson(),data=apcdata)  	
  } else {
    stop("Unknown \"linkfunc\"")	
  }
  
  if (Rplatform) {
    pvalue <- 1-pchisq(res.glm$deviance,res.glm$df.residual)
  } else {
    pvalue <- 1-pchisq(res.glm$deviance,res.glm$df) 	
  }

  # Basis for setting suggestion for 'recent' (whether to use recent trend or average trend)
  mod1 <- glm(Cases~as.factor(Age)+Period +as.factor(Cohort) + offset(log(y)) -1,family=poisson,data=apcdata)
  mod2 <- glm(Cases~as.factor(Age)+Period +I(Period^2)+as.factor(Cohort) + offset(log(y)) -1,family=poisson,data=apcdata)
  if (Rplatform) {
    pdiff<-anova(mod1,mod2,test="Chisq")$"P(>|Chi|)"[2]
	# Correction added 2012-09-19 for compabilty with newer R versions:
	if (is.null(pdiff))
      pdiff <- anova(mod1,mod2,test="Chisq")$"Pr(>Chi)"[2]  
  } else {
    pdiff<-anova(mod1,mod2,test="Chisq")[2,7] 	
  } 
  # use last two periods (5years*2) to predict if period square is significant, else use whole drift
  if (pdiff <0.05) {
    suggestionrecent <-T
  } else {
    suggestionrecent <-F
  }   
  
  # Set class and return results
  res <- list(glm=res.glm,cases=cases,pyr=pyr,noperiod=noperiod,gofpvalue=pvalue,startestage=startestage,
               suggestionrecent=suggestionrecent,pvaluerecent=pdiff,linkfunc=linkfunc)
  class(res) <- "nordpred.estimate"
  attr(res,"Call") <- sys.call()
  return(res)
}

nordpred.getpred <- function(nordpred.object,incidence=T,standpop=NULL,excludeobs=F,byage,agegroups="all") {
  # Nordpred:   R (www.r-project.org) & S-PLUS (www.insightful.com) functions 
  #             for prediction of cancer incidence (as used in the Nordpred project).
  # Written by:	Bjørn Møller and Harald Fekjaer <hf@kreftregisteret.no>, 2000-2002
  # See also:	http://www.kreftregisteret.no/software/nordpred/
  # License:	GNU version 2 
  
  # Seting defaults:
  if (missing(byage)) {
   byage <- ifelse(is.null(standpop),T,F)
  }
  
  # Checking imput:
  if (class(nordpred.object)!="nordpred") {
    stop("Variable \"nordpred.object\" must be of type \"nordpred\"")	
  } 
  
  if ((!is.null(standpop)) && (!incidence)) {
    stop("\"standpop\" should only be used with incidence predictions (incidence=T)") 	
  }
 
  if (!is.null(standpop)) {
    if (round(sum(standpop),5)!=1) {
      stop("\"standpop\" must be of sum 1") 	
    }
    if ((length(standpop)!=length(agegroups)) && (agegroups[1]!="all")) {
      stop("\"standpop\" must be the same length as \"agegroups\"") 	
    }
    if (byage) {
      stop("\"standpop\" is only valid for \"byage=T\"") 
    }
  }
 
  # Seting local data:
  datatable <- nordpred.object$predictions
  pyr       <- data.frame(nordpred.object$pyr)

  # Secting agegroups:
  if (agegroups[1]!="all") {
    datatable <- datatable[agegroups,]
    pyr       <- pyr[agegroups,]
  }

  # If needed; Standardize data and Collapse agegroups
  if (!is.null(standpop)) {
    datainc <- (datatable/pyr)*100000
    if (sum(is.na(datainc))>0) {
      datainc[is.na(datainc)] <- 0
    } 	
    res <- apply(datainc*standpop,2,sum) 
  } else {
    if (!byage) {
      datatable <- apply(datatable,2,sum)
      pyr       <- apply(pyr,2,sum) 
    }
    if (incidence) {
      res <- (datatable/pyr)*100000
      if (sum(is.na(res))>0) {
        res[is.na(res)] <- 0
      } 	
    } else {
      res <- datatable
    } 
  }

  # Select data:
  if (excludeobs) {
    if (is.matrix(res)) {
      predstart <- dim(res)[2]-nordpred.object$nopred+1
      res <- res[,predstart:(predstart+nordpred.object$nopred-1)]
    } else {
      predstart <- length(res)-nordpred.object$nopred+1
      res <- res[predstart:(predstart+nordpred.object$nopred-1)]
    }
  } 
  
  # Return data: 
  return(res)
}

nordpred.prediction <- function(nordpred.estimate.object,startuseage,recent,cuttrend=c(0,.25,.5,.75,.75)) {
  # Nordpred:   R (www.r-project.org) & S-PLUS (www.insightful.com) functions 
  #             for prediction of cancer incidence (as used in the Nordpred project).
  # Written by:	Bjørn Møller and Harald Fekjaer <hf@kreftregisteret.no>, 2000-2002
  # See also:	http://www.kreftregisteret.no/software/nordpred/
  # License:	GNU version 2 
  
  if (class(nordpred.estimate.object)!="nordpred.estimate") {
    stop("Variable \"nordpred.estimate.object\" must be of type \"nordpred.estimate\"")	
  } 

  if (nordpred.estimate.object$startestage>startuseage) {
    stop("\"startuseage\" is set to high compared to \"startestage\" in \"nordpred.estimate.object\"") 	
  }

  if (length(cuttrend)<(dim(nordpred.estimate.object$pyr)[2]-dim(nordpred.estimate.object$cases)[2])) {
    err <- paste("\"cuttrend\" must always be at least the same length as")
    err <- paste(err,"the number of periods with population forecasts")
    stop(err)
  }

  # Setting local variables:
  cases     <-  nordpred.estimate.object$cases
  pyr	    <-  nordpred.estimate.object$pyr
  noperiod  <- nordpred.estimate.object$noperiod
  nototper  <- dim(pyr)[2]
  noobsper  <- dim(cases)[2]
  nonewpred <- nototper-noobsper

  if (length(cuttrend)>nonewpred) {
    cuttrend <- cuttrend[1:nonewpred] 
  }
    
  if (is.data.frame(pyr)) {
    years <- names(pyr)
  } else {
    if (is.null(dimnames(pyr))) {
      years <- paste("Periode",1:nototper)	
    } else {
      years <- dimnames(pyr)[[2]]
    }	
  }  

  # We make data object:
  datatable <- matrix(NA,18,nototper)
  datatable[,1:(nototper-nonewpred)] <- as.matrix(cases)
  datatable <- data.frame(datatable)
  row.names(datatable) <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+")
  names(datatable)     <- years
  # We fill in observed cases:
 
  # Calculate predictions in number of cases:
  for (age in 1:(startuseage-1)) {
    # For agegroups with litle data, we use mean incidence for last two periods:
    obsinc <- cases[age,(noobsper-1):noobsper]/pyr[age,(noobsper-1):noobsper]
    if (sum(is.na(obsinc))) {
      obsinc[is.na(obsinc)] <- 0 	
    }
    datatable[age,(noobsper+1):nototper] <- ((obsinc[,1]+obsinc[,2])/2)*pyr[age,(noobsper+1):nototper]
  }
  for (age in startuseage:18) {
    startestage  <- nordpred.estimate.object$startestage
    coefficients <- nordpred.estimate.object$glm$coefficients

    coh       <- (18-startestage) - (age-startestage) + (noperiod+1:nonewpred)
      #          No. agegoups    -        age        + period
    noages    <- 18-startestage+1
    driftmp   <- cumsum(1-cuttrend)
    cohfind   <- noages + (noperiod-1) + 1 + (coh-1)
    maxcoh    <- 18 - startuseage + noperiod 
    agepar    <- as.numeric(coefficients[age-startestage+1])
    driftfind <- pmatch("Period",attributes(coefficients)$names)
    driftpar  <- as.numeric(coefficients[driftfind])
    
    cohpar <- rep(NA,length(coh))
    for (i in 1:length(coh)) {
      if (coh[i] < maxcoh) {
        cohpar[i] <- as.numeric(coefficients[cohfind[i]])
      } else {
        # For young cohorts not estimated:
        cohpar[i] <- as.numeric(coefficients[length(coefficients)-(startuseage-startestage)])
        cohpar[i][is.na(cohpar[i])] <- 0
      }
    } 
    
    if (recent) {
      lpfind <- driftfind + noperiod-2
        # -2 because first and last period effect is set as contrast (p.first=p.last)
      lppar  <-as.numeric(coefficients[lpfind])
        # lppar is the slope corresponding to a linear trend in the last 10 years (two periods)
      driftrecent <- driftpar - lppar
    }
    
    if (nordpred.estimate.object$linkfunc=="power5") { 
      if (recent) {
        rate <- (agepar+driftpar*noobsper+driftrecent*driftmp+cohpar)^5      
      } else {
        rate <- (agepar+driftpar*(noobsper+driftmp)+cohpar)^5
      }
    } else { # Possion link:
      if (recent) {
        rate <- exp(agepar+driftpar*noobsper+driftrecent*driftmp+cohpar)      
      } else {
        rate <- exp(agepar+driftpar*(noobsper+driftmp)+cohpar)
      }
    }
    datatable[age,(noobsper+1):nototper] <- rate*pyr[age,(noobsper+1):nototper]
  }

  # Structure and return results:
  res <- list(predictions=datatable,pyr=pyr,nopred=nonewpred,noperiod=nordpred.estimate.object$noperiod,
              gofpvalue=nordpred.estimate.object$gofpvalue,recent=recent,pvaluerecent=nordpred.estimate.object$pvaluerecent,
              cuttrend=cuttrend,startuseage=startuseage,startestage=nordpred.estimate.object$startestage,
              glm=nordpred.estimate.object$glm)
  class(res) <- "nordpred"
  attr(res,"Call") <- sys.call()
  return(res)
}

plot.nordpred <- 
  function(nordpred.object,incidence=T,standpop=NULL,agegroups="all",startplot=1,
           xlab="",ylab="",main="",labels=NULL,ylim=NULL,lty=c(1,3),col=c(1,1),new=T,...) {
  # Nordpred:   R (www.r-project.org) & S-PLUS (www.insightful.com) functions 
  #             for prediction of cancer incidence (as used in the Nordpred project).
  # Written by:	Bjørn Møller and Harald Fekjaer <hf@kreftregisteret.no>, 2000-2002
  # See also:	http://www.kreftregisteret.no/software/nordpred/
  # License:	GNU version 2 
  
  if (class(nordpred.object)!="nordpred") {
    stop("Variable \"nordpred.object\" must be of type \"nordpred\"")	
  } 

  # Seting internal variables:
  nopred <- nordpred.object$nopred 
  if (is.null(labels)) {
    labels <- dimnames(nordpred.object$predictions)[[2]]
    labels <- labels[startplot:length(labels)]
  }
  
  # Reding & formating data:
  indata <- nordpred.getpred(nordpred.object,incidence=incidence,standpop=standpop,
                             agegroups=agegroups,byage=F)
  indata <- indata[startplot:length(indata)]

  # Create plots:
  maxx <- length(indata)
  if (new) { 	
    maxy <- max(indata)	
    if (is.null(ylim)) {
      ylim <- c(0,maxy)	
    }
    plot(c(1,maxx),ylim,type="n",ylab=ylab,xlab=xlab,axes=F,...)
    axis(2)
    axis(1,at=1:maxx,labels=labels)
    box()
    title(main)
  }
  lines(1:(maxx-nopred),indata[1:(maxx-nopred)],lty=lty[1],col=col[1],...)
  lines((maxx-nopred):maxx,indata[(maxx-nopred):maxx],lty=lty[2],col=col[2],...)

  # Returning object as invisible
  invisible(nordpred.object)
}

print.nordpred <- function(nordpred.object,digits=1) {
  # Nordpred:   R (www.r-project.org) & S-PLUS (www.insightful.com) functions 
  #             for prediction of cancer incidence (as used in the Nordpred project).
  # Written by:	Bjørn Møller and Harald Fekjaer <hf@kreftregisteret.no>, 2000-2002
  # See also:	http://www.kreftregisteret.no/software/nordpred/
  # License:	GNU version 2 
  
  if (class(nordpred.object)!="nordpred") {
    stop("Variable \"nordpred.object\" must be of type \"nordpred\"")	
  } 

  # Setting internal variables:
  obsto <- names(nordpred.object$predictions)[dim(nordpred.object$predictions)[2]-nordpred.object$nopred]
  # Print information about object: 
  cat("Observed and predicted values:")
  cat("(observations up to",obsto,")\n")
  print(round(as.matrix(nordpred.object$predictions),digits=digits))
  cat("\n  Call: ")
  dput(attr(nordpred.object,"Call"))
  invisible(nordpred.object)

}

print.nordpred.estimate <- function(nordpred.estimate.object) {
  # Nordpred:   R (www.r-project.org) & S-PLUS (www.insightful.com) functions 
  #             for prediction of cancer incidence (as used in the Nordpred project).
  # Written by:	Bjørn Møller and Harald Fekjaer <hf@kreftregisteret.no>, 2000-2002
  # See also:	http://www.kreftregisteret.no/software/nordpred/
  # License:	GNU version 2 

  if (class(nordpred.estimate.object)!="nordpred.estimate") {
    stop("Variable \"nordpred.estimate.object\" must be of type \"nordpred.estimate\"")	
  } 

  # Print information about object: 
  cat("Fit of cancer prediction model (as of Nordpred project)\n")
  cat("Fitted for",nordpred.estimate.object$noperiod,"periods")
  cat(", with estimation from age group number",nordpred.estimate.object$startestage,"\n")
  cat("  Call: ")
  dput(attr(nordpred.estimate.object,"Call"))
 
  invisible(nordpred.estimate.object)
}

summary.nordpred <- function(nordpred.object,printpred=T,printcall=F,digits=1) {
  # Nordpred:   R (www.r-project.org) & S-PLUS (www.insightful.com) functions 
  #             for prediction of cancer incidence (as used in the Nordpred project).
  # Written by:	Bjørn Møller and Harald Fekjaer <hf@kreftregisteret.no>, 2000-2002
  # See also:	http://www.kreftregisteret.no/software/nordpred/
  # License:	GNU version 2 
  
  if (class(nordpred.object)!="nordpred") {
    stop("Variable \"nordpred.object\" must be of type \"nordpred\"")	
  } 
  
  # Setting internal variables:
  obsto <- names(nordpred.object$predictions)[dim(nordpred.object$predictions)[2]-nordpred.object$nopred]

  if (!is.null(nordpred.object$pvaluerecent)) {
    precent <- round(nordpred.object$pvaluerecent,4)
  } else { precent <- NA }
   
  if (!is.null(nordpred.object$gofpvalue)) {
    gofpvalue <- round(nordpred.object$gofpvalue,4)
  } else { gofpvalue <- NA }
      
  # Print information about object: 
  if (printpred) {
    cat("Observed and predicted values:")
    cat("(observations up to",obsto,")\n")
    print(round(as.matrix(nordpred.object$predictions),digits=digits))
    cat("\n")
  }
  cat("\nPrediction done with:\n")
    
  moptions <- matrix(NA,8,2)
  moptions[,1] <- c("Number of periods predicted (nopred):","Trend used in predictions (cuttrend):",
                    "Number of periods used in estimate (noperiod):",
                    "P-value for goodness of fit:",
                    "Used recent (recent):","P-value for recent:",
                    "First age group used (startuseage):","First age group estimated (startestage):")        
  moptions[,2] <- c(nordpred.object$nopred,paste(nordpred.object$cuttrend,collapse=" , "),
                    nordpred.object$noperiod,gofpvalue,
                    nordpred.object$recent,precent,
                    nordpred.object$startuseage,nordpred.object$startestage)
  maxl <- max(nchar(moptions[,1]))
    
  for (i in 1:dim(moptions)[1]) {
    spaces <- rep (" ",maxl-nchar(moptions[i,1])+2)
    cat(moptions[i,1],spaces,moptions[i,2],"\n",sep="")	
  }
    
  if (printcall) {
    cat("\n  Call: ")
    dput(attr(nordpred.object,"Call"))
  }
  invisible(nordpred.object)
}

