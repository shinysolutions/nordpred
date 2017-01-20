predict.nordpred <- function(object, recent = NULL, constant = FALSE) {
  
  ## Requirment;
  if (class(object) != "nordpred") {
    stop("Variable \"object\" must be of classs \"nordpred\"")	
  } 

  cas <- object$cas
  pyr <- object$pyr
  dcas <- dim(object$cas)
  dpyr <- dim(object$pyr)
  
  startAge <- object$startAge
  periods  <- object$periods
  
  mAge <- matrix(rep(1:18, dpyr[2]), nrow = 18)
  mPeriod <- matrix(rep(1:dpyr[2], each = 18), nrow = 18)
  mCohort <- 18 - mAge + mPeriod
  
  ## Prediction
  Fit <- pyr; 
  Fit[,] <- NA;
  Fit[,1:ncol(cas)] <- cas;
  row.names(Fit) <- row.names(cas)

  if (constant) {
      obsinc <- cas[, -1:0 + dcas[2]]/pyr[, -1:0 + dcas[2]]
      if (sum(is.na(obsinc))) {obsinc[is.na(obsinc)] <- 0 }
      Fit[,(dcas[2]+1):dpyr[2]] <- rowMeans(obsinc) * pyr[,(dcas[2]+1):dpyr[2]]
  } else {
    # Calculate predictions in number of cas:
    for (age in 1:(startAge-1)) {
      # For agegroups with litle data, we use mean incidence for last two periods:
      obsinc <- cas[age,(dcas[2]-1):dcas[2]]/pyr[age,(dcas[2]-1):dcas[2]]
      if (sum(is.na(obsinc))) {obsinc[is.na(obsinc)] <- 0 }
      Fit[age,(dcas[2]+1):dpyr[2]] <- ((obsinc[,1]+obsinc[,2])/2)*pyr[age,(dcas[2]+1):dpyr[2]]
    }
    pCuttrend <- cumsum(1 - c(0,.25,.5,.75,.75))
    Coeff <- object$mod$coefficients
    rPeriod <- dcas[2] + pCuttrend
    for (age in startAge:18) {
      pAge <- as.numeric(Coeff[age - startAge + 1])
      pPeriod <- as.numeric(Coeff[which(names(Coeff) == "Period")])
      rCohort <- mCohort[age, (dcas[2] + 1):dpyr[2]]
      pCohort <- rep(0, length(rCohort))
      for (i in 1:length(rCohort)) {
        coh <- rCohort[i]
        id <- grep(paste("Cohort", coh, sep = ")"), names(Coeff))
        if (length(id) > 0) {
          pCohort[i] <- Coeff[id]
        }
      }
      pCohort[which(is.na(pCohort))] <- 0
      ## rate without link function;
      rate <- pAge + pPeriod * rPeriod + pCohort 
      if (is.null(recent))  {
        recent <- object$recent
      }
      if (recent) {
        lppar <- as.numeric(Coeff[max(grep("Period", names(Coeff)))-1])  
        rate <- rate - lppar*pCuttrend
      } 
      if (object$mod$family$link == "log") {
        rate <- exp(rate)
      } else {
        rate <- (rate)^5
      }
      Fit[age,(dcas[2]+1):dpyr[2]] <- rate * pyr[age,(dcas[2]+1):dpyr[2]]
    }
  }
 
  
  #################################################
  ## Crude rate;
  id <- nrow(Fit) + 1
  Fit[id, ] <- colSums(Fit[1:18, ])/colSums(pyr)*100000
  row.names(Fit)[id] <- "Crude rate"
  ## Standardize rate;
  wstand <- c(0.12, 0.1, 0.09, 0.09, 0.08, 0.08, 0.06, 0.06, 0.06, 
              0.06,0.05, 0.04, 0.04, 0.03, 0.02, 0.01, 0.005, 0.005)
  id <- nrow(Fit) + 1
  Fit[id, ] <- colSums(Fit[1:18, ]/pyr*100000*wstand)
  row.names(Fit)[id] <- "Standardized rate"
  
  
  ## Total cas number for each period
  id <- nrow(Fit) + 1
  Fit[id, ] <- colSums(Fit)
  row.names(Fit)[id] <- "Total"
  
  ## Annual average case number;
  id <- nrow(Fit) + 1
  id.total <- which(row.names(Fit) == "Total")
  Fit[id, ] <- Fit[id.total, ]/5
  row.names(Fit)[id] <- "Annual average"
  
  ## Overall change;
  id <- nrow(Fit) + 1
  id.avg <- which(row.names(Fit) == "Annual average")
  Fit[id, ] <- rep(NA, ncol(Fit))
  Fit[id, (dcas[2]+1):dpyr[2]] <- Fit[id.avg, (dcas[2]+1):dpyr[2]] - Fit[id.avg, dcas[2]]
  row.names(Fit)[id] <- "Overall change"
  

  ## Change due to:
  N.pre <- Fit[1:18, (dcas[2]+1):dpyr[2]]
  N.obs <- Fit[1:18, dcas[2]]
  N.off <- pyr[1:18, (dcas[2]+1):dpyr[2]] * (Fit[1:18, dcas[2]]/pyr[1:18, dcas[2]])
  if (constant) {
    obsinc <- Fit[1:18, -1:0 + dcas[2]]/pyr[1:18, -1:0 + dcas[2]]
    if (sum(is.na(obsinc))) {obsinc[is.na(obsinc)] <- 0 }
    N.off <- rowMeans(obsinc) * pyr[1:18,(dcas[2]+1):dpyr[2]]
  }
    

  
  ## overall change percentage;
  id <- nrow(Fit) + 1
  Fit[id, ] <- rep(NA, ncol(Fit))
  Fit[id, (dcas[2]+1):dpyr[2]] <- colSums((N.pre - N.obs)/N.obs, na.rm = TRUE)
  row.names(Fit)[id] <- "Overall change (%)"
  ## change due risk
  id <- nrow(Fit) + 1
  Fit[id, ] <- rep(NA, ncol(Fit))
  Fit[id, (dcas[2]+1):dpyr[2]] <- colSums((N.pre - N.off)/N.obs, na.rm = TRUE)
  row.names(Fit)[id] <- "Change due to change in risk"
  ## change due to population
  id <- nrow(Fit) + 1
  Fit[id, ] <- rep(NA, ncol(Fit))
  Fit[id, (dcas[2]+1):dpyr[2]] <- colSums((N.off - N.obs)/N.obs, na.rm = TRUE)
  row.names(Fit)[id] <- "Change due to change in population"
  
  Fit[1:18, ] <- Fit[1:18, ]/pyr*100000
  # if (iRate) { Fit[1:18, ] <- Fit[1:18, ]/pyr*100000}
  return(round(Fit, 1))
}

