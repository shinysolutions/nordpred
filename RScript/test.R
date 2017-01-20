# Example of use of "nordpred"-functions.

# Nordpred:     R (www.r-project.org) & S-PLUS (www.insightful.com) functions 
#               for prediction of cancer incidence (as used in the Nordpred project).
# Written by:	Bj?rn M?ller and Harald Fekjaer <harald.fekjar@kreftregisteret.no>, 2000-2002
# See also:	http://www.kreftregisteret.no/software/nordpred

# Reading package:

# Reading data (Colon cancer for Norwegian males)
indata <- read.table("colon-men-Norway.txt",header =T,sep=",",row.names=1)
inpop1 <- read.table("men-Norway.txt",header =T,sep=",",row.names=1)
inpop2 <- read.table("men-Norway-pred.txt",header =T,sep=",",row.names=1)
inpop <- cbind(inpop1,inpop2)
wstand <- c(0.12, 0.1, 0.09, 0.09, 0.08, 0.08, 0.06, 0.06, 0.06, 
            0.06,0.05,  
            0.04, 0.04, 0.03, 0.02, 0.01, 0.005, 0.005) 
write.table(inpop, file = "pyr.csv", sep = ",", quote = FALSE)


# Run predictions:
# source("RScript/Bjørn/nordpred.s")
# mod_B <- nordpred.estimate(cases=indata,pyr=inpop,noperiod=4,startestage=4, linkfunc = "poisson")
# mod_B$glm
# nordpred.prediction(mod_B,startuseage=4,recent=FALSE) 
# nordpred.prediction(mod_B,startuseage=4,recent=TRUE) 
source("RScript/Bjørn/nordpred.s")
mod_B <- nordpred(cases=indata,pyr=inpop,noperiod=4,startestage=4, startuseage = 4, linkfunc = "poisson")
round(nordpred.getpred(res,incidence=F),2)
 
mod_B <- nordpred(cases=indata,pyr=inpop,noperiod=4,startestage=4, startuseage = 4, linkfunc = "poisson", recent = TRUE)
round(nordpred.getpred(mod_B,incidence=F),2)

###########################################
source("RScript/Huidong/nordpred.R")
source("RScript/Huidong/predict.nordpred.R")
mod_H <- nordpred(cas = indata, pyr = inpop, linkfunc = "power5")
mod_H$periods


predict.nordpred(mod_H, recent = FALSE, constant = TRUE)


predict.nordpred(mod_H, recent = FALSE)
predict.nordpred(mod_H, recent = TRUE, iRate = FALSE)[1:18,]
predict.nordpred(mod_H, recent = TRUE, iRate = TRUE)[1:18,]


