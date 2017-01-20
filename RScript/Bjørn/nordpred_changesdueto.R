nordpred.changesdueto <- function(

	nordpred.prediction.object,
	predper=NULL,
	periodslabels=NULL 
){

#periodslabels= c("2010","2015","2020")

if(is.null(predper)){predper <- c(1:nordpred.prediction.object$nopred)}
if(is.null(periodslabels)){periodslabels <- predper}


tmp <- nordpred.prediction.object

noobsper <- nordpred.prediction.object$noobsper

changesdueto <- as.data.frame(matrix(NA,1,5*length(predper)+1))

counter <- 0

str <- dQuote("lastobsnum")

for (fp in predper) {

Nfff <- sum(tmp$predictions[noobsper+fp])/5
Nooo <- sum(tmp$predictions[noobsper])/5
Noff <- sum((tmp$predictions[noobsper]/tmp$pyr[noobsper])*tmp$pyr[noobsper+fp])/5 

changesdueto[1,1] <- Nooo 			#lastobsnum

changesdueto[1,2+5*counter] <- Nfff 		# prednum
changesdueto[1,3+5*counter] <- (Nfff-Nooo) 	# changenum
changesdueto[1,4+5*counter] <- (Nfff-Nooo)*100/Nooo # %change
changesdueto[1,5+5*counter] <- (Nfff-Noff)*100/Nooo # %risk
changesdueto[1,6+5*counter] <- (Noff-Nooo)*100/Nooo # %pop

counter <- counter + 1 

n1 <-  dQuote(paste("prednum",periodslabels[counter],sep=""))
n2 <-  dQuote(paste("changenum",periodslabels[counter],sep=""))
n3 <-  dQuote(paste("%change",periodslabels[counter],sep=""))
n4 <-  dQuote(paste("%risk",periodslabels[counter],sep=""))
n5 <-  dQuote(paste("%pop",periodslabels[counter],sep=""))

str <- (paste(str,n1,n2,n3,n4,n5,sep=","))

}


names(changesdueto) <- eval(parse(text=paste("c(",str,")",sep="")))


return(changesdueto)

}
