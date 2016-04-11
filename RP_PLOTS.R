library(RSQLite); library(reldist); library(svglite); library(Hmisc)
LSAY <- dbConnect(SQLite(), dbname="~/Dropbox/Databases/LSAY_DATSETS/LSAY.sqlite")
#### Plots ####
OECD <- c('DEU','POL','SWE','ISL')

plotDistCompare <- function(yOrig = PISA2000M, yComp = PISA2012, target = "math", ...)
{
	par(mfrow=c(2,2))
	#Panel a
	plot(density(yComp[,target]), type = "l",
		 lty = 2, main = "(a)", bty="n", xlab = "PISA Scores")
	lines(density(yOrig[,target], na.rm=TRUE))
	legend("topright", legend = c("PISA2000", "PISA2012"), lty = c(1,2),bty = "n")
	#Panel b
	panel.b <- reldist(y=yComp[,target],yo=yOrig[,target],
			yowgt=yOrig[,"wt"], ywgt=yComp[,"wt"],
			smooth = 15, ylab="Relative Density",                 
			xlab="Proportion", bty = "n",main = "(b)",
			bar = TRUE, ci = TRUE
	)
	#Panel c
	panel.c <- reldist(y=yComp[,target],yo=yOrig[,target],
			yowgt=yOrig[,"wt"], ywgt=yComp[,"wt"],
			smooth = 15,ylab="Relative Density",                 
			xlab="Proportion", bty = "n",main = "(c)",
			show="effect",bar = TRUE, ci = TRUE
	)
	#Panel d
	panel.d <- reldist(y=yComp[,target],yo=yOrig[,target],
			yowgt=yOrig[,"wt"], ywgt=yComp[,"wt"],
			smooth = 15,ylab="Relative Density",                 
			xlab="Proportion", bty = "n",main = "(d)",
			show="residual", bar = TRUE, ci = TRUE
	)
	print(rbind(
	rpy(y=yComp[,target],yo=yOrig[,target],
			   ywgt=yComp[,"wt"],yowgt=yOrig[,"wt"],pvalue=TRUE),
	rpluy(y=yComp[,target],yo=yOrig[,target],
				 ywgt=yComp[,"wt"],yowgt=yOrig[,"wt"],pvalue=TRUE),
	rpluy(y=yComp[,target],yo=yOrig[,target],
				 ywgt=yComp[,"wt"],yowgt=yOrig[,"wt"],pvalue=TRUE,
				 upper=TRUE)
	)
	)
	cat("\n")
}

for ( i in seq_along(OECD))
{
	cat("Currently working on country: ", OECD[i],"\n")
	PISA2000M <- dbGetQuery(LSAY, paste0("SELECT PV1MATH as math,W_FSTUWT as wt FROM PISA2000_MATH where CNT = '", OECD[i], "'") )
	PISA2000R <- dbGetQuery(LSAY, paste0("SELECT PV1READ as read,W_FSTUWT as wt FROM PISA2000_READ where CNT = '", OECD[i], "'") )
	PISA2000S <- dbGetQuery(LSAY, paste0("SELECT PV1SCIE as scie,W_FSTUWT as wt FROM PISA2000_SCIENCE where CNT = '", OECD[i], "'") )

	PISA2003 <- dbGetQuery(LSAY, paste0("SELECT PV1MATH as math,PV1READ as read, PV1SCIE as scie, W_FSTUWT as wt FROM PISA2003 where CNT = '",OECD[i],"'") )
	PISA2006 <- dbGetQuery(LSAY, paste0("SELECT PV1MATH as math,PV1READ as read, PV1SCIE as scie, W_FSTUWT as wt FROM PISA2006 where CNT = '",OECD[i],"'") )
	PISA2009 <- dbGetQuery(LSAY, paste0("SELECT PV1MATH as math,PV1READ as read, PV1SCIE as scie, W_FSTUWT as wt FROM PISA2009 where CNT = '",OECD[i],"'") )
	PISA2012 <- dbGetQuery(LSAY, paste0("SELECT PV1MATH as math,PV1READ as read, PV1SCIE as scie, W_FSTUWT as wt FROM PISA2012 where CNT = '",OECD[i],"'") )
	#MATH
	domains <- c("math", "read", "scie")
	domainData <- list(PISA2000M,PISA2000R,PISA2000S)
	for (j in  seq_along(domains))
	{
		svglite(file = paste0("./submission/plots/",OECD[i],"_",domains[j],"_2012.svg") )
		plotDistCompare(yComp = PISA2012,yOrig = domainData[[j]], target = domains[j])
		dev.off()
	}
}

# tmp <- reldist(y=PISA2012$math,yo=PISA2000M$math,
# 		yowgt=PISA2000M$wt, ywgt=PISA2012$wt,
# 		ylab="Relative Density",ci=TRUE, bar=TRUE,              
# 		xlab="Proportion", bty = "n",
# 		show="residual",main = "(d)"
# )


