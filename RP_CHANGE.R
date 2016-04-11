library(RSQLite); library(reldist); library(svglite); library(Hmisc); library(lmerTest)
LSAY <- dbConnect(SQLite(), dbname="~/Dropbox/Databases/LSAY_DATSETS/LSAY.sqlite")

OECD <- c('AUS','AUT','BEL','CAN','CZE','DNK','FIN',
		 'FRA','DEU','GRC','HUN','ISL','IRL','ITA',
		 'JPN','KOR','LUX','NLD','NZL','NOR','POL',
		 'PRT','ESP','SWE','CHE','GBR','USA')

plotDistCompare <- function(yOrig = PISA2000M, yComp = PISA2012, target = "math", ...)
{
tmp <- rbind.data.frame(
	rpy(y=yComp[,target],yo=yOrig[,target],
		ywgt=yComp[,"wt"],yowgt=yOrig[,"wt"],pvalue=TRUE),
	rpluy(y=yComp[,target],yo=yOrig[,target],
		  ywgt=yComp[,"wt"],yowgt=yOrig[,"wt"],pvalue=TRUE),
	rpluy(y=yComp[,target],yo=yOrig[,target],
		  ywgt=yComp[,"wt"],yowgt=yOrig[,"wt"],pvalue=TRUE,
		  upper=TRUE)
)
tmp
}

####math ####
out <- matrix(NA, nrow=length(OECD)*5, ncol = 6)

for ( i in seq_along(OECD))
{
	cat("Currently working on country: ", OECD[i],"\n")
	PISA2000 <- dbGetQuery(LSAY, paste0("SELECT PV1MATH as math,W_FSTUWT as wt FROM PISA2000_MATH where CNT = '", OECD[i], "'") )
	PISA2003 <- dbGetQuery(LSAY, paste0("SELECT PV1MATH as math, W_FSTUWT as wt FROM PISA2003 where CNT = '",OECD[i],"'") )
	PISA2006 <- dbGetQuery(LSAY, paste0("SELECT PV1MATH as math, W_FSTUWT as wt FROM PISA2006 where CNT = '",OECD[i],"'") )
	PISA2009 <- dbGetQuery(LSAY, paste0("SELECT PV1MATH as math, W_FSTUWT as wt FROM PISA2009 where CNT = '",OECD[i],"'") )
	PISA2012 <- dbGetQuery(LSAY, paste0("SELECT PV1MATH as math, W_FSTUWT as wt FROM PISA2012 where CNT = '",OECD[i],"'") )
	#2000 vs 2003
	tmp00.03.01 <- plotDistCompare(yComp = PISA2003,yOrig = PISA2000, target = 'math')
	tmp00.03.02 <- weighted.mean(PISA2003$math, PISA2003$wt) - weighted.mean(PISA2000$math, PISA2000$wt)
	#2003 vs 2006
	tmp03.06.01 <- plotDistCompare(yComp = PISA2006,yOrig = PISA2003, target = 'math')
	tmp03.06.02 <- weighted.mean(PISA2006$math, PISA2006$wt) - weighted.mean(PISA2003$math, PISA2003$wt)
	#2006 vs 2009
	tmp06.09.01 <- plotDistCompare(yComp = PISA2009,yOrig = PISA2006, target = 'math')
	tmp06.09.02 <- weighted.mean(PISA2009$math, PISA2009$wt) - weighted.mean(PISA2006$math, PISA2006$wt)
	#2009 vs 2012
	tmp09.12.01 <- plotDistCompare(yComp = PISA2012,yOrig = PISA2009, target = 'math')
	tmp09.12.02 <- weighted.mean(PISA2012$math, PISA2012$wt) - weighted.mean(PISA2009$math, PISA2009$wt)
	#2000 vs 2012
	tmp00.12.01 <- plotDistCompare(yComp = PISA2012,yOrig = PISA2000, target = 'math')
	tmp00.12.02 <- weighted.mean(PISA2012$math, PISA2012$wt) - weighted.mean(PISA2000$math, PISA2000$wt)
	
	out[i,] <- c(OECD[i], 1,tmp00.03.02, tmp00.03.01[,2])
	out[i+length(OECD),] <- c(OECD[i], 2,tmp03.06.02, tmp03.06.01[,2])
	out[i+length(OECD)*2,] <- c(OECD[i], 3,tmp06.09.02, tmp06.09.01[,2])
	out[i+length(OECD)*3,] <- c(OECD[i], 4,tmp09.12.02, tmp09.12.01[,2])
	out[i+length(OECD)*4,] <- c(OECD[i], 5,tmp00.12.02, tmp00.12.01[,2])
	
	
}


math <- as.data.frame(out)
names(math) <-c('country', 'lag', 'mean', 'median', 'lower', 'upper')
math[,3:6] <- apply(math[,3:6],2,as.numeric)
####read ####
out <- matrix(NA, nrow=length(OECD)*5, ncol = 6)

for ( i in seq_along(OECD))
{
	cat("Currently working on country: ", OECD[i],"\n")
	PISA2000 <- dbGetQuery(LSAY, paste0("SELECT PV1READ as math,W_FSTUWT as wt FROM PISA2000_READ where CNT = '", OECD[i], "'") )
	PISA2003 <- dbGetQuery(LSAY, paste0("SELECT PV1READ as math, W_FSTUWT as wt FROM PISA2003 where CNT = '",OECD[i],"'") )
	PISA2006 <- dbGetQuery(LSAY, paste0("SELECT PV1READ as math, W_FSTUWT as wt FROM PISA2006 where CNT = '",OECD[i],"'") )
	PISA2009 <- dbGetQuery(LSAY, paste0("SELECT PV1READ as math, W_FSTUWT as wt FROM PISA2009 where CNT = '",OECD[i],"'") )
	PISA2012 <- dbGetQuery(LSAY, paste0("SELECT PV1READ as math, W_FSTUWT as wt FROM PISA2012 where CNT = '",OECD[i],"'") )
	#2000 vs 2003
	tmp00.03.01 <- plotDistCompare(yComp = PISA2003,yOrig = PISA2000, target = 'math')
	tmp00.03.02 <- weighted.mean(PISA2003$math, PISA2003$wt) - weighted.mean(PISA2000$math, PISA2000$wt)
	#2003 vs 2006
	tmp03.06.01 <- plotDistCompare(yComp = PISA2006,yOrig = PISA2003, target = 'math')
	tmp03.06.02 <- weighted.mean(PISA2006$math, PISA2006$wt) - weighted.mean(PISA2003$math, PISA2003$wt)
	#2006 vs 2009
	tmp06.09.01 <- plotDistCompare(yComp = PISA2009,yOrig = PISA2006, target = 'math')
	tmp06.09.02 <- weighted.mean(PISA2009$math, PISA2009$wt) - weighted.mean(PISA2006$math, PISA2006$wt)
	#2009 vs 2012
	tmp09.12.01 <- plotDistCompare(yComp = PISA2012,yOrig = PISA2009, target = 'math')
	tmp09.12.02 <- weighted.mean(PISA2012$math, PISA2012$wt) - weighted.mean(PISA2009$math, PISA2009$wt)
	#2000 vs 2012
	tmp00.12.01 <- plotDistCompare(yComp = PISA2012,yOrig = PISA2000, target = 'math')
	tmp00.12.02 <- weighted.mean(PISA2012$math, PISA2012$wt) - weighted.mean(PISA2000$math, PISA2000$wt)
	
	out[i,] <- c(OECD[i], 1,tmp00.03.02, tmp00.03.01[,2])
	out[i+length(OECD),] <- c(OECD[i], 2,tmp03.06.02, tmp03.06.01[,2])
	out[i+length(OECD)*2,] <- c(OECD[i], 3,tmp06.09.02, tmp06.09.01[,2])
	out[i+length(OECD)*3,] <- c(OECD[i], 4,tmp09.12.02, tmp09.12.01[,2])
	out[i+length(OECD)*4,] <- c(OECD[i], 5,tmp00.12.02, tmp00.12.01[,2])
	
	
}

read <- as.data.frame(out)
names(read) <-c('country', 'lag', 'mean', 'median', 'lower', 'upper')
read[,3:6] <- apply(read[,3:6],2,as.numeric)
####science ####
out <- matrix(NA, nrow=length(OECD)*5, ncol = 6)

for ( i in seq_along(OECD))
{
	cat("Currently working on country: ", OECD[i],"\n")
	PISA2000 <- dbGetQuery(LSAY, paste0("SELECT PV1SCIE as math,W_FSTUWT as wt FROM PISA2000_SCIENCE where CNT = '", OECD[i], "'") )
	PISA2003 <- dbGetQuery(LSAY, paste0("SELECT PV1SCIE as math, W_FSTUWT as wt FROM PISA2003 where CNT = '",OECD[i],"'") )
	PISA2006 <- dbGetQuery(LSAY, paste0("SELECT PV1SCIE as math, W_FSTUWT as wt FROM PISA2006 where CNT = '",OECD[i],"'") )
	PISA2009 <- dbGetQuery(LSAY, paste0("SELECT PV1SCIE as math, W_FSTUWT as wt FROM PISA2009 where CNT = '",OECD[i],"'") )
	PISA2012 <- dbGetQuery(LSAY, paste0("SELECT PV1SCIE as math, W_FSTUWT as wt FROM PISA2012 where CNT = '",OECD[i],"'") )
	#2000 vs 2003
	tmp00.03.01 <- plotDistCompare(yComp = PISA2003,yOrig = PISA2000, target = 'math')
	tmp00.03.02 <- weighted.mean(PISA2003$math, PISA2003$wt) - weighted.mean(PISA2000$math, PISA2000$wt)
	#2003 vs 2006
	tmp03.06.01 <- plotDistCompare(yComp = PISA2006,yOrig = PISA2003, target = 'math')
	tmp03.06.02 <- weighted.mean(PISA2006$math, PISA2006$wt) - weighted.mean(PISA2003$math, PISA2003$wt)
	#2006 vs 2009
	tmp06.09.01 <- plotDistCompare(yComp = PISA2009,yOrig = PISA2006, target = 'math')
	tmp06.09.02 <- weighted.mean(PISA2009$math, PISA2009$wt) - weighted.mean(PISA2006$math, PISA2006$wt)
	#2009 vs 2012
	tmp09.12.01 <- plotDistCompare(yComp = PISA2012,yOrig = PISA2009, target = 'math')
	tmp09.12.02 <- weighted.mean(PISA2012$math, PISA2012$wt) - weighted.mean(PISA2009$math, PISA2009$wt)
	#2000 vs 2012
	tmp00.12.01 <- plotDistCompare(yComp = PISA2012,yOrig = PISA2000, target = 'math')
	tmp00.12.02 <- weighted.mean(PISA2012$math, PISA2012$wt) - weighted.mean(PISA2000$math, PISA2000$wt)
	
	out[i,] <- c(OECD[i], 1,tmp00.03.02, tmp00.03.01[,2])
	out[i+length(OECD),] <- c(OECD[i], 2,tmp03.06.02, tmp03.06.01[,2])
	out[i+length(OECD)*2,] <- c(OECD[i], 3,tmp06.09.02, tmp06.09.01[,2])
	out[i+length(OECD)*3,] <- c(OECD[i], 4,tmp09.12.02, tmp09.12.01[,2])
	out[i+length(OECD)*4,] <- c(OECD[i], 5,tmp00.12.02, tmp00.12.01[,2])
}

scie <- as.data.frame(out)
names(scie) <-c('country', 'lag', 'mean', 'median', 'lower', 'upper')
scie[,3:6] <- apply(scie[,3:6],2,as.numeric)

math[,4:6] <- math[,4:6] * 100
read[,4:6] <- read[,4:6] * 100
scie[,4:6] <- scie[,4:6] * 100

####Models####
#math
summary(lmer(mean ~ median + lag + (1|country), math, subset = lag != 5))
summary(lmer(mean ~ lower + lag + (1|country), math, subset = lag != 5))
summary(lmer(mean ~ upper + lag + (1|country), math, subset = lag != 5))

M1 <- lm(mean ~ median*as.factor(country) + lag, math, subset = lag != 5)
tmp <- c(coef(M1)[2], coef(M1)[2]+ coef(M1)[32:57])
mean(tmp);median(tmp)
M1 <- lm(mean ~ lower*as.factor(country) + lag, math, subset = lag != 5)
tmp <- c(coef(M1)[2], coef(M1)[2]+ coef(M1)[32:57])
mean(tmp);median(tmp)
M1 <- lm(mean ~ upper*as.factor(country) + lag, math, subset = lag != 5)
tmp <- c(coef(M1)[2], coef(M1)[2]+ coef(M1)[32:57])
mean(tmp);median(tmp)
#reading
summary(lmer(mean ~ median + lag + (1|country), read, subset = lag != 5))
summary(lmer(mean ~ lower + lag + (1|country), read, subset = lag != 5))
summary(lmer(mean ~ upper + lag + (1|country), read, subset = lag != 5))

M1 <- lm(mean ~ median*as.factor(country) + lag, read, subset = lag != 5)
tmp <- c(coef(M1)[2], coef(M1)[2]+ coef(M1)[31:55])
mean(tmp);median(tmp)
M1 <- lm(mean ~ lower*as.factor(country) + lag, read, subset = lag != 5)
tmp <- c(coef(M1)[2], coef(M1)[2]+ coef(M1)[31:55])
mean(tmp);median(tmp)
M1 <- lm(mean ~ upper*as.factor(country) + lag, read, subset = lag != 5)
tmp <- c(coef(M1)[2], coef(M1)[2]+ coef(M1)[31:55])
mean(tmp);median(tmp)
#science
summary(lmer(mean ~ median + lag + (1|country), scie, subset = lag != 5))
summary(lmer(mean ~ lower + lag + (1|country), scie, subset = lag != 5))
summary(lmer(mean ~ upper + lag + (1|country), scie, subset = lag != 5))

M1 <- lm(mean ~ median*as.factor(country) + lag, scie, subset = lag != 5)
tmp <- c(coef(M1)[2], coef(M1)[2]+ coef(M1)[32:57])
mean(tmp);median(tmp)
M1 <- lm(mean ~ lower*as.factor(country) + lag, scie, subset = lag != 5)
tmp <- c(coef(M1)[2], coef(M1)[2]+ coef(M1)[32:57])
mean(tmp);median(tmp)
M1 <- lm(mean ~ upper*as.factor(country) + lag, scie, subset = lag != 5)
tmp <- c(coef(M1)[2], coef(M1)[2]+ coef(M1)[32:57])
mean(tmp);median(tmp)

math.z <- math
math.z[,3:6] <- apply(math.z[,3:6],2,scale)
read.z <- read
read.z[,3:6] <- apply(read.z[,3:6],2,scale)
scie.z <- scie
scie.z[,3:6] <- apply(scie.z[,3:6],2,scale)
#math
summary(lmer(mean ~ median + lag + (1|country), math.z, subset = lag != 5))
summary(lmer(mean ~ lower + lag + (1|country), math.z, subset = lag != 5))
summary(lmer(mean ~ upper + lag + (1|country), math.z, subset = lag != 5))
#reading
summary(lmer(mean ~ median + lag + (1|country), read.z, subset = lag != 5))
summary(lmer(mean ~ lower + lag + (1|country), read.z, subset = lag != 5))
summary(lmer(mean ~ upper + lag + (1|country), read.z, subset = lag != 5))
#science
summary(lmer(mean ~ median + lag + (1|country), scie.z, subset = lag != 5))
summary(lmer(mean ~ lower + lag + (1|country), scie.z, subset = lag != 5))
summary(lmer(mean ~ upper + lag + (1|country), scie.z, subset = lag != 5))

math.rp <- math[math$lag == 5,]
read.rp <- read[read$lag == 5,]
scie.rp <- scie[scie$lag == 5,]

math.rp <- math.rp[order(math.rp$median),]
read.rp <- read.rp[order(read.rp$median),]
scie.rp <- scie.rp[order(scie.rp$median),]

round(apply(math.rp[,4:6],2,min,na.rm = TRUE),4)*100
round(apply(math.rp[,4:6],2,max,na.rm = TRUE),4)*100
round(apply(math.rp[,4:6],2,mad,na.rm = TRUE),4)*100
round(apply(math.rp[,4:6],2,median,na.rm = TRUE),4)*100

round(apply(read.rp[,4:6],2,min,na.rm = TRUE),4)*100
round(apply(read.rp[,4:6],2,max,na.rm = TRUE),4)*100
round(apply(read.rp[,4:6],2,mad,na.rm = TRUE),4)*100
round(apply(read.rp[,4:6],2,median,na.rm = TRUE),4)*100

round(apply(scie.rp[,4:6],2,min,na.rm = TRUE),4)*100
round(apply(scie.rp[,4:6],2,max,na.rm = TRUE),4)*100
round(apply(scie.rp[,4:6],2,mad,na.rm = TRUE),4)*100
round(apply(scie.rp[,4:6],2,median,na.rm = TRUE),4)*100

