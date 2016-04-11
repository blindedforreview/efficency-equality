####### DESCRIPTION #######
# {_id: "002_Models_PartialTrajectory_2015-12-22_v.2.0.1" {
# 		{title: "Achievement Inequality versus Performance: Where is the Trade-Off?"},
# 		{author: "Blinded"},
# 		{date: "2016-04-04 16:32:37 AEST"},
# 		{version: "v2.0.1"},
# 		{description: "Simple difference between PISA2000 to PISA 2012"}
# 	}
# }
#---------------------

library(car)			# recode variables

##### load files & Data Prep #####
file.load <- list.files(path = "./replication/data",pattern=".*RData$", full.names = TRUE)
#load the files into an environment for quick clean up.
data.env <- new.env()
for (i in seq_along(file.load))
{
	load(file.load[i], verbose = TRUE, envir = data.env)
	rm(i)
}
# Rounds of the PISA data collection
years <- c(2000, 2003, 2006, 2009, 2012)

# Abstraction for Math, Reading, and science
data.creator <- function(domain="math")
{
	data <- list()
	for (i in years)
	{
		tmp <- mget(paste0("PISA_",i,"_",domain), data.env)[[1]]
		tmp$year <- i
		data[[paste(i)]] <- tmp
		rm(tmp);rm(i)
	}
	data <- do.call(rbind.data.frame, data)
	data$cnt <- gsub("[0-9|\\.]+", "", row.names(data))
	data$gini <- data$gini*100
	data$icc <- data$icc*100
	return(data)
}

math <- data.creator("MATH")
read <- data.creator("READ")
scie <- data.creator("SCIE") 

rm(data.env, file.load, years, data)

math <- subset(math, year %in% c(2000, 2012))
read <- subset(read, year %in% c(2000, 2012))
scie <- subset(scie, year %in% c(2000, 2012))

##### Gini #####
d <- cbind.data.frame(tapply(math$ach, math$cnt, function(x) x[2]-x[1]),
	  tapply(math$gini, math$cnt, function(x) x[2]-x[1]), 
	  tapply(read$ach, read$cnt, function(x) x[2]-x[1]),
	  tapply(read$gini, read$cnt, function(x) x[2]-x[1]),
	  tapply(scie$ach, scie$cnt, function(x) x[2]-x[1]),
	  tapply(scie$gini, scie$cnt, function(x) x[2]-x[1])
	  )
names(d) <- c("math.ach", "math.gini", "read.ach", "read.gini", "scie.ach","scie.gini")

apply(d, 2, range, na.rm=TRUE)
apply(d, 2, mad, na.rm=TRUE)
apply(d, 2, median, na.rm=TRUE)

cor.test(d$math.ach,d$math.gini, use = "pairwise.complete.obs")
cor.test(d$read.ach,d$read.gini, use = "pairwise.complete.obs")
cor.test(d$scie.ach,d$scie.gini, use = "pairwise.complete.obs")

cor.test(d$math.ach,d$math.gini, use = "pairwise.complete.obs",method = "spearman")
cor.test(d$read.ach,d$read.gini, use = "pairwise.complete.obs", method = 'spearman')
cor.test(d$scie.ach,d$scie.gini, use = "pairwise.complete.obs", method = 'spearman')

plot(d$math.ach,d$math.gini, bty = 'n',
	 xlab = "Achievement trend", ylab = "Gini trend",
	 pch = "", main = "")
text(d$math.ach,d$math.gini,row.names(d), cex=.8)
abline(lm(d$math.gini~d$math.ach), col="red")

##### P95 - P5 #####
d <- cbind.data.frame(tapply(math$ach, math$cnt, function(x) x[2]-x[1]),
					  tapply(math$p5.p95, math$cnt, function(x) x[2]-x[1]), 
					  tapply(read$ach, read$cnt, function(x) x[2]-x[1]),
					  tapply(read$p5.p95, read$cnt, function(x) x[2]-x[1]),
					  tapply(scie$ach, scie$cnt, function(x) x[2]-x[1]),
					  tapply(scie$p5.p95, scie$cnt, function(x) x[2]-x[1])
)
names(d) <- c("math.ach", "math.gini", "read.ach", "read.gini", "scie.ach","scie.gini")

apply(d, 2, range, na.rm=TRUE)
apply(d, 2, mad, na.rm=TRUE)
apply(d, 2, median, na.rm=TRUE)

cor.test(d$math.ach,d$math.gini, use = "pairwise.complete.obs")
cor.test(d$read.ach,d$read.gini, use = "pairwise.complete.obs")
cor.test(d$scie.ach,d$scie.gini, use = "pairwise.complete.obs")

cor.test(d$math.ach,d$math.gini, use = "pairwise.complete.obs",method = "spearman")
cor.test(d$read.ach,d$read.gini, use = "pairwise.complete.obs", method = 'spearman')
cor.test(d$scie.ach,d$scie.gini, use = "pairwise.complete.obs", method = 'spearman')

plot(d$math.ach,d$math.gini, bty = 'n',
	 xlab = "Achievement trend", ylab = "P95 - P5 trend",
	 pch = "", main = "")
text(d$math.ach,d$math.gini,row.names(d), cex=.8)
abline(lm(d$math.gini~d$math.ach), col="red")


##### ICC #####
d <- cbind.data.frame(tapply(math$ach, math$cnt, function(x) x[2]-x[1]),
					  tapply(math$icc, math$cnt, function(x) x[2]-x[1]), 
					  tapply(read$ach, read$cnt, function(x) x[2]-x[1]),
					  tapply(read$icc, read$cnt, function(x) x[2]-x[1]),
					  tapply(scie$ach, scie$cnt, function(x) x[2]-x[1]),
					  tapply(scie$icc, scie$cnt, function(x) x[2]-x[1])
)
names(d) <- c("math.ach", "math.gini", "read.ach", "read.gini", "scie.ach","scie.gini")

apply(d[,c(F,T)], 2, range, na.rm=TRUE)
apply(d[,c(F,T)], 2, mad, na.rm=TRUE)
apply(d[,c(F,T)], 2, median, na.rm=TRUE)

cor.test(d$math.ach,d$math.gini, use = "pairwise.complete.obs")
cor.test(d$read.ach,d$read.gini, use = "pairwise.complete.obs")
cor.test(d$scie.ach,d$scie.gini, use = "pairwise.complete.obs")

cor.test(d$math.ach,d$math.gini, use = "pairwise.complete.obs",method = "spearman")
cor.test(d$read.ach,d$read.gini, use = "pairwise.complete.obs", method = 'spearman')
cor.test(d$scie.ach,d$scie.gini, use = "pairwise.complete.obs", method = 'spearman')

plot(d$math.ach,d$math.gini, bty = 'n',
	 xlab = "Achievement trend", ylab = "ICC trend",
	 pch = "", main = "")
text(d$math.ach,d$math.gini,row.names(d), cex=.8)
abline(lm(d$math.gini~d$math.ach), col="red")



d$improve <- apply(d[,c(1,3,5)],1, function(x) sum(x > 20) )
d$decline <- apply(d[,c(1,3,5)],1, function(x) sum(x < -20) )
d$efficent <- apply(d[,c(2,4,6)],1, function(x) sum(x > 1) )
d$equal <- apply(d[,c(2,4,6)],1, function(x) sum(x < -1) )

names(d)

d[which(d$improve >= 2 & d$equal >= 2),]
d[which(d$decline >= 2 & d$efficent >= 2),]



##### P85 - P15 #####
d <- cbind.data.frame(tapply(math$ach, math$cnt, function(x) x[2]-x[1]),
					  tapply(math$p15.p85, math$cnt, function(x) x[2]-x[1]), 
					  tapply(read$ach, read$cnt, function(x) x[2]-x[1]),
					  tapply(read$p15.p85, read$cnt, function(x) x[2]-x[1]),
					  tapply(scie$ach, scie$cnt, function(x) x[2]-x[1]),
					  tapply(scie$p15.p85, scie$cnt, function(x) x[2]-x[1])
)
names(d) <- c("math.ach", "math.gini", "read.ach", "read.gini", "scie.ach", "scie.gini")

apply(d, 2, range, na.rm=TRUE)
apply(d, 2, mad, na.rm=TRUE)
apply(d, 2, median, na.rm=TRUE)

cor.test(d$math.ach,d$math.gini, use = "pairwise.complete.obs")
cor.test(d$read.ach,d$read.gini, use = "pairwise.complete.obs")
cor.test(d$scie.ach,d$scie.gini, use = "pairwise.complete.obs")

cor.test(d$math.ach,d$math.gini, use = "pairwise.complete.obs",method = "spearman")
cor.test(d$read.ach,d$read.gini, use = "pairwise.complete.obs", method = 'spearman')
cor.test(d$scie.ach,d$scie.gini, use = "pairwise.complete.obs", method = 'spearman')
