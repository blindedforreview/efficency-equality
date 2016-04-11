####### DESCRIPTION #######
# {_id: "002_Models_PartialTrajectory_2015-12-22_v.2.0.1" {
# 		{title: "Achievement Inequality versus Performance: Where is the Trade-Off?"},
# 		{author: "Blinded"},
# 		{date: "2016-04-04 16:32:37 AEST"},
# 		{version: "v2.0.1"},
# 		{description: "Fits multilevel models and:
# 						1) Tests significance of random effects;
# 						2) Extracts country specific estimates
#						3) Plots differences
#						4) Estimates Correlation
#		"}
# 	}
# }
#---------------------

library(car)			# recode variables
library(lmerTest)		# Fit multilevel models and gain access to quick lrt.

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

####Overaching correations
#------------------

math$time <- recode(math$year, "'2000'=1;'2003'=2;'2006'=3;'2009'=4;'2012'=5")
read$time <- recode(read$year, "'2000'=1;'2003'=2;'2006'=3;'2009'=4;'2012'=5")
scie$time <- recode(scie$year, "'2000'=1;'2003'=2;'2006'=3;'2009'=4;'2012'=5")

#### Math Trajectories ####
#achievement
m0.ach <- lmer(ach ~ time + (1|cnt), data = math)
rand(m0.ach)
m1.ach <- lmer(ach ~ time + (time|cnt), data = math)
summary(m1.ach)
rand(m1.ach)
#gini
m0.gini <- lmer(gini ~ time + (1|cnt), data = math)
rand(m0.gini)
m1.gini <- lmer(gini ~ time + (time|cnt), data = math)
summary(m1.gini)
rand(m1.gini)
#icc
m0.icc <- lmer(icc ~ time + (1|cnt), data = math)
rand(m0.icc)
m1.icc <- lmer(icc ~ time + (time|cnt), data = math)
summary(m1.icc)
rand(m1.icc)
#p5.p95
m0.p5.p95 <- lmer(p5.p95 ~ time + (1|cnt), data = math)
rand(m0.p5.p95)
m1.p5.p95 <- lmer(p5.p95 ~ time + (time|cnt), data = math)
summary(m1.p5.p95)
rand(m1.p5.p95)
#achievement results
fix.ach <- fixef(m1.ach)
ran.ach <- ranef(m1.ach)
results.ach <- fix.ach[2]+ran.ach$cnt[,2]
#gini results
fix.gini <- fixef(m1.gini)
ran.gini <- ranef(m1.gini)
results.gini <- fix.gini[2]+ran.gini$cnt[,2]
#icc results
fix.icc <- fixef(m1.icc)
ran.icc <- ranef(m1.icc)
results.icc <- fix.icc[2]+ran.icc$cnt[,2]
#p5.p95 results
fix.p5.p95 <- fixef(m1.p5.p95)
ran.p5.p95 <- ranef(m1.p5.p95)
results.p5.p95 <- fix.p5.p95[2]+ran.p5.p95$cnt[,2]

math.results <- cbind.data.frame(row.names(ran.gini$cnt),results.ach, results.gini, results.icc, results.p5.p95)
names(math.results)[1] <- 'cnt'

##correlations
#Pearson
cor.test(math.results$results.ach,math.results$results.gini)
cor.test(math.results$results.ach,math.results$results.icc)
cor.test(math.results$results.ach,math.results$results.p5.p95)
#Spearman
cor.test(math.results$results.ach,math.results$results.gini,method = 'spearman')
cor.test(math.results$results.ach,math.results$results.icc,method = 'spearman')
cor.test(math.results$results.ach,math.results$results.p5.p95,method = 'spearman')

par(mfrow=c(1,3))
#panel a Gini
plot(results.ach,results.gini, bty = 'n',
	 xlab = "Achievement trend", ylab = "Gini trend",
	 pch = "", main = "(a)")
text(results.ach,results.gini,row.names(ran.gini$cnt), cex=.8)
abline(lm(results.gini~results.ach), col="grey")
#panel b ICC
plot(results.ach,results.icc, bty = 'n',
	 xlab = "Achievement trend", ylab = "ICC trend",
	 pch = "", main = "(b)")
text(results.ach,results.icc,row.names(ran.icc$cnt), cex=.8)
abline(lm(results.icc~results.ach), col="grey")
#panel c p5.p95
plot(results.ach,results.p5.p95, bty = 'n',
	 xlab = "Achievement trend", ylab = "P95 - P5 trend",
	 pch = "", main = "(C)")
text(results.ach,results.p5.p95,row.names(ran.p5.p95$cnt), cex=.8)
abline(lm(results.p5.p95~results.ach), col="grey")

#### Reading Trajectories ####
#achievement
m0.ach <- lmer(ach ~ time + (1|cnt), data = read)
rand(m0.ach)
m1.ach <- lmer(ach ~ time + (time|cnt), data = read)
summary(m1.ach)
rand(m1.ach)
#gini
m0.gini <- lmer(gini ~ time + (1|cnt), data = read)
rand(m0.gini)
m1.gini <- lmer(gini ~ time + (time|cnt), data = read)
summary(m1.gini)
rand(m1.gini)
#icc
m0.icc <- lmer(icc ~ time + (1|cnt), data = read)
rand(m0.icc)
m1.icc <- lmer(icc ~ time + (time|cnt), data = read)
summary(m1.icc)
rand(m1.icc)
#p5.p95
m0.p5.p95 <- lmer(p5.p95 ~ time + (1|cnt), data = read)
rand(m0.p5.p95)
m1.p5.p95 <- lmer(p5.p95 ~ time + (time|cnt), data = read)
summary(m1.p5.p95)
rand(m1.p5.p95)
#achievement results
fix.ach <- fixef(m1.ach)
ran.ach <- ranef(m1.ach)
results.ach <- fix.ach[2]+ran.ach$cnt[,2]
#gini results
fix.gini <- fixef(m1.gini)
ran.gini <- ranef(m1.gini)
results.gini <- fix.gini[2]+ran.gini$cnt[,2]
#icc results
fix.icc <- fixef(m1.icc)
ran.icc <- ranef(m1.icc)
results.icc <- fix.icc[2]+ran.icc$cnt[,2]
#p5.p95 results
fix.p5.p95 <- fixef(m1.p5.p95)
ran.p5.p95 <- ranef(m1.p5.p95)
results.p5.p95 <- fix.p5.p95[2]+ran.p5.p95$cnt[,2]

read.results <- cbind.data.frame(row.names(ran.gini$cnt),results.ach, results.gini, results.icc, results.p5.p95)
names(read.results)[1] <- 'cnt'

##correlations
#Pearson
cor.test(read.results$results.ach,read.results$results.gini)
cor.test(read.results$results.ach,read.results$results.icc)
cor.test(read.results$results.ach,read.results$results.p5.p95)
#Spearman
cor.test(read.results$results.ach,read.results$results.gini,method = 'spearman')
cor.test(read.results$results.ach,read.results$results.icc,method = 'spearman')
cor.test(read.results$results.ach,read.results$results.p5.p95,method = 'spearman')

par(mfrow=c(1,3))
#panel a Gini
plot(results.ach,results.gini, bty = 'n',
	 xlab = "Achievement trend", ylab = "Gini trend",
	 pch = "", main = "(a)")
text(results.ach,results.gini,row.names(ran.gini$cnt), cex=.8)
abline(lm(results.gini~results.ach), col="grey")
#panel b ICC
plot(results.ach,results.icc, bty = 'n',
	 xlab = "Achievement trend", ylab = "ICC trend",
	 pch = "", main = "(b)")
text(results.ach,results.icc,row.names(ran.icc$cnt), cex=.8)
abline(lm(results.icc~results.ach), col="grey")
#panel c p5.p95
plot(results.ach,results.p5.p95, bty = 'n',
	 xlab = "Achievement trend", ylab = "P95 - P5 trend",
	 pch = "", main = "(C)")
text(results.ach,results.p5.p95,row.names(ran.p5.p95$cnt), cex=.8)
abline(lm(results.p5.p95~results.ach), col="grey")

 #### Science Trajectories ####
#achievement
m0.ach <- lmer(ach ~ time + (1|cnt), data = scie)
rand(m0.ach)
m1.ach <- lmer(ach ~ time + (time|cnt), data = scie)
summary(m1.ach)
rand(m1.ach)
#gini
m0.gini <- lmer(gini ~ time + (1|cnt), data = scie)
rand(m0.gini)
m1.gini <- lmer(gini ~ time + (time|cnt), data = scie)
summary(m1.gini)
rand(m1.gini)
#icc
m0.icc <- lmer(icc ~ time + (1|cnt), data = scie)
rand(m0.icc)
m1.icc <- lmer(icc ~ time + (time|cnt), data = scie)
summary(m1.icc)
rand(m1.icc)
#p5.p95
m0.p5.p95 <- lmer(p5.p95 ~ time + (1|cnt), data = scie)
rand(m0.p5.p95)
m1.p5.p95 <- lmer(p5.p95 ~ time + (time|cnt), data = scie)
summary(m1.p5.p95)
rand(m1.p5.p95)
#achievement results
fix.ach <- fixef(m1.ach)
ran.ach <- ranef(m1.ach)
results.ach <- fix.ach[2]+ran.ach$cnt[,2]
#gini results
fix.gini <- fixef(m1.gini)
ran.gini <- ranef(m1.gini)
results.gini <- fix.gini[2]+ran.gini$cnt[,2]
#icc results
fix.icc <- fixef(m1.icc)
ran.icc <- ranef(m1.icc)
results.icc <- fix.icc[2]+ran.icc$cnt[,2]
#p5.p95 results
fix.p5.p95 <- fixef(m1.p5.p95)
ran.p5.p95 <- ranef(m1.p5.p95)
results.p5.p95 <- fix.p5.p95[2]+ran.p5.p95$cnt[,2]

scie.results <- cbind.data.frame(row.names(ran.gini$cnt),results.ach, results.gini, results.icc, results.p5.p95)
names(scie.results)[1] <- 'cnt'

##correlations
#Pearson
cor.test(scie.results$results.ach,scie.results$results.gini)
cor.test(scie.results$results.ach,scie.results$results.icc)
cor.test(scie.results$results.ach,scie.results$results.p5.p95)
#Spearman
cor.test(scie.results$results.ach,scie.results$results.gini,method = 'spearman')
cor.test(scie.results$results.ach,scie.results$results.icc,method = 'spearman')
cor.test(scie.results$results.ach,scie.results$results.p5.p95,method = 'spearman')

par(mfrow=c(1,3))
#panel a Gini
plot(results.ach,results.gini, bty = 'n',
	 xlab = "Achievement trend", ylab = "Gini trend",
	 pch = "", main = "(a)")
text(results.ach,results.gini,row.names(ran.gini$cnt), cex=.8)
abline(lm(results.gini~results.ach), col="grey")
#cor.est <- cor.test(results.ach,results.gini)
#text(x = -3, y = -.3,labels = round(cor.est$estimate,3))
#panel b ICC
plot(results.ach,results.icc, bty = 'n',
	 xlab = "Achievement trend", ylab = "ICC trend",
	 pch = "", main = "(b)")
text(results.ach,results.icc,row.names(ran.icc$cnt), cex=.8)
abline(lm(results.icc~results.ach), col="grey")
#cor.est <- cor.test(results.ach,results.icc)
#text(x = -3, y = -3,labels = round(cor.est$estimate,3))
#panel c p5.p95
plot(results.ach,results.p5.p95, bty = 'n',
	 xlab = "Achievement trend", ylab = "P95 - P5 trend",
	 pch = "", main = "(C)")
text(results.ach,results.p5.p95,row.names(ran.p5.p95$cnt), cex=.8)
abline(lm(results.p5.p95~results.ach), col="grey")
#cor.est <- cor.test(results.ach,results.p5.p95)
#text(x = -3, y = -4,labels = round(cor.est$estimate,3))

results <- list(math.results, read.results, scie.results) 

save(results,file = "./PARTIAL_TRAJECTORY.RData")
