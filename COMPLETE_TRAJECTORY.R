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

library(car)

math$time <- recode(math$year, "'2000'=1;'2003'=2;'2006'=3;'2009'=4;'2012'=5")
read$time <- recode(read$year, "'2000'=1;'2003'=2;'2006'=3;'2009'=4;'2012'=5")
scie$time <- recode(scie$year, "'2000'=1;'2003'=2;'2006'=3;'2009'=4;'2012'=5")

#### Math Trajectories ####
#achievement
m0.ach <- lm(ach ~ time*as.factor(cnt), data = math)
summary(m0.ach)
m0.ach.out <- c(coef(m0.ach)[2],coef(m0.ach)[2]+coef(m0.ach)[30:56])
#gini
m0.gini <- lm(gini ~ time*as.factor(cnt), data = math)
summary(m0.gini)
m0.gini.out <- c(coef(m0.gini)[2],coef(m0.gini)[2]+coef(m0.gini)[30:56])

cor.test(m0.ach.out, m0.gini.out)
cor.test(m0.ach.out, m0.gini.out, method = 'spearman')
#icc
m0.icc <- lm(icc ~ time*as.factor(cnt), data = math)
summary(m0.icc)
m0.icc.out <- c(coef(m0.icc)[2],coef(m0.icc)[2]+coef(m0.icc)[30:56])

cor.test(m0.ach.out, m0.icc.out)
cor.test(m0.ach.out, m0.icc.out, method = 'spearman')
#p5.p95
m0.p5.p95 <- lm(p5.p95 ~ time*as.factor(cnt), data = math)
summary(m0.p5.p95)
m0.p5.p95.out <- c(coef(m0.p5.p95)[2],coef(m0.p5.p95)[2]+coef(m0.p5.p95)[30:56])

cor.test(m0.ach.out, m0.p5.p95.out)
cor.test(m0.ach.out, m0.p5.p95.out,method = 'spearman')

math.results <- cbind.data.frame(unique(math$cnt),m0.ach.out,m0.gini.out,m0.icc.out,m0.p5.p95.out)
row.names(math.results) <- NULL
names(math.results) <- c('cnt', 'results.ach', 'results.gini', 'results.icc', 'results.p5.p95')

par(mfrow=c(1,3))
#panel a Gini
plot(m0.ach.out,m0.gini.out, bty = 'n',
	 xlab = "Achievement trend", ylab = "Gini trend",
	 pch = "", main = "(a)")
text(m0.ach.out,m0.gini.out,unique(math$cnt), cex=.8)
abline(lm(m0.gini.out~m0.ach.out), col="grey")

#panel b ICC
plot(m0.ach.out,m0.icc.out, bty = 'n',
	 xlab = "Achievement trend", ylab = "ICC trend",
	 pch = "", main = "(a)")
text(m0.ach.out,m0.icc.out,unique(math$cnt), cex=.8)
abline(lm(m0.icc.out~m0.ach.out), col="grey")
#panel c p5.p95
plot(m0.ach.out,m0.p5.p95.out, bty = 'n',
	 xlab = "Achievement trend", ylab = "P95 - P5 trend",
	 pch = "", main = "(a)")
text(m0.ach.out,m0.p5.p95.out,unique(math$cnt), cex=.8)
abline(lm(m0.p5.p95.out~m0.ach.out), col="grey")

#### Reading Trajectories ####
#achievement
m0.ach <- lm(ach ~ time*as.factor(cnt), data = read)
summary(m0.ach)
m0.ach.out <- c(coef(m0.ach)[2],coef(m0.ach)[2]+coef(m0.ach)[30:56])
#gini
m0.gini <- lm(gini ~ time*as.factor(cnt), data = read)
summary(m0.gini)
m0.gini.out <- c(coef(m0.gini)[2],coef(m0.gini)[2]+coef(m0.gini)[30:56])

cor.test(m0.ach.out, m0.gini.out)
cor.test(m0.ach.out, m0.gini.out,method = 'spearman')
#icc
m0.icc <- lm(icc ~ time*as.factor(cnt), data = read)
summary(m0.icc)
m0.icc.out <- c(coef(m0.icc)[2],coef(m0.icc)[2]+coef(m0.icc)[30:56])

cor.test(m0.ach.out, m0.icc.out)
cor.test(m0.ach.out, m0.icc.out,method = 'spearman')
#p5.p95
m0.p5.p95 <- lm(p5.p95 ~ time*as.factor(cnt), data = read)
summary(m0.p5.p95)
m0.p5.p95.out <- c(coef(m0.p5.p95)[2],coef(m0.p5.p95)[2]+coef(m0.p5.p95)[30:56])

cor.test(m0.ach.out, m0.p5.p95.out)
cor.test(m0.ach.out, m0.p5.p95.out,method = 'spearman')

read.results <- cbind.data.frame(unique(read$cnt),m0.ach.out,m0.gini.out,m0.icc.out,m0.p5.p95.out)
row.names(read.results) <- NULL
names(read.results) <- c('cnt', 'results.ach', 'results.gini', 'results.icc', 'results.p5.p95')

#panel a Gini
plot(m0.ach.out,m0.gini.out, bty = 'n',
	 xlab = "Achievement trend", ylab = "Gini trend",
	 pch = "", main = "(a)")
text(m0.ach.out,m0.gini.out,unique(read$cnt), cex=.8)
abline(lm(m0.gini.out~m0.ach.out), col="grey")

#panel b ICC
plot(m0.ach.out,m0.icc.out, bty = 'n',
	 xlab = "Achievement trend", ylab = "ICC trend",
	 pch = "", main = "(a)")
text(m0.ach.out,m0.icc.out,unique(read$cnt), cex=.8)
abline(lm(m0.icc.out~m0.ach.out), col="grey")

#panel c p5.p95
plot(m0.ach.out,m0.p5.p95.out, bty = 'n',
	 xlab = "Achievement trend", ylab = "P95 - P5 trend",
	 pch = "", main = "(a)")
text(m0.ach.out,m0.p5.p95.out,unique(read$cnt), cex=.8)
abline(lm(m0.p5.p95.out~m0.ach.out), col="grey")

#### Science Trajectories ####
#achievement
m0.ach <- lm(ach ~ time*as.factor(cnt), data = scie)
summary(m0.ach)
m0.ach.out <- c(coef(m0.ach)[2],coef(m0.ach)[2]+coef(m0.ach)[30:56])
#gini
m0.gini <- lm(gini ~ time*as.factor(cnt), data = scie)
summary(m0.gini)
m0.gini.out <- c(coef(m0.gini)[2],coef(m0.gini)[2]+coef(m0.gini)[30:56])

cor.test(m0.ach.out, m0.gini.out)
cor.test(m0.ach.out, m0.gini.out,method = 'spearman')
#icc
m0.icc <- lm(icc ~ time*as.factor(cnt), data = scie)
summary(m0.icc)
m0.icc.out <- c(coef(m0.icc)[2],coef(m0.icc)[2]+coef(m0.icc)[30:56])

cor.test(m0.ach.out, m0.icc.out)
cor.test(m0.ach.out, m0.icc.out,method = 'spearman')
#p5.p95
m0.p5.p95 <- lm(p5.p95 ~ time*as.factor(cnt), data = scie)
summary(m0.p5.p95)
m0.p5.p95.out <- c(coef(m0.p5.p95)[2],coef(m0.p5.p95)[2]+coef(m0.p5.p95)[30:56])

cor.test(m0.ach.out, m0.p5.p95.out)
cor.test(m0.ach.out, m0.p5.p95.out,method = 'spearman')

scie.results <- cbind.data.frame(unique(scie$cnt),m0.ach.out,m0.gini.out,m0.icc.out,m0.p5.p95.out)
row.names(scie.results) <- NULL
names(scie.results) <- c('cnt', 'results.ach', 'results.gini', 'results.icc', 'results.p5.p95')

#panel a Gini
plot(m0.ach.out,m0.gini.out, bty = 'n',
	 xlab = "Achievement trend", ylab = "Gini trend",
	 pch = "", main = "(a)")
text(m0.ach.out,m0.gini.out,unique(scie$cnt), cex=.8)
abline(lm(m0.gini.out~m0.ach.out), col="grey")
#panel b ICC
plot(m0.ach.out,m0.icc.out, bty = 'n',
	 xlab = "Achievement trend", ylab = "ICC trend",
	 pch = "", main = "(a)")
text(m0.ach.out,m0.icc.out,unique(scie$cnt), cex=.8)
abline(lm(m0.icc.out~m0.ach.out), col="grey")
#panel c p5.p95
plot(m0.ach.out,m0.p5.p95.out, bty = 'n',
	 xlab = "Achievement trend", ylab = "P95 - P5 trend",
	 pch = "", main = "(a)")
text(m0.ach.out,m0.p5.p95.out,unique(scie$cnt), cex=.8)
abline(lm(m0.p5.p95.out~m0.ach.out), col="grey")

results <- list(math.results,read.results,scie.results)
save(results,file = "./submission/COMPLETE_TRAJECTORY.RData")
