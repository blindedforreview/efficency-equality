####### Meta data ######
# {_id: "002_StratificationAndAbilityAnalysis_2016-01-05_v.0.0.1" {
# 		{title: "Ability Stratification and Achievement"},
# 		{author: "Philip D Parker"},
# 		{date: "2016-02-18 20:39:45 AEDT"},
# 		{version: "v2.0.1"},
# 		{description: "This file calculates the within and between country estimtes 
# 						of the association between
#						ability stratification and country achievement.
# 	}
# }
#---------------------
library(magrittr)
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
math$ach %<>% scale()
math$gini %<>% scale()
math$icc %<>% scale()
math$p5.p95 %<>% scale()

read <- data.creator("READ")
read$ach %<>% scale()
read$gini %<>% scale()
read$icc %<>% scale()
read$p5.p95 %<>% scale()

scie <- data.creator("SCIE")
scie$ach %<>% scale()
scie$gini %<>% scale()
scie$icc %<>% scale()
scie$p5.p95 %<>% scale()

#Make sure lag maker only lags for a single PISA round
expandData <- function(data)
{
	#splitData by country
	tmp.data <- split(data, data$cnt)
	#Add empty rows for missing years to ensure lag works
	returnData <- list()
	for (i in seq_along(tmp.data))
	{
		tmp.years <- which(!(years %in% tmp.data[[i]]$year))
		if(length(tmp.years)==0){tmp.data[[i]]
		}else{
			tmp.data.frame <- data.frame(ach = NA, gini = NA, icc = NA,
									   	p5 = NA, p95 = NA, p5.p95 = NA,
										year = years[tmp.years],
									   	cnt = names(tmp.data)[i])
			tmp.data[[i]] <- rbind.data.frame(tmp.data[[i]], tmp.data.frame)
			tmp.data[[i]] <- tmp.data[[i]][order(tmp.data[[i]]$year),]
		}
		returnData[[i]] <- tmp.data[[i]]
	}
	returnData <- do.call(rbind.data.frame, returnData)
}

math <- expandData(math)
read <- expandData(read)
scie <- expandData(scie)

rm(data.env, file.load, years, data)
#### Data cleaning ####
library(Hmisc)
lag.maker <- function(d = math, variables = c("gini", "icc", "ach", "p5.p95"), lag = 1, group = "cnt")
{
	return.d <- data.frame(index = 1:nrow(d))
	for (i in seq_along(variables))
	{
		tmp <- tapply(d[[variables[i]]], d[[group]], Lag, shift = lag)
		return.d[[paste0(variables[i], ".lag.", lag)]] <- do.call(c,tmp )
		return.d[[paste0(variables[i], ".change.", lag)]] <- d[[variables[i]]]-return.d[[paste0(variables[i], ".lag.", lag)]]
	}
	return.d
}

math <-cbind.data.frame(math, lag.maker(math))
read <-cbind.data.frame(read, lag.maker(read))
scie <-cbind.data.frame(scie, lag.maker(scie))

math <- data.creator("MATH")
math$ach %<>% scale()
math$gini %<>% scale()
math$icc %<>% scale()
math$p5.p95 %<>% scale()

read <- data.creator("READ")
read$ach %<>% scale()
read$gini %<>% scale()
read$icc %<>% scale()
read$p5.p95 %<>% scale()

scie <- data.creator("SCIE")
scie$ach %<>% scale()
scie$gini %<>% scale()
scie$icc %<>% scale()
scie$p5.p95 %<>% scale()
#------------------
##### Model Runs #####
library(lmeTest)
model.run <- function(domain)
{
	#gini
	gini.results <- lmer(ach ~ gini.change.1 + offset(ach.lag.1*1) + (gini.change.1|cnt) + as.factor(year), data = domain)
	#ICC
	icc.results <- lmer(ach ~ icc.change.1 + offset(ach.lag.1*1) + (icc.change.1|cnt) + as.factor(year), data = domain)
	#p5.p95 
	p95.results <- lmer(ach ~ p5.p95.change.1 + offset(ach.lag.1*1) + (p5.p95.change.1|cnt) + as.factor(year), data = domain,control=lmerControl(optimizer="bobyqa"))
	#return results
	return.results <- list(gini.results = gini.results, icc.results = icc.results,
						   p95.results = p95.results)
}

model.results <- list(math = model.run(math),
					  read = model.run(read),
					  scie = model.run(scie)
)

##### Model Summaries #####
recurse <- function (L, f)
{
	if (inherits(L, "lmerMod")) f(L)
	else lapply(L, recurse, f)
}

fixed.effects.coefficent <- function(x)
{
	tmp <- summary(x)
	tmp$coefficients
}

fixed.effects.results <- recurse(model.results, fixed.effects.coefficent)
