####### DESCRIPTION #######
# {_id: "001_MetaData_PISA2006_2015-12-22_v.2.0.1" {
# 		{title: "Achievement Inequality versus Performance: Where is the Trade-Off?"},
# 		{author: "Blinded"},
# 		{date: "2016-04-04 16:32:37 AEST"},
# 		{version: "v2.0.1"},
# 		{description: "Construction of meta data:
# 						1) Gini: A sale free estimate of the amount of variance;
# 						2) ICC: Homogeneity of schools
#						3) p5-p95: Distance between the 5th and 95th percentile
#						4) Achievement: The average achievement for a given country
#		"}
# 	}
# }
#---------------------

#####Data Preperation #####
library(RSQLite)		# Used to extract data from SQL database
library(reldist)		# Used to calculate weighted Gini and quantiles
library(multilevel)		# Used to calculate ICCs

#Connect to database
LSAY <- dbConnect(SQLite(), dbname="~/Dropbox/Databases/LSAY_DATSETS/LSAY.sqlite")

#### Math: PISA2006 #####
MATH <- dbGetQuery(LSAY, "SELECT CNT,SCHOOLID, PV1MATH, PV2MATH, PV3MATH, PV4MATH, PV5MATH, W_FSTUWT FROM PISA2006
				   where cnt in ('AUS','AUT','BEL','CAN','CZE','DNK','FIN',
		  						'FRA','DEU','GRC','HUN','ISL','IRL','ITA',
				   				'JPN','KOR','LUX','NLD','NZL','NOR','POL',
				   				'PRT','SVK','ESP','SWE','CHE','GBR','USA')"
)

PISA_2006_MATH_ach <- sapply(split(MATH, MATH$CNT),
						   function(x) apply(x[,3:7],2,
	   							  function(z) weighted.mean(z,w = x$W_FSTUWT, na.rm=TRUE)
	   						)
)

PISA_2006_MATH_gini <- sapply(split(MATH, MATH$CNT),
						 function(x) apply(x[,3:7],2,
						 				  function(z) gini(z,w = x$W_FSTUWT)
						 )
)

PISA_2006_MATH_ICC <- sapply(split(MATH, MATH$CNT),
							  function(x) apply(x[,3:7],2,
							  				  function(z) ICC1(aov(z~x$SCHOOLID,w = x$W_FSTUWT))
							  )
)

PISA_2006_MATH_P5 <- sapply(split(MATH, MATH$CNT),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.05, weight = x$W_FSTUWT, na.rm=TRUE)
							)
)


PISA_2006_MATH_P95 <- sapply(split(MATH, MATH$CNT),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.95, weight = x$W_FSTUWT, na.rm=TRUE)
							 )
)

PISA_2006_MATH_P15 <- sapply(split(MATH, MATH$CNT),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.15, weight = x$W_FSTUWT, na.rm=TRUE)
							)
)


PISA_2006_MATH_P85 <- sapply(split(MATH, MATH$CNT),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.85, weight = x$W_FSTUWT, na.rm=TRUE)
							 )
)

PISA_2006_MATH <- data.frame(ach = colMeans(PISA_2006_MATH_ach),
							 gini = colMeans(PISA_2006_MATH_gini),
							 icc = colMeans(PISA_2006_MATH_ICC),
							 p5 = colMeans(PISA_2006_MATH_P5),
							 p95 = colMeans(PISA_2006_MATH_P95),
							 p5.p95 = colMeans(PISA_2006_MATH_P95) - colMeans(PISA_2006_MATH_P5),
							 p15 = colMeans(PISA_2006_MATH_P15),
							 p85 = colMeans(PISA_2006_MATH_P85),
							 p15.p85 = colMeans(PISA_2006_MATH_P85) - colMeans(PISA_2006_MATH_P15)
)


save(PISA_2006_MATH,file = "~/Dropbox/Projects_Research/PISA_inequality/replication/data/PISA_2006_math_v1.0.1.RData")
rm(list=ls(pattern="MATH"))

#### READING: PISA 2006 ####
READ <- dbGetQuery(LSAY, "SELECT CNT,SCHOOLID, PV1READ, PV2READ, PV3READ, PV4READ, PV5READ, W_FSTUWT FROM PISA2006
				   where cnt in ('AUS','AUT','BEL','CAN','CZE','DNK','FIN',
		  						'FRA','DEU','GRC','HUN','ISL','IRL','ITA',
				   				'JPN','KOR','LUX','NLD','NZL','NOR','POL',
				   				'PRT','SVK','ESP','SWE','CHE','GBR','USA')"
)
READ <-na.omit(READ)

PISA_2006_READ_ach <- sapply(split(READ, READ$CNT),
							 function(x) apply(x[,3:7],2,
							 				  function(z) weighted.mean(z,w = x$W_FSTUWT, na.rm=TRUE)
							 )
)

PISA_2006_READ_gini <- sapply(split(READ, READ$CNT),
							  function(x) apply(x[,3:7],2,
							  				  function(z) gini(z,w = x$W_FSTUWT)
							  )
)

PISA_2006_READ_ICC <- sapply(split(READ, READ$CNT),
							 function(x) apply(x[,5:6],2,
							 				  function(z) ICC1(aov(z~as.factor(x$SCHOOLID),w = x$W_FSTUWT))
							 )
)

PISA_2006_READ_P5 <- sapply(split(READ, READ$CNT),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.05, weight = x$W_FSTUWT, na.rm=TRUE)
							)
)


PISA_2006_READ_P95 <- sapply(split(READ, READ$CNT),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.95, weight = x$W_FSTUWT, na.rm=TRUE)
							 )
)

PISA_2006_READ_P15 <- sapply(split(READ, READ$CNT),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.15, weight = x$W_FSTUWT, na.rm=TRUE)
							)
)


PISA_2006_READ_P85 <- sapply(split(READ, READ$CNT),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.85, weight = x$W_FSTUWT, na.rm=TRUE)
							 )
)

PISA_2006_READ <- data.frame(ach = colMeans(PISA_2006_READ_ach),
							 gini = colMeans(PISA_2006_READ_gini),
							 icc = colMeans(PISA_2006_READ_ICC),
							 p5 = colMeans(PISA_2006_READ_P5),
							 p95 = colMeans(PISA_2006_READ_P95),
							 p5.p95 = colMeans(PISA_2006_READ_P95) - colMeans(PISA_2006_READ_P5),
							 p15 = colMeans(PISA_2006_READ_P15),
							 p85 = colMeans(PISA_2006_READ_P85),
							 p15.p85 = colMeans(PISA_2006_READ_P85) - colMeans(PISA_2006_READ_P15)
)

save(PISA_2006_READ,file = "~/Dropbox/Projects_Research/PISA_inequality/replication/data/PISA_2006_read_v1.0.1.RData")
rm(list=ls(pattern="READ"))

#### SCIENCE: PISA 2006 ####
SCIE <- dbGetQuery(LSAY, "SELECT CNT,SCHOOLID, PV1SCIE, PV2SCIE, PV3SCIE, PV4SCIE, PV5SCIE, W_FSTUWT FROM PISA2006
				   where cnt in ('AUS','AUT','BEL','CAN','CZE','DNK','FIN',
		  						'FRA','DEU','GRC','HUN','ISL','IRL','ITA',
				   				'JPN','KOR','LUX','NLD','NZL','NOR','POL',
				   				'PRT','SVK','ESP','SWE','CHE','GBR','USA')"
)

PISA_2006_SCIE_ach <- sapply(split(SCIE, SCIE$CNT),
							 function(x) apply(x[,3:7],2,
							 				  function(z) weighted.mean(z,w = x$W_FSTUWT, na.rm=TRUE)
							 )
)

PISA_2006_SCIE_gini <- sapply(split(SCIE, SCIE$CNT),
							  function(x) apply(x[,3:7],2,
							  				  function(z) gini(z,w = x$W_FSTUWT)
							  )
)

PISA_2006_SCIE_ICC <- sapply(split(SCIE, SCIE$CNT),
							 function(x) apply(x[,3:7],2,
							 				  function(z) ICC1(aov(z~x$SCHOOLID,w = x$W_FSTUWT))
							 )
)

PISA_2006_SCIE_P5 <- sapply(split(SCIE, SCIE$CNT),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.05, weight = x$W_FSTUWT, na.rm=TRUE)
							)
)


PISA_2006_SCIE_P95 <- sapply(split(SCIE, SCIE$CNT),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.95, weight = x$W_FSTUWT, na.rm=TRUE)
							 )
)

PISA_2006_SCIE_P15 <- sapply(split(SCIE, SCIE$CNT),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.15, weight = x$W_FSTUWT, na.rm=TRUE)
							)
)


PISA_2006_SCIE_P85 <- sapply(split(SCIE, SCIE$CNT),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.85, weight = x$W_FSTUWT, na.rm=TRUE)
							 )
)

PISA_2006_SCIE <- data.frame(ach = colMeans(PISA_2006_SCIE_ach),
							 gini = colMeans(PISA_2006_SCIE_gini),
							 icc = colMeans(PISA_2006_SCIE_ICC),
							 p5 = colMeans(PISA_2006_SCIE_P5),
							 p95 = colMeans(PISA_2006_SCIE_P95),
							 p5.p95 = colMeans(PISA_2006_SCIE_P95) - colMeans(PISA_2006_SCIE_P5),
							 p15 = colMeans(PISA_2006_SCIE_P15),
							 p85 = colMeans(PISA_2006_SCIE_P85),
							 p15.p85 = colMeans(PISA_2006_SCIE_P85) - colMeans(PISA_2006_SCIE_P15)
)

save(PISA_2006_SCIE,file = "~/Dropbox/Projects_Research/PISA_inequality/replication/data/PISA_2006_scie_v1.0.1.RData")
