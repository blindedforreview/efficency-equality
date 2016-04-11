####### DESCRIPTION #######
# {_id: "001_MetaData_PISA2000_2015-12-22_v.2.0.1" {
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

#### Math: PISA2000 #####

# Query database
MATH <- dbGetQuery(LSAY, "SELECT cnt,SCHOOLID, PV1MATH, PV2MATH, PV3MATH, PV4MATH, PV5MATH, W_FSTUWT FROM PISA2000_MATH
				   where cnt in ('AUS','AUT','BEL','CAN','CZE','DNK','FIN',
		  						'FRA','DEU','GRC','HUN','ISL','IRL','ITA',
				   				'JPN','KOR','LUX','NLD','NZL','NOR','POL',
				   				'PRT','SVK','ESP','SWE','CHE','GBR','USA')"
)
# Get weighted mean for each country from database
PISA_2000_MATH_ach <- sapply(split(MATH, MATH$cnt),
	   					function(x) apply(x[,3:7],2,
	   				  				function(z) weighted.mean(z,w = x$w_fstuwt, na.rm=TRUE)
	   					)
)
# Get weighted Gini index from 
PISA_2000_MATH_gini <- sapply(split(MATH, MATH$cnt),
						 function(x) apply(x[,3:7],2,
						 				  function(z) gini(z,w = x$w_fstuwt)
						 )
)

PISA_2000_MATH_ICC <- sapply(split(MATH, MATH$cnt),
							  function(x) apply(x[,3:7],2,
							  				  function(z) ICC1(aov(z~x$SCHOOLID,w = x$w_fstuwt))
							  )
)

PISA_2000_MATH_P5 <- sapply(split(MATH, MATH$cnt),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.05, weight = x$w_fstuwt, na.rm=TRUE)
											  )
)


PISA_2000_MATH_P95 <- sapply(split(MATH, MATH$cnt),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.95, weight = x$w_fstuwt, na.rm=TRUE)
											  )
)

PISA_2000_MATH_P15 <- sapply(split(MATH, MATH$cnt),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.15, weight = x$w_fstuwt, na.rm=TRUE)
							)
)


PISA_2000_MATH_P85 <- sapply(split(MATH, MATH$cnt),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.85, weight = x$w_fstuwt, na.rm=TRUE)
							 )
)

PISA_2000_MATH <- data.frame(ach = colMeans(PISA_2000_MATH_ach),
							 gini = colMeans(PISA_2000_MATH_gini),
							 icc = colMeans(PISA_2000_MATH_ICC),
							 p5 = colMeans(PISA_2000_MATH_P5),
							 p95 = colMeans(PISA_2000_MATH_P95),
							 p5.p95 = colMeans(PISA_2000_MATH_P95) - colMeans(PISA_2000_MATH_P5),
							 p15 = colMeans(PISA_2000_MATH_P15),
							 p85 = colMeans(PISA_2000_MATH_P85),
							 p15.p85 = colMeans(PISA_2000_MATH_P85) - colMeans(PISA_2000_MATH_P15)
)

save(PISA_2000_MATH,file = "~/Dropbox/Projects_Research/PISA_inequality/replication/data/PISA_2000_math_v1.0.1.RData")
rm(list=ls(pattern="MATH"))
#### READING: PISA 2000 ####
READ <- dbGetQuery(LSAY, "SELECT cnt,SCHOOLID, PV1READ, PV2READ, PV3READ, PV4READ, PV5READ, W_FSTUWT FROM PISA2000_READ
				   where cnt in ('AUS','AUT','BEL','CAN','CZE','DNK','FIN',
		  						'FRA','DEU','GRC','HUN','ISL','IRL','ITA',
				   				'JPN','KOR','LUX','NLD','NZL','NOR','POL',
				   				'PRT','SVK','ESP','SWE','CHE','GBR','USA')"
)

PISA_2000_READ_ach <- sapply(split(READ, READ$cnt),
							 function(x) apply(x[,3:7],2,
							 				  function(z) weighted.mean(z,w = x$w_fstuwt, na.rm=TRUE)
							 )
)

PISA_2000_READ_gini <- sapply(split(READ, READ$cnt),
							  function(x) apply(x[,3:7],2,
							  				  function(z) gini(z,w = x$w_fstuwt)
							  )
)

PISA_2000_READ_ICC <- sapply(split(READ, READ$cnt),
							 function(x) apply(x[,3:7],2,
							 				  function(z) ICC1(aov(z~x$SCHOOLID,w = x$w_fstuwt))
							 )
)


PISA_2000_READ_P5 <- sapply(split(READ, READ$cnt),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.05, weight = x$w_fstuwt, na.rm=TRUE)
							)
)


PISA_2000_READ_P95 <- sapply(split(READ, READ$cnt),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.95, weight = x$w_fstuwt, na.rm=TRUE)
							 )
)

PISA_2000_READ_P15 <- sapply(split(READ, READ$cnt),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.15, weight = x$w_fstuwt, na.rm=TRUE)
							)
)


PISA_2000_READ_P85 <- sapply(split(READ, READ$cnt),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.85, weight = x$w_fstuwt, na.rm=TRUE)
							 )
)

PISA_2000_READ <- data.frame(ach = colMeans(PISA_2000_READ_ach),
							 gini = colMeans(PISA_2000_READ_gini),
							 icc = colMeans(PISA_2000_READ_ICC),
							 p5 = colMeans(PISA_2000_READ_P5),
							 p95 = colMeans(PISA_2000_READ_P95),
							 p5.p95 = colMeans(PISA_2000_READ_P95) - colMeans(PISA_2000_READ_P5),
							 p15 = colMeans(PISA_2000_READ_P15),
							 p85 = colMeans(PISA_2000_READ_P85),
							 p15.p85 = colMeans(PISA_2000_READ_P85) - colMeans(PISA_2000_READ_P15)
)

save(PISA_2000_READ,file = "~/Dropbox/Projects_Research/PISA_inequality/replication/data/PISA_2000_read_v1.0.1.RData")
rm(list=ls(pattern="READ"))
#### SCIENCE: PISA 2000 ####
SCIE <- dbGetQuery(LSAY, "SELECT cnt,SCHOOLID, PV1SCIE, PV2SCIE, PV3SCIE, PV4SCIE, PV5SCIE, W_FSTUWT FROM PISA2000_SCIENCE
				   where cnt in ('AUS','AUT','BEL','CAN','CZE','DNK','FIN',
		  						'FRA','DEU','GRC','HUN','ISL','IRL','ITA',
				   				'JPN','KOR','LUX','NLD','NZL','NOR','POL',
				   				'PRT','SVK','ESP','SWE','CHE','GBR','USA')"
)

PISA_2000_SCIE_ach <- sapply(split(SCIE, SCIE$cnt),
							 function(x) apply(x[,3:7],2,
							 				  function(z) weighted.mean(z,w = x$w_fstuwt, na.rm=TRUE)
							 )
)

PISA_2000_SCIE_gini <- sapply(split(SCIE, SCIE$cnt),
							  function(x) apply(x[,3:7],2,
							  				  function(z) gini(z,w = x$w_fstuwt)
							  )
)

PISA_2000_SCIE_ICC <- sapply(split(SCIE, SCIE$cnt),
							 function(x) apply(x[,3:7],2,
							 				  function(z) ICC1(aov(z~x$SCHOOLID,w = x$w_fstuwt))
							 )
)

PISA_2000_SCIE_P5 <- sapply(split(SCIE, SCIE$cnt),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.05, weight = x$w_fstuwt, na.rm=TRUE)
							)
)


PISA_2000_SCIE_P95 <- sapply(split(SCIE, SCIE$cnt),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.95, weight = x$w_fstuwt, na.rm=TRUE)
							 )
)

PISA_2000_SCIE_P15 <- sapply(split(SCIE, SCIE$cnt),
							function(x) apply(x[,3:7],2,
											  function(z) wtd.quantile(z, q = 0.15, weight = x$w_fstuwt, na.rm=TRUE)
							)
)


PISA_2000_SCIE_P85 <- sapply(split(SCIE, SCIE$cnt),
							 function(x) apply(x[,3:7],2,
							 				  function(z) wtd.quantile(z, q = 0.85, weight = x$w_fstuwt, na.rm=TRUE)
							 )
)

PISA_2000_SCIE <- data.frame(ach = colMeans(PISA_2000_SCIE_ach),
							 gini = colMeans(PISA_2000_SCIE_gini),
							 icc = colMeans(PISA_2000_SCIE_ICC),
							 p5 = colMeans(PISA_2000_SCIE_P5),
							 p95 = colMeans(PISA_2000_SCIE_P95),
							 p5.p95 = colMeans(PISA_2000_SCIE_P95) - colMeans(PISA_2000_SCIE_P5),
							 p15 = colMeans(PISA_2000_SCIE_P15),
							 p85 = colMeans(PISA_2000_SCIE_P85),
							 p15.p85 = colMeans(PISA_2000_SCIE_P85) - colMeans(PISA_2000_SCIE_P15)
)

save(PISA_2000_SCIE,file = "~/Dropbox/Projects_Research/PISA_inequality/replication/data/PISA_2000_scie_v1.0.1.RData")
