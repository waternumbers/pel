
expect_equal( 2,2)

## some simple tests to make sure it runs....
#rm(list=ls())
#source("pel.R")

x <- seq(0.1,10,length=100)

a <- 10
q <- 3
v <- 1.1

dpel(x,a,q,v)


## reversability
px <- ppel(x,a,q,v)
xx <- qpel(px,a,q,v)

e <- (xx-x)
range(e) ## this isn't great exp in the upper tail - is it down to lamW function, seems more significant on tails??


rpel(100,a,q,v)

## ## tests for the capa class objects
## ## these tests are based on matching the output of anomaly v4.0.2

## ## NOTES:
## ## 1. expect_equivalent used for testing collective anomalies since row.names differ
## ## 2. multivariate robust mean checks cancelled out since take to long to run - TODO fix this

## data(simulated)

## ## Adapt data to contain a clear point anomaly
## X <- sim.data
## X[100,1] <- 45

## ## adapt X to match the robust scaling applied in v4.0.2
## X <- apply(X,2,function(x){ (x-median(x))/mad(x) })

## ## read in the results from v4.0.2
## out <- readRDS("capa_results402_v2.rds") ## TODO need changing to remove path

## fc <- function(tmp,type){
##     out <- collective_change(tmp)
##     out <- out[out$segment==type,c("start","end")]
##     return(out) 
## }

## fp <- function(tmp,type){
##     out <- point_change(tmp)
##     out <- out[out$segment==type,"index"]
##     return(out)
## }

## ## #################################
## ## univariate tests
## x <- X[,1]
## mu <- rep(0,length(x))
## sigma <- rep(1,length(x))
## beta <- 4*log(length(x))
## betaP <- 3*log(length(x))


## expect_silent({ res_op <- capa_op(x,mu,sigma,gaussMeanVar,beta,betaP,min_length=2) })
## expect_silent({ res <- capa(x,mu,sigma,gaussMeanVar,beta,betaP,min_length=2) })
## expect_equal( collective_change(res), collective_change(res_op))
## expect_equal( point_change(res), point_change(res_op))
## expect_equal( fp(res,"gaussPoint"), out$single_meanvar$point$location )
## expect_equivalent( fc(res,"gaussMeanVar"), out$single_meanvar$collective[,c("start","end")] )
## ## expect_silent({ summary(res) })
## ## expect_silent({ show(res) })
## ## expect_silent({ plot(res,variate_names=TRUE) })

## expect_silent({ res_op<-capa_op(x,mu,sigma,gaussMean,beta,betaP,min_length=2) })
## expect_silent({ res <- capa(x,mu,sigma,gaussMean,beta,betaP,min_length=2) })
## expect_equal( collective_change(res), collective_change(res_op))
## expect_equal( point_change(res), point_change(res_op))
## expect_equal( fp(res,"gaussPoint"), out$single_mean$point$location )
## expect_equivalent( fc(res,"gaussMean"), out$single_mean$collective[,c("start","end")] )
## ## expect_silent({ summary(res) })
## ## expect_silent({ show(res) })
## ## expect_silent({ plot(res,variate_names=TRUE) })



## ##expect_silent({ res<-capa(X[,1,drop=F],type="robustmean",min_seg_len=2) })
## ## expect_equal( point_anomalies(res), out$single_robustmean$point ) ## fails in v4.0.2
## ##expect_equivalent( collective_anomalies(res), out$single_robustmean$collective )
## ## expect_silent({ summary(res) }) ## fails in v4.0.2
## ## expect_silent({ show(res) }) ## fails in v4.0.2
## ## expect_silent({ plot(res,variate_names=TRUE) }) ## fails in v4.0.2


