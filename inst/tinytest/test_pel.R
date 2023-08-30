
## test of the pel distribution

## Test silent running and failing when expected
for(v in c(0.1,1,4)){
    expect_silent({
        x <- rpel(10,2,v,2)
        d <- dpel(x,2,v,2)
        p <- ppel(x,2,v,2)
        q <- qpel(p,2,v,2)
    })
}
expect_error( { x <- rpel(10,-1,1,2) } )
expect_error( { x <- dpel(0.4,-1,1,2) } )
expect_error( { x <- ppel(0.4,-1,1,2) } )
expect_error( { x <- rpel(0.4,-1,1,2) } )
expect_error( { x <- dpel(-0.2,1,1,2) } )
expect_error( { x <- ppel(-0.2,1,1,2) } )
expect_error( { x <- qpel(-0.2,1,1,2) } )
expect_error( { x <- qpel(2,1,1,2) } )

## reverse log transform
a <- 10; v <- 1.1; theta <- 3
set.seed(4522435)
x <- rpel(100,a,v,theta)
d <- dpel(x,a,v,theta)
dd <- dpel(x,a,v,theta,log=TRUE)
expect_equal(d,exp(dd))

## reversability of p & q - may fail on tails due to lamW
a <- 10; v <- 1.1; theta <- 3
set.seed(4522435)
z <- runif(100)
x <- qpel(z,a,v,theta)
zz <- ppel(x,a,v,theta)
expect_equal(z,zz)

x <- qpel(z,a,v,theta,lower.tail=F)
zz <- ppel(x,a,v,theta,lower.tail=F)
expect_equal(z,zz)

lz <- log(z)
x <- qpel(lz,a,v,theta,lower.tail=F,log.p=T)
lzz <- ppel(x,a,v,theta,lower.tail=F,log.p=T)
expect_equal(lz,lzz)

xx <- qpel(lz,a,v,theta,log.p=T)
lzz <- ppel(x,a,v,theta,log.p=T)
expect_equal(lz,lzz)










## ## visual plots for comparision with paper
## x <- seq(0,5,length=1000)

## #Y <- cbind( rep(1,1000), rep(2,1000), rep(3,1000), rep(4,1000))
## #x11(); matplot(x,Y,type="l")

## Y <- cbind( dpel(x,2,2,0.5), dpel(x,2,2,1), dpel(x,2,2,2), dpel(x,2,2,5) )
## x11(); matplot(x,Y,type="l")

## Y <- cbind( dpel(x,2,0.5,2), dpel(x,2,1.1,2), dpel(x,2,2,2), dpel(x,2,5,2) )
## x11(); matplot(x,Y,type="l")

## Y <- cbind( dpel(x,0.8,2,2), dpel(x,1,2,2), dpel(x,2,2,2), dpel(x,5,2,2) )
## x11(); matplot(x,Y,type="l")


## x <- seq(0,10,length=100)


## d <- 1e-10
## a <- 2; v <- 2; theta <- 5
## Y <- cbind(dpel(x+d,a,v,theta), (ppel(x+d+d,a,v,theta) - ppel(x,a,v,theta))/(2*d))
## x11(); matplot(x+d,Y,type="l")

## a <- 0.8; v <- 2; theta <- 2
## Y <- cbind(dpel(x,a,v,theta), (ppel(x+d+d,a,v,theta) - ppel(x,a,v,theta))/(2*d))
## x11(); matplot(x+d,Y,type="l",ylim=c(0,2))




## ## test gradient - v=1.1simulation study typr values
## rm(list=ls())
## source("./R/lindlay.R")
## source("./R/pel.R")
## set.seed(0)
## a <- 7
## v <- 6
## theta <- 2
## out <- matrix(NA,3,1000)
## for(ii in 1:1000){
##     x <- rpel(10,a,v,theta)
##     p0 <- list(a=a,v=v,theta=theta)
##     out[,ii] <- fitPEL(x,p0)$estimate
## }

    
## p0 <- c(1.342203e+01,2.204119e+06,3.912776e+00)
## gg <- gnpel(p0,x)
## dlt <- 1e-6
## ge <- rep(NA,3)
## for(ii in 1:3){
##     pp <- p0; pp[ii] <- pp[ii] + dlt
##     pn <- p0; pn[ii] <- pn[ii] - dlt
    
##     ge[ii] <- ( npel(pp,x) - npel(pn,x) ) / (2*dlt)
## }
## gg
## ge











## ## test gradient - v=1.1
## set.seed(0)
## a <- 10
## v <- 1.1
## theta <- 3
## x <- rpel(10000,a,v,theta)
## p0 <- c(a,v,theta)
## gg <- gnpel(p0,x)
## dlt <- 1e-16
## ge <- rep(NA,3)
## for(ii in 1:3){
##     pp <- p0; pp[ii] <- pp[ii] + dlt
##     pn <- p0; pn[ii] <- pn[ii] - dlt
    
##     ge[ii] <- ( npel(pp,x) - npel(pn,x) ) / (2*dlt)
## }

## gg
## ge


## ## gradient with v=1
## p0 <- c(a,1,theta)
## gg <- gnpel(p0,x)
## dlt <- 1e-10
## ge <- rep(NA,3)
## for(ii in 1:3){
##     pp <- p0; pp[ii] <- pp[ii] + dlt
##     pn <- p0; pn[ii] <- pn[ii] - dlt
    
##     ge[ii] <- ( npel(pp,x) - npel(pn,x) ) / (2*dlt)
## }
## gg
## ge


## ## test optimisation - matches fitdistr output
## p0 <- list(a=10,v=1,theta=3)
## res <- optim(p0,npel,method="L-BFGS-B",lower=c(1e-10,1e-10,1e-10),x=x,hessian=TRUE)
## n <- length(x)
## vc <- solve(res$hessian)
## sds <- sqrt(diag(vc))

## tst <- structure(list(estimate = res$par, sd = sds, vcov = vc, loglik = -res$value, 
##                n = n), class = "fitdistr")

## tmp <- MASS::fitdistr(x,dpel,start=p0,lower=c(1e-10,1e-10,1e-10))




## npel(p0,x)


## tmp <- optim(p0,npel,gnpel,method="L-BFGS-B",lower=c(1e-10,1e-10,1e-10),x=x,hessian=T)

## d <- 1e-10
## g <- rep(NA,3)
## for(ii in 1:3){
##     pp <- p0; pp[ii] <- pp[ii] + d
##     pn <- p0; pn[ii] <- pn[ii] - d
    
##     g[ii] <- ( negllkl(pp,x) - negllkl(pn,x) ) / (2*d)
## }


## ## some simple tests to make sure it runs....
## rm(list=ls())
## source("./R/lindlay.R")
## source("./R/pel.R")
## who <- read.csv("./inst/extdata/WHO-COVID-19-global-data.csv")
## who$Date_reported <- as.Date(who$Date_reported)
## for(ii in c("New_cases","Cumulative_cases","New_deaths","Cumulative_deaths")){ who[[ii]] <- as.numeric(who[[ii]]) }

## idx <- ( who$Date_reported >= as.Date("2022-03-07") ) & ( who$Date_reported <= as.Date("2022-03-31") ) & (who$Country=="China")
## dss1 <- 100* who$New_deaths[idx] / who$New_cases[idx]
## tmp <- fitPEL(dss1)

## ds1 <- c(1.09, 1.00, 1.08, 1.12, 1.50, 1.60, 1.77, 1.81, 2.07, 1.75, 2.58,
##          2.59, 2.65, 3.09, 3.20, 3.47, 3.21, 2.77, 3.17, 2.65, 3.00, 3.61,
##          3.08, 2.70, 2.41)
## fit1 <- fitPEL(ds1)

## npel(c(5.615004,19.384176,1.684938),dss1)

## ds2 <- c(1.067, 1.757, 1.705, 1.849, 1.131, 1.595, 1.524, 1.014, 1.266,
##          1.678, 1.758, 1.898, 1.459, 3.370, 1.283, 1.753, 1.840, 2.134,
##          2.293, 2.217, 2.365, 1.485, 2.603, 2.952, 2.164, 3.142, 4.880,
##          2.885, 1.513, 2.704, 3.169, 2.485, 6.080, 2.462, 1.508, 1.078,
##          3.777, 3.407, 2.363, 5.893, 3.421, 7.211, 2.001, 2.087, 3.487,
##          3.457, 4.925, 2.469, 10.48, 2.514, 2.779, 2.514, 2.285, 3.895)
## #fit2 <- fitPEL(ds2)
## ds3 <- c(174, 161, 132, 136, 131, 144, 148, 140, 127, 122, 108, 104,
##          132, 111, 112, 120, 71, 90, 95, 123, 113, 131, 114, 85, 82,
##          112, 126, 176, 146, 160, 141, 137)
## #fit3 <- fitPEL(ds3)
