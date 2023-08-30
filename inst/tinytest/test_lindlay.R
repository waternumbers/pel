## some simple tests on the lindley distribution

x <- seq(0,10,length=1000)

theta <- 5

d <- dlind(x,theta)
dd <- dlind(x,theta,log=TRUE)
expect_equal(d,exp(dd))

## ddd <- VGAM::dlind(x,theta)
## range(d-ddd)

## range(plind(x,theta) - VGAM::plind(x,theta))

## ## inversion
## set.seed(0)
## use.n <- 10000
## z <- runif(use.n)
## theta <- 5
## ## taken from vgam
## x <- ifelse(runif(use.n) < rep_len(1 / (1 + 1/theta), use.n),
##             rexp(use.n, theta),
##             rgamma(use.n, shape = 2, scale = 1 / theta))
## zz <- qlind(x,theta)

## plot(sort(x),qlind((1:use.n)/(use.n+1),theta))





## reversability
px <- plind(x,theta)
xx <- qlind(px,theta)
expect_equal(x,xx)
## this isn't great exp in the upper tail - is it down to lamW function, seems more significant on tails??

px <- plind(x,theta,lower.tail=F)
xx <- qlind(px,theta,lower.tail=F)
expect_equal(x,xx)

px <- plind(x,theta,lower.tail=F, log.p=T)
xx <- qlind(px,theta,lower.tail=F,log.p=T)
expect_equal(x,xx)

px <- plind(x,theta,log.p=T)
xx <- qlind(px,theta,log.p=T)
e <- (xx-x)
range(e) ## this isn't great exp in the upper tail - is it down to lamW function, seems more significant on tails??


## ## test gradient of G(x)
## dtheta <- 1e-10

## gg <- gplind(x,theta)
## ge <- (plind(x,theta+dtheta) - plind(x,theta-dtheta))/ (2*dtheta)

## matplot(plind(x,theta),cbind(gg,ge),type="l")
## range(gg-ge)


## ## test gradient of log(g(x))
## dtheta <- 1e-10

## gg <- gldlind(x,theta)
## ge <- (dlind(x,theta+dtheta,log=T) - dlind(x,theta-dtheta,log=T))/ (2*dtheta)

## matplot(plind(x,theta),cbind(gg,ge),type="l")
## range(gg-ge)


## ## test density
## dx <- 1e-10
## x <- seq(1e-10,10,length(1000))
## gg <- dlind(x,theta)
## ge <- (plind(x+dx,theta) - plind(x-dx,theta))/ (2*dx)

## matplot(x,cbind(gg,ge),type="l")
## range(gg-ge)
