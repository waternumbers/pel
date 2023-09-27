## some simple tests on the lindley distribution

## test silent running
expect_silent({
    theta <- 4
    x <- rlind(10,theta)
    d <- dlind(x,theta)
    p <- plind(x,theta)
    q <- qlind(p,theta)
})

expect_error( x <- rlind(10,-1) )
expect_error( x <- dlind(10,-1) )
expect_error( x <- plind(10,-1) )
expect_error( x <- qlind(0.7,-1) )

expect_error( x <- rlind(-10,1) )
expect_error( x <- dlind(-10,1) )
expect_error( x <- plind(-10,1) )
expect_error( x <- qlind(-0.7,1) )
expect_error( x <- qlind(7,1) )

## generate data to test against
theta <- 5
z <- seq(0,1,length=100) ## cdf values
x <- qlind(z,theta) ## evaluation of r.v. at those values

## log density
expect_equal( dlind(x,theta), exp(dlind(x,theta,log=TRUE)) )

## reversability of ppel & qpel
expect_equal( z, plind(x,theta) )
expect_equal( 1-z, plind(x,theta,lower.tail=F) )

expect_equal( x, qlind(log(z),theta,log.p=T) )
expect_equal( log(z), plind(x,theta,log.p=T) )

expect_equal( x, qlind(log(1-z),theta,log.p=T,lower.tail=F) )
expect_equal( log(1-z), plind(x,theta,log.p=T,lower.tail=F) )






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
