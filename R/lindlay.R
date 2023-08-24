## Lindlay Distribution

## Code bit to finish
## - check input lengths
## - check consistency with say *gamma wrt parameters and values out of range
## - add ML optimistaion and gradient terms
##

dlind <- function(x,theta,log=FALSE){
    stopifnot("x should be >= 0" = all(x>=0),
              "theta should be > 0" = all(theta>0))
    lgx <- 2*log(theta) - log1p(theta) + log1p(x) - theta*x
    
    if(log) return(lgx)
    else return(exp(lgx))
}

plind <- function(x,theta, lower.tail = TRUE, log.p = FALSE){
    stopifnot("x should be >= 0" = all(x>=0),
              "theta should be >= 0" = all(theta>0))
    
    Gx <- 1 - (exp(-theta*x)*(1+theta+theta*x)/(1+theta))

    if( !lower.tail ){ Gx <- 1-Gx }
    if( log.p ){ Gx <- log(Gx) }
    return( Gx )
}

qlind <- function(q,theta, lower.tail = TRUE, log.p = FALSE){
    if(log.p){ q <- exp(q) }
    if( !lower.tail ){ q <- 1-q }
    stopifnot("q should be >= 0" = all(q>=0),
              "q should be =< 1" = all(q<=1),
              "theta should be > 0" = all(theta>0))

    ## work out the value of Gx
    x <- -1 - (1/theta) - (1/theta)*lamW::lambertWm1( (1+theta)*(q-1)*exp(-1-theta) )
    return( x )
}

rlind <- function(n,theta){
    n <- floor(n)
    stopifnot("n should be > 0" = n>0,
              "theta should be > 0" = all(theta>0))
    ## interesting that q must be >0g 
    ## TODO - get or check they are all the same length....

    return( qpel(runif(n),theta) )
}


## gradient of G(x) wrt theta in paper
gplind <- function(x,theta){
    stopifnot("x should be >= 0" = all(x>=0),
              "theta should be >= 0" = all(theta>0))
    
    ## TODO - get or check they are all the same length....
    ##dGx <- -exp(-theta*x)*( (1-theta*x-theta*(x^2))/(1+theta) - (1+theta+theta*x)/((1+theta)^2) )
    dGx <- x*exp(-theta*x)*( (1+theta+theta*x)/(1+theta) - 1/((1+theta)^2) )

    return(dGx)
}

## gradient of log(g(x)) wrt theta in paper
gldlind <- function(x,theta){
    stopifnot("x should be >= 0" = all(x>=0),
              "theta should be >= 0" = all(theta>0))

    dld <- (2/theta) - (1/(1+theta)) - x
    return(dld)
}
