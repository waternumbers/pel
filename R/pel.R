## Reimplimentation of PEL distribution R style

## Code bit to finish
## - check input lengths
## - check consistency with say *gamma wrt parameters and values out of range
## - add ML optimistaion and gradient terms
##


## Issues/Not understood
## - negative branch of lambert W function is?? Which implimentation have they used?? Current code uses lamW package
## - what happens as v --> 1. At the moment set to fail in code to match paper. This will make ML optimisation interesting - not addressed in paper
## - what happens to CDF and pdf as x-->0
## - Paper indicates ML estimation done in R by setting gradients to 0 - suspect given Newton-Raphlson step mentioned that actually done using one of the base R optimiser to minimising log likelihood using gradients
## - log-likelihood given in paper can;t be evaluated for v<1
## - lim v->1 log(v)/(v-1) =1
## - 

dpel <- function(x,a,q,v,log=FALSE){
    stopifnot("x should be > 0" = all(x>0),
              "a should be > 0" = all(a>0),
              "q should be > 0" = all(q>0),
              "v should be > 0" = all(v>0),
              "v should be not equal 1" = all(v!=1))

    ## TODO - get or check they are all the same length....
    
    xx <- 1 - (exp(-q*x)*(1+q+q*x)/(1+q))
    L <- ( a*(v^(xx^a))*log(v)*(xx^(a-1))*(q^2)*(1+x)*exp(-q*x) )/ ((v-1)*(q+1))
    ## L <- log(a) + log(log(v)) - log(v-1) - log(q+1) + log(q^2) -
    ##     q*x + log(1+x) + log(v)*(xx^a) + (a-1)*log(xx)
    if(log) return(log(L))
    else return(L)
}

ppel <- function(x,a,q,v, lower.tail = TRUE, log.p = FALSE){
    stopifnot("x should be >= 0" = all(x>0),
              "a should be >= 0" = all(a>0),
              "q should be >= 0" = all(q>0),
              "v should be >= 0" = all(v>0),
              "v should be not equal 1" = all(v!=1))
    
    ## TODO - get or check they are all the same length....
    
    xx <- 1 - (exp(-q*x)*(1+q+q*x)/(1+q))
    pp <-  ( v^( xx^a ) -1 )/(v-1)
    if( !lower.tail ){ pp <- 1-pp }
    if( log.p ){ pp <- log(pp) }
    return( pp )
}

qpel <- function(u,a,q,v, lower.tail = TRUE, log.p = FALSE){
    if(log.p){ u <- exp(u) }
    stopifnot("u should be >= 0" = all(u>=0),
              "u should be =< 1" = all(u<=1),
              "a should be > 0" = all(a>0),
              "q should be > 0" = all(q>0),
              "v should be > 0" = all(v>0),
              "v should be not equal 1" = all(v!=1))
    ## interesting that q must be >0g 
    ## TODO - get or check they are all the same length....

    yy <- -exp(-(1+q))*(1-(log(u*(v-1) +1)/log(v))^(1/a))*(1+q)
    qq <- -1 - (1/q) - (1/q)*lamW::lambertWm1(yy)
    if( !lower.tail ){ qq <- 1-qq }
    return( qq )
}
    
rpel <- function(n,a,q,v){
    n <- floor(n)
    stopifnot("n should be > 0" = n>0,
              "a should be > 0" = all(a>0),
              "q should be > 0" = all(q>0),
              "v should be > 0" = all(v>0),
              "v should be not equal 1" = all(v!=1))
    ## interesting that q must be >0g 
    ## TODO - get or check they are all the same length....

    return( qpel(runif(n),a,q,v) )
}
