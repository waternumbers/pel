## Reimplimentation of PEL distribution R style

## Code bit to finish
## - check input lengths
## - check consistency with say *gamma wrt parameters and values out of range
## - add ML optimistaion and gradient terms
##

dpel <- function(x,a,q,v,log=FALSE){
    stopifnot("x should be > 0" = all(x>0),
              "a should be > 0" = all(a>0),
              "q should be > 0" = all(q>0),
              "v should be > 0" = all(v>0))
    
    ## TODO - get or check they are all the same length....
    
    Gx <- 1 - (exp(-q*x)*(1+q+q*x)/(1+q)) ## cdf
    lgx <- 2*log(q) - log(1+q) + log(1+x) - q*x ## log pdf
    tmp <- ifelse(v==1,0,log( log(v)/(v-1) ))
    L <- log(a) + (a-1)*log(Gx) + lgx + log(v)*(Gx^a) + tmp
    
    if(log) return(L)
    else return(exp(L))
}

ppel <- function(x,a,q,v, lower.tail = TRUE, log.p = FALSE){
    stopifnot("x should be >= 0" = all(x>0),
              "a should be >= 0" = all(a>0),
              "q should be >= 0" = all(q>0),
              "v should be >= 0" = all(v>0))
    
    ## TODO - get or check they are all the same length....
    
    Gx <- 1 - (exp(-q*x)*(1+q+q*x)/(1+q))
    pp <- Gx^a
    idx <- v!=1
    pp[idx] <-  ( v[idx]^( pp[idx] ) -1 )/(v[idx]-1)

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
              "v should be > 0" = all(v>0))

    ## work out the value of Gx
    Gx <- rep(NA,length(u))
    idx <- v!=1
    
    Gx[!idx] <- u[!idx]^(1/a[!idx])
    Gx[idx] <- ( log(1-u[idx]+u[idx]*v[idx]) / log(v[idx]) )^(1/a[idx])

    qq <- -1 - (1/q) - (1/q)*lamW::lambertWm1( (1+q)*(Gx-1)*exp(-1-q) )
    
    if( !lower.tail ){ qq <- 1-qq }
    return( qq )
}
    
rpel <- function(n,a,q,v){
    n <- floor(n)
    stopifnot("n should be > 0" = n>0,
              "a should be > 0" = all(a>0),
              "q should be > 0" = all(q>0),
              "v should be > 0" = all(v>0))
    ## interesting that q must be >0g 
    ## TODO - get or check they are all the same length....

    return( qpel(runif(n),a,q,v) )
}


