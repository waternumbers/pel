## Reimplimentation of PEL distribution

## density
dpel <- function(x,a,v,theta,log=FALSE){
    stopifnot("a should be > 0" = all(a>0),
              "v should be > 0" = all(v>0))
    
    ##browser()
    Gx <- plind(x,theta) ## cdf
    gx <- dlind(x,theta) ## pdf
    tmp <- ifelse(v==1,1,log(v)/(v-1))
    L <- a*(v^(Gx^a))*(Gx^(a-1))*gx*tmp
    L[Gx==0] <- 0 ## handles the fact that 0^0=1 in R
    if(log[1]) return(log(L))
    else return(L)
    ## Gx <- plind(x,theta) ## cdf
    ## lgx <- dlind(x,theta,log=T) ## log pdf
    ## tmp <- ifelse(v==1,0,log( log(v)/(v-1) ))
    ## L <- log(a) + (a-1)*log(Gx) + lgx + log(v)*(Gx^a) + tmp
    ## ##L[x==0] <- -Inf
    ## if(log[1]) return(L)
    ## else return(exp(L))
}

## distribution function
ppel <- function(x,a,v,theta,lower.tail = TRUE, log.p = FALSE){
    stopifnot("a should be >= 0" = all(a>0),
              "v should be >= 0" = all(v>0))
    
    ## TODO - get or check they are all the same length....
    Gx <- plind(x,theta)
    pp <- Gx^a
    idx <- v!=1
    pp[idx] <-  ( ( v[idx]^pp[idx] ) -1 )/(v[idx]-1)

    pp[x==Inf] <- 1
    
    if( !lower.tail[1] ){ pp <- 1-pp }
    if( log.p[1] ){ pp <- log(pp) }
    return( pp )
}

## quantile function
qpel <- function(q,a,v,theta, lower.tail = TRUE, log.p = FALSE){
    if(log.p[1]){ q <- exp(q) }
    if( !lower.tail[1] ){ q <- 1-q }
    
    stopifnot("q should be >= 0" = all(q>=0),
              "q should be =< 1" = all(q<=1),
              "a should be > 0" = all(a>0),
              "v should be > 0" = all(v>0))

    ## work out the value of Gx
    Gx <- rep(NA,length(q))
    idx <- v!=1
    
    Gx[!idx] <- q[!idx]^(1/a[!idx])
    Gx[idx] <- ( log(1-q[idx]+q[idx]*v[idx]) / log(v[idx]) )^(1/a[idx])

    qq <- qlind(Gx,theta)
    
    return( qq )
}

## random sample generation
rpel <- function(n,a,v,theta){
    n <- floor(n)
    stopifnot("n should be > 0" = n>0)
    ## interesting that q must be >0g 

    return( qpel(runif(n),a,v,theta) )
}

## negative log likelihood
## private function for use in fitting
npel <- function(param,x){
    out <- min( sum( -dpel(x,param[1],param[2],param[3],log=TRUE) ) , .Machine$double.xmax^0.5 )
    return(out)
}

## gradient of negative log likelihood
## private function for use in fitting
gnpel <- function(param,x){
    a <- param[1]
    v <- param[2]
    theta <- param[3]
    stopifnot("a should be > 0" = all(a>0),
              "v should be > 0" = all(v>0))
    ## values from the lind distribution
    Gx <- plind(x,theta) ## cdf
    dGx <- gplind(x,theta) ## gradient of cdf wrt theta
    lgx <- dlind(x,theta,log=T) ## log pdf
    dlgx <- gldlind(x,theta) ## gradient of log of pdf
    n <- length(x)
   
    ## gradient of a
    da <- (n/a) + sum(log(Gx)*(1+log(v)*(Gx^a)))
    ## gradient of v
    if(v==1){
        dv <- sum( Gx^a ) - (n/2)
    }else{
        dv <- ( sum( Gx^a ) + n*((1/log(v)) - (v/(v-1))) ) / v
    }
    ## gradient wrt theta
    tmp <- (log(v)*a*(Gx^a) + a -1 ) * (dGx/Gx)
    tmp[dGx==0] <- 0
    dtheta <- sum(tmp) + sum( dlgx )

    g <- -c(da,dv,dtheta)
    return( g )
}

## function for fitting to a random sample
## returns a fitdistr object
fitPEL <- function(x,start=list(a=1,v=1,theta=1),lower=c(1e-10,1e-10,1e-5),...){
    if (missing(x) || length(x) == 0L || mode(x) != "numeric") 
        stop("'x' must be a non-empty numeric vector")
    if (any(!is.finite(x))) 
        stop("'x' contains missing or infinite values")
    n <- length(x)

    ## optimise for v<1 & v>1
    res <- optim(start,npel,gnpel,method="L-BFGS-B",lower=lower,upper=c(Inf,1,Inf),x=x,...)
    ## optimise for v>1
    lower[2] <- 1
    uv <- optim(start,npel,gnpel,method="L-BFGS-B",lower=lower,x=x,...)
    if(uv$value < res$value){ res <- uv }

    ## work out Hessian
    ## a bit awkward since optim will put in values outside of the lower and upper ranges
    vc <- sds <- NULL
    tryCatch({
        vc <- solve( optimHess(res$par, npel, gnpel, x=x,...) )
        sds <- sqrt(diag(vc))
    },
    warning = function(w){warning(paste("Unable to compute Variance:",w$message))},
    error = function(e){warning(paste("Unable to compute Variance:",e$message))})
    
    structure(list(estimate = res$par, sd = sds, vcov = vc, loglik = -res$value, 
                   n = n), class = "fitdistr")
}


## ## negative log likelihood with optimal v value
## ## private function for use in fitting
## subv <- function(param,x){
##     a <- param[1]
##     theta <- param[2]
    
##     G <- plind(x,theta)
##     lv <- -sum(log(G)+(1/a)) / sum( (G^a)*log(G) )
##     ##print(exp(lv))
##     ##print( param )
##     ##out <- sum( -dpel(x,param[1],param[2],param[3],log=TRUE) )
##     ##print(out)
##     out <- min( sum( -dpel(x,param[1],exp(lv),param[2],log=TRUE) ) , .Machine$double.xmax^0.5 )
##     ##print(out)
##     return(c(lv,out))
## }


