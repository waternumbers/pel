
## some simple tests to make sure it runs....
rm(list=ls())
source("./R/lindlay.R")
source("./R/pel.R")

pgen <- list(c(a=7,v=6,theta=2),
             c(a=5,v=5,theta=3),
             c(a=5,v=4,theta=2),
             c(a=5,v=6,theta=2),
             c(a=4,v=5,theta=2),
             c(a=5,v=5,theta=2),
             c(a=5,v=5,theta=1))

fgen <- function(x){
    list(param=x,
         n10 = matrix( rpel(10*1000,a=x["a"],v=x["v"],theta=x["theta"]), ncol=1000 ),
         n40 = matrix( rpel(40*1000,a=x["a"],v=x["v"],theta=x["theta"]), ncol=1000 ),
         n70 = matrix( rpel(70*1000,a=x["a"],v=x["v"],theta=x["theta"]), ncol=1000 ),
         n100 = matrix( rpel(100*1000,a=x["a"],v=x["v"],theta=x["theta"]), ncol=1000 ))
}

X <- list()
for(ii in 1:length(pgen)){ X[[ii]] <- fgen(pgen[[ii]]) }

Y <- list()
for(ii in 1){ ##:length(X)){
    p0 <- as.list(X[[ii]]$param)
    out <- matrix(NA,3,1000)
    for(jj in 1:1000){
        out[,jj] <- fitPEL(X[[ii]]$n10[,jj],start=p0)$estimate

    }
    
    ## Y[[ii]] <- lapply(X[[ii]],
##                       function(x){
##                           if(!is.matrix(x)){ return(x) }
##                           else{
                              
##                               return( apply(x,2,function(z){fitPEL(z,start=p0)$estimate}) )
##                           }
##                       })
}
    


    out$param <- X[[ii]]$param
    for(jj in c("n10","n4
    X[[ii]] <- fgen(pgen[[ii]]) }
