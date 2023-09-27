
## some simple tests to make sure it runs....
rm(list=ls())
devtools::load_all()

set.seed(231987)
pgen <- list(c(a=7,v=6,theta=2),
             c(a=5,v=5,theta=3),
             c(a=5,v=4,theta=2),
             c(a=5,v=6,theta=2),
             c(a=4,v=5,theta=2),
             c(a=5,v=5,theta=2),
             c(a=5,v=5,theta=1))

sim_data <- list()
for(ii in 1:length(pgen)){
    x <- pgen[[ii]];
    sim_data[[paste0("set_",ii)]] <- list(par0=x,X=matrix( rpel(100*1000,a=x["a"],v=x["v"],theta=x["theta"]), ncol=1000 ))
}
save(sim_data,file="./data/sim_data.rda")
tools::resaveRdaFiles("./data/sim_data.rda")

##op <- options("warn"=2, error = function() traceback(3))
rec <- list()
for(nm in names(sim_data)){
    print(nm)
    X <- sim_data[[nm]]$X
    p0 <- sim_data[[nm]]$par0
    out <- list(E = matrix(NA,3,ncol(X)),
                llkl = rep(NA,ncol(X)))
    out <- setNames(rep(list(out),4),paste(c(10,40,70,100)))
    for(tt in c(10,40,70,100)){
        print(tt)
        for(ii in 1:ncol(X)){
            ft <- fitPEL(X[1:tt,ii],start=p0)
            out[[paste(tt)]]$E[,ii] <- ft$estimate
            out[[paste(tt)]]$llkl[ii] <- ft$loglik
        }
    }
    rec[[nm]] <- out
}
save(rec,file="./data/sim_fits.rda")
tools::resaveRdaFiles("./data/sim_fits.rda")
        


## system.time({
##     tmp <- sim_data[[1]]
##     E <- matrix(NA,3,ncol(tmp$X))
##     llkl <- rep(NA,ncol(tmp$X))
##     for(ii in 1:ncol(tmp$X)){
##         ft <- fitPEL(tmp$X[,ii],start=tmp$par0)
##         E[,ii] <- ft$estimate
##         llkl[ii] <- ft$loglik
##     }
## })

## ii <- which.max(E[2,])
## x <- tmp$X[,ii]
## p <- ppel(x,tmp$par0[1],tmp$par0[2],tmp$par0[3])
## plot((1:100)/100,sort(p))
## abline(0,1)

## x <- tmp$X[,ii]
## p0 <- E[,ii] #tmp$par0
## dk <- gnpel(p0,x)
## g <- rep(NA,3); delta <- 1e-6
## for(ii in 1:3){
##     pu <- pl <- p0
##     pu[ii] <- pu[ii]+delta
##     pl[ii] <- pl[ii]-delta
##     g[ii] <- (npel(pu,x)-npel(pl,x))/(2*delta)
## }

