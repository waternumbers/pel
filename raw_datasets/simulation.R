
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
    sim_data[[paste0("set_",ii)]] <- matrix( rpel(100*1000,a=x["a"],v=x["v"],theta=x["theta"]), ncol=1000 )
}
save(sim_data,file="./data/sim_data.rda")
tools::resaveRdaFiles("./data/sim_data.rda")


Y <- list()
for(ii in 1){ ##:length(X)){
    p0 <- as.list(X[[ii]]$param)
    out <- matrix(NA,3,1000)
    for(jj in 1:1000){
        out[,jj] <- fitPEL(X[[ii]]$n10[,jj],start=p0)$estimate

    }
}
