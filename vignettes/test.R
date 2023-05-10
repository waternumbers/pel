### R code from vignette source 'test.Rnw'

###################################################
### code chunk number 1: setup
###################################################
# set global chunk options
#opts_chunk$set(fig.path='figure/minimal-', fig.align='center', fig.show='hold')
options(formatR.arrow=TRUE,width=90)


###################################################
### code chunk number 2: boring-random
###################################################
set.seed(1121)
(x=rnorm(20))
mean(x);var(x)


###################################################
### code chunk number 3: boring-plots
###################################################
## two plots side by side (option fig.show='hold')
par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3,las=1)
boxplot(x)
hist(x,main='')


