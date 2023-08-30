## repackage the WHO csv file into a R data.frame and save in data
rm(list=ls())

who <- read.csv("WHO-COVID-19-global-data.csv")
save(who,file="../data/who.rda")
tools::resaveRdaFiles("../data/who.rda")
