rm(list=ls())

loadPkg=function(toLoad){
 	for(lib in toLoad){
            if(!(lib %in% installed.packages()[,1])){ 
                install.packages(lib,
                                 dependencies=TRUE,
                                 repos='http://cran.rstudio.com/') }
            suppressMessages( library(lib, character.only=TRUE) )
 	}
    }

packs <- c("MCMCpack", "tmvtnorm", "truncnorm")

packs2 <- c("gmm", "devtools", "reshape2",
            "dplyr")

loadPkg(c(packs, packs2))


library(devtools)

install.packages('IRTM',
                 repos=NULL,
                 type='source')

library(IRTM)


source("anchors.R")

## Load Data:

dataPath <- "~/Dropbox/Interventions/Eurobarometer94.3/"

M <- readRDS(paste0(dataPath,
                    "EB_M.rds"))

Y <- readRDS(paste0(dataPath,
    "EB_Y.rds"))


dim(Y) ### 38718 x 355
dim(M) ## 6x6 x355

## Finalize processing:

l2<-pair_gen_anchors(M,5)
l3 <- anchors(l2, Y) ## NB: Anchors creates Yall with the anchor points first

d_which_fix<-1:nrow(l3$Yfake)
d_theta_fix<-l2$theta_fake

## Run in command line:
Sys.setenv('R_MAX_VSIZE'=32000000000)

load("irtm_eurobarometer943.RData")

class(irt)
length(irt) ## 5
names(irt) ## theta, lambda, "b", "Sigma", "Omega"


## Analyze theta:
## create point averages and remove anchor points:

## 15k ok for vector mem, 20l not
avgthetas1 <- apply(irt$theta[1:15000, , ], c(1,2), mean)
avgthetas2 <- apply(irt$theta[15001:30000, , ], c(1,2), mean)
avgtheats3 <- apply(irt$theta[30001:38838, , ], c(1,2), mean)

avgthetas <- rbind(avgthetas1,
                   avgthetas2,
                   avgtheats3)

dim(avgthetas)

## Removing anchor points:

end.of.anchors <- dim(l2$Yfake)[1]+1

avgthetas <- avgthetas[end.of.anchors:dim(avgthetas)[1],]

dim(avgthetas)

## Bring in some demographic DVs
## Country and media trust:

idvs <- read.csv(paste0(dataPath,
                        "ebdatDemographVaribs.csv"))

colnames(idvs)

dim(idvs)
dim(avgthetas)

thetas <- cbind(avgthetas, idvs)

dim(thetas)
head(thetas)

## rename the Theta estimates:
colnames(thetas)[1:6] <- paste0("Theta", 1:6)

## Make factors where important:
thetas$mediatrust <- as.factor(thetas$mediatrust)
thetas$class <- as.factor(thetas$class)
thetas$polorient <- as.factor(thetas$polorient)
thetas$isocntry <- as.factor(thetas$isocntry)

colnames(thetas)[colnames(thetas)=="qb7_2"] <- "MoreBorderControl"

dim(thetas)
head(thetas)

## Plotting
## First: Thetas only:

basedata <- melt(thetas[,1:6])

head(basedata)

colnames(basedata) <- c("Theta", "Value")
basedata$Theta <- as.factor(basedata$Theta)

save(basedata,thetas,  ## This saves the averaged thetas + IDV data
     file="EuroB_IRTM_toplot.RData")

