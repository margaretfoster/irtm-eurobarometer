## This script to run unconstrained
## Eurobarometer IRT, Three dimensions
## (eg: No M-Matrix, No "Fake" Points)

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

source("./IRTM-local/R/anchors.R")

## Data:

dataPath <- "~/Dropbox/Interventions/Eurobarometer94.3/"

Y <- readRDS(paste0(dataPath,
    "EB_Y.rds"))

## Finalize Processing
dim(Y) ### 38718 x 355 

Y <- as.matrix(Y)

d <- 3
nsamp= 10^4
nburn=2000

irt.unconstrained<-M_constrained_irt(Y=Y,
                       d=d,
                       nburn=nburn,
                       nsamp=nsamp,
                       thin=1,
                       learn_Omega=TRUE)

dim(irt.unconstrained$theta) ## 38718 x 6 x samp

## save(irt.unconstrained,
##      file="irt.unconstrained_eurobarometer943D3.RData")


##%%%%%%%%%%%%%%%%
## Analyize Thetas:
##%%%%%%%%%%%%%%%%

## Load from checkpoint:
## Take point averages

avgthetas <- apply(irt.unconstrained$theta, c(1,2), mean)
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
colnames(thetas)[1:3] <- paste0("Theta", 1:3)

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

basedata <- melt(thetas[,1:3])

head(basedata)

colnames(basedata) <- c("Theta", "Value")
basedata$Theta <- as.factor(basedata$Theta)


library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(dplyr)
library(ggrepel)

gg <- ggplot(basedata,
             aes(x=Value,
                 y=Theta,
                 fill=Theta))+
    geom_density_ridges() +
    scale_y_discrete(limits = rev(levels(basedata$Theta))) +
    ggtitle('Posterior Distribution of Group Mean',
            subtitle= "Thetas and Outcome, Unconstrained IRT") +
    scale_fill_brewer(palette='Spectral') +
    theme_bw()
gg

ggsave(file="ebirtmunconstrainedd3.pdf")

### Cluster by media trust patterns:

dat2 <- melt(thetas[,c(1:3, 14)])

colnames(dat2)[2] <- "Theta"
head(dat2)

head(dat2)

gg2 <- ggplot(dat2,
             aes(x=value,
                 y=mediatrust,
                 fill=mediatrust))+
    geom_density_ridges(alpha=.5) +
    ggtitle('Posterior Distribution of Group Mean',
            subtitle= "Thetas and Outcome") +
    labs(y="Levels of Media Trust",
         x= "Theta Posterior Estimates, Unconstrained IRT Model")+
    scale_y_discrete(limits=rev(levels(dat2$mediatrust)))+
    facet_wrap(~Theta, ncol=2)+
    scale_fill_brewer(palette='Spectral') +
    theme_bw()

gg2

ggsave(gg2,
       file="ThetaEstByMediaTrustunconstrainedd3.pdf")
