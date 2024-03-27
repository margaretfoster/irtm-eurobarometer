rm(list=ls())
setwd("~/Dropbox/Interventions/Eurobarometer94.3/Vignette/")

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
            "dplyr", "tidyverse")

loadPkg(c(packs, packs2))

library(IRTM)


## If need a fresh IRTM package install:
#setwd("~/Dropbox/Research/Interventions/Theory-IRT/") ## WD for package install

#install.packages('IRTM',
#                 repos=NULL,
#                 type='source')

##source("./IRTM-local/R/anchors.R")## Path to the local installation

## Switch back to the data and analysis scripts
## Online repository:
## https://doi.org/10.7910/DVN/FH74D9

dataPath <- "./Data/"

M <- readRDS(paste0(dataPath,
                    "EB_M.rds"))

Y <- readRDS(paste0(dataPath,
                    "EB_Y.rds"))

## Finalize processing:

l2<-pair_gen_anchors(M,5)
l3 <- anchors(l2, Y) ## NB: Anchors creates Yall with the anchor points first

d_which_fix<-1:nrow(l3$Yfake)
d_theta_fix<-l2$theta_fake

## Note: the following configuration is a toy configuration
## It is designed for speed and illustrating the package
## For research purposes, you need to increase the number of iterations

d <- 6
nsamp= 20^3
nburn=20^1

## Quick dimension check, to save debugging time:

if(dim(Y)[2] == dim(M)[3]){
  print("length M = length Y check, passed!")
}else(
  print("Length Y not equal length M, check processing")
)

## Make sure M is a list of dxd matricies:

if(dim(M)[1] == d &&
   dim(M)[2]==d){
  print("M and d dimension checks, passed!")
}else(
  print("M not dxd, check processing")
)

## One we have all of the elements verified, we can 

irt <- M_constrained_irt(Y=l3$Yall,
                       d=d,
                       M=abs(M)*2,
                       theta_fix = d_theta_fix,
                       nburn=nburn,
                       nsamp=nsamp,
                       thin=1,
                       learn_Omega=TRUE)

names(irt) ## theta, lambda, "b", "Sigma", "Omega"

##save(irt,
##     file="irtm_eurobarometer943.Rds")

##%%%%%%%%%%%%%%%%%%%%%%%%
## Example analysis:
##%%%%%%%%%%%%%%%%%%%%%%%%
## Analyze thetas
## Generate point averages and remove anchor points:

avgthetas <- apply(irt$theta, c(1,2), mean)


## Removing anchor points:

end.of.anchors <- dim(l2$Yfake)[1]+1

avgthetas <- avgthetas[end.of.anchors:dim(avgthetas)[1],]

dim(avgthetas)

## Bring in some demographic DVs
## Country and media trust:

idvs <- read.csv(paste0(dataPath,
                        "ebdatDemographVaribs.csv"))

thetas <- cbind(avgthetas, idvs)

## Rename columns for readability:
colnames(thetas)[1:6] <- paste0("Theta", 1:6)
colnames(thetas)[colnames(thetas)=="qb7_2"] <- "MoreBorderControl"

## Cast into factors:
thetas$mediatrust <- as.factor(thetas$mediatrust)
thetas$class <- as.factor(thetas$class)
thetas$polorient <- as.factor(thetas$polorient)
thetas$isocntry <- as.factor(thetas$isocntry)

head(thetas)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Example visualizations:
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Plotting
## First: Plot Thetas

library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(dplyr)
library(ggrepel)

basedata <- melt(thetas[,1:6])

colnames(basedata) <- c("Theta", "Value")
basedata$Theta <- as.factor(basedata$Theta)


gg <- ggplot(basedata,
             aes(x=Value,
                 y=Theta,
                 fill=Theta))+
  geom_density_ridges() +
  scale_y_discrete(limits = rev(levels(basedata$Theta))) +
  ggtitle('Posterior Distribution of Group Mean',
          subtitle= "Thetas and Outcome") +
  scale_fill_brewer(palette='Spectral') +
  theme_bw()
gg

ggsave(gg,
       file="theta_dist.png")

## Example of plotting with accompanying metadata:
## Cluster by media trust patterns:

dat2 <- melt(thetas[,c(1:6, 17)])

colnames(dat2)[2] <- "Theta"

gg2 <- ggplot(dat2,
              aes(x=value,
                  y=mediatrust,
                  fill=mediatrust))+
  geom_density_ridges(alpha=.5) +
  ggtitle('Posterior Distribution of Group Mean',
          subtitle= "Thetas and Outcome") +
  labs(y="Levels of Media Trust",
       x= "Theta Posterior Estimates")+
  scale_y_discrete(limits=rev(levels(dat2$mediatrust)))+
  facet_wrap(~Theta, ncol=2)+
  scale_fill_brewer(palette='Spectral') +
  theme_bw()

gg2

ggsave(gg2,
       file="theta_media_trust.png")

####%%%%%%%%%%%%%%%%%%%%%%%
### Lambda Analysis
###%%%%%%%%%%%%%%%%%%%%%%%%%
 
dim(irt$lambda)
## questions x thetas x numsims/thinning

## Which theta do we want to analyze:
theta_id <-  5 ## 
## Key:
## 1: culture threat
## 2: immigration threat
## 3: economic threat
## 4: health threat
## 5:  support immigration
## 6: support EU
lambda_weight <- as.data.frame(irt$lambda[,theta_id,]) %>%
   rowwise() %>%
   mutate(mean = mean(c_across(starts_with("V")), na.rm = TRUE),
          variance = var(c_across(starts_with("V")), na.rm = TRUE)) %>%
  select(mean, variance) %>%
  ungroup() 

dim(lambda_weight)

## Connect to human-readable question
## codes:

MCodes <- read_csv(paste0(dataPath,
                          "Immigration_EB_MCodes.csv"))

## Only keep M-Codes with loadings or outcomes:
MCodes$encoding <- rowSums(abs(MCodes[,4:9])) 
MCodes <- MCodes[which(MCodes$encoding > 0),]

## Verify that the data objects have the same length:
dim(MCodes)[1] == dim(lambda_weight)[1]

t_five <- as.data.frame(cbind(question= as.character(MCodes$QMap), 
                             substantive = as.character(MCodes$SubstantiveNotes),
                              mean= round(as.numeric(lambda_weight$mean),3),
                              variance= round(as.numeric(lambda_weight$variance),3),
                              theta= theta_id)) %>%
  arrange(desc(mean))

head(t_five)


