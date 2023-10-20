

## Script to format and run EB Y and M through IRTM

## pulling out specific scripts from the package:
sourcePath <- "~/Research/Interventions/Theory-IRT/IRTM/R/"

source(paste0(sourcePath,
              "pair_gen_anchors.R"))

source(paste0(sourcePath,
              "anchors.R"))

source(paste0(sourcePath,
              "constrained_IRT.R"))

    ## Formatted Y and M:

M <- readRDS("EB_M.rds")

Y <- readRDS("EB_Y.rds")


dim(Y) ### 38718
dim(M) ## 6x6284


l2<-pair_gen_anchors(M,5)
l3 <- anchors(l2, Y)


d_which_fix<-1:nrow(l3$Yfake)
d_theta_fix<-l2$theta_fake

d <- 6

irt<-M_constrained_irt(l3$Yall,
                       d,
                       abs(M)*2,
                       theta_fix = d_theta_fix,
                       which_fix = d_which_fix,
                       nburn=100,
                       nsamp=100,
                       thin=1,
                       learn_Omega=TRUE)
