
##%%%%%%%%%%%%%%%%
## Analyze Thetas:
##%%%%%%%%%%%%%%%%

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
packs2 <- c("gmm", "devtools", "reshape2",
            "dplyr")

loadPkg(c(packs, packs2))


load("irt.unconstrained_eurobarometer943.RData")

## Load from checkpoint
## Take point averages

avgthetas1 <- apply(irt.unconstrained$theta[1:15000,,],
                    c(1,2), mean)

avgthetas2 <- apply(irt.unconstrained$theta[15001:30000,,],
                    c(1,2), mean)

avgthetas3 <- apply(irt.unconstrained$theta[30001:38718,,],
                    c(1,2), mean)

avgthetas <- rbind(avgthetas1,
                   avgthetas2,
                   avgthetas3)

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

ggsave(file="ebirtmunconstrained.pdf")

### Cluster by media trust patterns:

dat2 <- melt(thetas[,c(1:6, 17)])

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
       file="ThetaEstByMediaTrustunconstrained.pdf")

## gg3: Thetas by political orientation

dat3 <-  melt(thetas[,c(1:6, 16)])

colnames(dat3)[2] <- "Theta"

head(dat3)

dat3$polorient <- factor(dat3$polorient,
                         levels=c("FarLeft",
                                  "Left",
                                  "Center",
                                  "Right",
                                  "FarRight",
                                  "Refused/DNt"))

gg3 <- ggplot(dat3,
              aes(x=value,
                  y=polorient,
                  fill=polorient))+
  geom_density_ridges(alpha=.5) +
  ggtitle('Posterior Distribution of Group Mean',
          subtitle= "Thetas and Outcome, Unconstrained IRT") +
  labs(y="Political Orientation",
       x= "Theta Posterior Estimates")+
  scale_y_discrete(limits=rev(levels(dat3$polorient)))+
  facet_wrap(~Theta, ncol=2)+
  scale_fill_brewer(palette='Spectral') +
  theme_bw()

gg3

ggsave(gg3,
       file="ThetaEstByPoliticalOrientationunconstrained.pdf")


## gg4: Thetas by social class

colnames(thetas)

dat4 <-  melt(thetas[,c(1:6, 11)])

head(dat4)

colnames(dat4)[2] <- "Theta"

dat4$class <- factor(dat4$class,
                     levels=c("Other/None/DN/Refused",
                              "WorkingClass",
                              "LowerMiddle",
                              "MiddleClass",
                              "UpperMiddleClass",
                              "UpperClass"))

gg5 <- ggplot(dat4,
              aes(x=value,
                  y=class,
                  fill=class))+
  geom_density_ridges(alpha=.5) +
  ggtitle('Posterior Distribution of Group Mean',
          subtitle= "Thetas and Outcome") +
  labs(y="Self-Reported Social Class",
       x= "Theta Posterior Estimates, Unconstrained IRT Model")+
  scale_y_discrete(limits=rev(levels(dat4$class)))+
  facet_wrap(~Theta, ncol=2)+
  scale_fill_brewer(palette='Spectral') +
  theme_bw()

gg5

ggsave(gg5,
       file="ThetaEstBySocialClassunconstrained.pdf")


## Pairwise statistically different distributions?

distCompare <- function(pairs, theta,var){
  pairs.df <- as.data.frame(pairs)
  pairs.df$statistic <- 0
  pairs.df$p.value <- 0
  pairs.df$whichtheta <- theta
  pairs.df$var <- var
  
  for(r in 1:dim(pairs)[1]){
    comps.test <- ks.test(
      x =thetas[thetas[,var]==pairs[r,1],
                theta],
      y=thetas[thetas[,var]==pairs[r,2],
               theta]
    )
    pairs.df[r, "statistic"] <- round(comps.test$statistic,3)
    pairs.df[r, "p.value"] <- round(comps.test$p.value,3)    
  }
  return(pairs.df)
}


## social class
pairs.class <- t(combn(x = levels(thetas$class),
                       m = 2))

comp.class.t1 <- distCompare(pairs.class, theta="Theta1",
                             var="class")

comp.class.t2 <- distCompare(pairs.class, theta="Theta2",
                             var="class")
comp.class.t3  <- distCompare(pairs.class,
                              theta="Theta3", var="class")


## media trust

pairs.media <- t(combn(x = levels(thetas$mediatrust),
                       m = 2))


table(thetas$mediatrust)

comp.media.t1 <- distCompare(pairs.media,
                             theta="Theta1",
                             var="mediatrust") 
comp.media.t2 <- distCompare(pairs.media,
                             theta="Theta2",
                             var="mediatrust")
comp.media.t3  <- distCompare(pairs.media,
                              theta="Theta3",
                              var="mediatrust")

comp.media.t1
comp.media.t2
comp.media.t3

## political orientation

pairs.pol <- t(combn(x = levels(thetas$polorient),
                     m = 2))


table(thetas$polorient)

comp.pol.t1 <- distCompare(pairs.pol,
                           theta="Theta1",
                           var="polorient") 
comp.pol.t2 <- distCompare(pairs.pol,
                           theta="Theta2",
                           var="polorient")
comp.pol.t3  <- distCompare(pairs.pol,
                            theta="Theta3",
                            var="polorient")

comp.pol.t1
comp.pol.t2
comp.pol.t3

## Country

pairs.state <- t(combn(x = levels(thetas$isocntry),
                       m = 2))

table(thetas$isocntry)

comp.state.t1 <- distCompare(pairs.state,
                             theta="Theta1",
                             var="isocntry") 
comp.state.t2 <- distCompare(pairs.state,
                             theta="Theta2",
                             var="isocntry")
comp.state.t3  <- distCompare(pairs.state,
                              theta="Theta3",
                              var="isocntry")

#comp.state.t1
#comp.state.t2
#comp.state.t3


comp.all <- rbind(comp.class.t1,
                  comp.class.t2,
                  comp.class.t3,
                  comp.media.t1,
                  comp.media.t2,
                  comp.media.t3,
                  comp.pol.t1,
                  comp.pol.t2,
                  comp.pol.t3,
                  comp.state.t1,
                  comp.state.t2,
                  comp.state.t3)
dim(comp.all) ##2460 x 5

comp.all$name <- paste0(comp.all$V1, " - ",
                        comp.all$V2, "-",
                        comp.all$whichtheta)

comp.all$label <- ifelse(
  comp.all$p.value <.01 &
    comp.all$var != "isocntry", 1,0)

table(comp.all$label) ## 2374 no; 86 yes 

table(comp.all$var)

comp.all$PresVar <- NA
comp.all[which(comp.all$var=="class"),
         "PresVar"] <- "Social Class"
comp.all[which(comp.all$var=="isocntry"),
         "PresVar"] <- "Survey Country"
comp.all[which(comp.all$var=="mediatrust"),
         "PresVar"] <- "Media Trust"
comp.all[which(comp.all$var=="polorient"),
         "PresVar"] <- "Politics"

## Plot
gg.c <- ggplot(data=comp.all,
               aes(y=statistic,
                   x=p.value)) +
  geom_point()+
  geom_label_repel(aes(label=ifelse(comp.all$label==1,
                                    as.character(comp.all$name),
                                    '')))+
  ggtitle("Kolmogorov Smirnov Test Results",
          subtitle="Paiwise Within Class, Media Trust, Political Orientation, Country Variables, Unconstrained IRT Model")+
  facet_wrap(~PresVar, ncol=2)+
  theme_bw()

gg.c

ggsave(gg.c,
       file="kstestgraphunconstrained.pdf")               


## Correlations:
## Between theta1; Theta3 & Theta 5


library(Hmisc)
library(corrplot)

colnames(thetas)[which(
  colnames(thetas) =="d63")] <- "socialclass"
head(thetas)

cor(thetas$Theta1, thetas$Theta5)
cor(thetas$Theta3, thetas$Theta5)

colnames(thetas)

corvars <- c("Theta1",
             "Theta2",
             "Theta3",
             "Theta4",
             "Theta5",
             "Theta6",
             "socialclass",
             "MoreBorderControl",
             "trusttradm",
             "trustwebonly",
             "trustallm",
             "trustnom")

dat <- thetas[,corvars]

cors <- round(cor(dat),
              digits = 3)

cors.pvalues <- rcorr(as.matrix(dat))


cors.pvalues

## Code for color-coded
## correlation plot from rbloggers

corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficiens on the diagonal
           diag = diag
  )
}



## Need to reset the "high" values of social class
## because they're NA/don't know/won't tel/
## Dropping for this
head(dat)

colnames(dat)[7:12] <- c("S.Class",
                         "MoreBorderCont",
                         "MediaTrustTradOnly",
                         "MediaTrustWebOnly",
                         "MediaTrustAll",
                         "MediaTrustNone")

## Drop small # of misleading values:

dat[which(dat$S.Class > 5),
    "S.Class"] <-NA
dat[which(dat$MoreBorderCont > 2),
    "MoreBorderCont"] <- NA

table(thetas$socialclass)
table(dat$S.Class)
table(dat$MoreBorderCont) ## 1= for; 2=against

head(dat)

pdf(file="corMatrixunconstrained.pdf")
corrplot2(
  data = dat,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75)
dev.off()

