## Script to take some demographic variables
## that we will eventually attach to the theta estimates

library(tidyverse)
library(stats)


## 1. Load and Subset Data:

ebdat <- read_csv(paste0("./Data/","SI395.csv"))

## Subset instrument:
## Want responses starting at qa1a_1
## (column 58)

colnames(ebdat) 

length(unique(ebdat$isocntry))## 40 countries represented
## Trust social media vs traditional media:
## QA6a trust in media
## QA20 for trust to get info on covid
## QD4a where get news

colnames(ebdat)[grep(colnames(ebdat), pattern="a6a")]


## 1: trust;  2: not trust; 3: NA/DN?
table(ebdat$"qa6a_1") ## qa6a_1 written pres
## qa6a_2 is radio
## qa6a_3 is TV
## qa6a_4 is internet
## qa6a_5 is online social networks

table(ebdat$qa6a_5) ## 


## trust traditional sources
ebdat$trusttradm <- 0

ebdat[which(ebdat$qa6a_1==1 | ## written press
            ebdat$qa6a_2==1  |## radio
            ebdat$qa6a_3==1 & ## TV
            ebdat$qa6a_4== 2 & ## Internet
            ebdat$qa6a_5 ==2), ## web social nets
      "trusttradm"] <- 1


## trust internet sources
## And not traditional sources
ebdat$trustwebonly <- 0
ebdat[which(ebdat$qa6a_1==2 & ## written press
            ebdat$qa6a_2==2  &## radio
            ebdat$qa6a_3==2 & ## TV
            ebdat$qa6a_4== 1 | ## Internet
            ebdat$qa6a_5 ==1),
      "trustwebonly"] <- 1## web social nets

## trust all
ebdat$trustallm <- 0
ebdat[which(ebdat$qa6a_1==1 & ## written press
            ebdat$qa6a_2==1 &## radio
            ebdat$qa6a_3==1 & ## TV
            ebdat$qa6a_4== 1 &  ## Internet
            ebdat$qa6a_5 == 1),
      "trustallm"] <- 1## online social nets

## trust none

ebdat$trustnom <- 0
ebdat[which(ebdat$qa6a_1== 2 & ## written press
            ebdat$qa6a_2==2 &## radio
            ebdat$qa6a_3==2 & ## TV
            ebdat$qa6a_4== 2 &  ## Internet
            ebdat$qa6a_5 == 2),## online social nets
      "trustnom"] <- 1 

##
## Factor version:

ebdat$mediatrust <- "Other" ## Other as base category
ebdat[which(ebdat$qa6a_1==1 | ## written press
            ebdat$qa6a_2==1  |## radio
            ebdat$qa6a_3==1 & ## TV
            ebdat$qa6a_4== 2 & ## Internet
            ebdat$qa6a_5 ==2), ## web
      "mediatrust"] <- "TrustTrad" ## Trad media 


ebdat[which(ebdat$qa6a_1==2 & ## written press
            ebdat$qa6a_2==2  &## radio
            ebdat$qa6a_3==2 & ## TV
            ebdat$qa6a_4== 1 | ## Internet
            ebdat$qa6a_5 ==1), ## online social nets
      "mediatrust"] <- "TrustWebOnly" ## web-only

## trust all
ebdat[which(ebdat$qa6a_1==1 & ## written press
            ebdat$qa6a_2==1 &## radio
            ebdat$qa6a_3==1 & ## TV
            ebdat$qa6a_4== 1 &  ## Internet
            ebdat$qa6a_5 == 1),## online social nets
      "mediatrust"] <- "TrustAll" ## trust all

## trust none
ebdat[which(ebdat$qa6a_1== 2 & ## written press
            ebdat$qa6a_2==2 &## radio
            ebdat$qa6a_3==2 & ## TV
            ebdat$qa6a_4== 2 &  ## Internet
            ebdat$qa6a_5 == 2),## online social nets
      "mediatrust"] <- "TrustNone"  ## Trust no media

table(ebdat$mediatrust)

colnames(ebdat)

table(ebdat$d63) ## self-identified class

## Make factor
ebdat$class <- NA
ebdat[which(ebdat$d63==1),
      "class"] <- "WorkingClass"
ebdat[which(ebdat$d63==2),
      "class"] <- "LowerMiddle"
ebdat[which(ebdat$d63==3),
      "class"] <- "MiddleClass"
ebdat[which(ebdat$d63==4),
      "class"] <- "UpperMiddleClass"
ebdat[which(ebdat$d63==5),
      "class"] <- "UpperClass"
ebdat[which(ebdat$d63 == 6|
            ebdat$d63 == 7 |
            ebdat$d63 == 8 |
            ebdat$d63 == 9),
      "class"] <- "Other/None/DN/Refused"

table(ebdat$d63)

table(ebdat$class)

ebdat$polorient <- NA
ebdat[which(ebdat$d1== 1 | ## furthest two left
            ebdat$d1==2),
      "polorient"] <- "FarLeft" ## 

ebdat[which(ebdat$d1== 3| ## middle left
            ebdat$d1==4),
      "polorient"] <- "Left" ## 

ebdat[which(ebdat$d1== 5 | ## center of scale
            ebdat$d1==6 ),
      "polorient"] <- "Center" ## 

ebdat[which(ebdat$d1== 7 | ## right of center
            ebdat$d1==8),
      "polorient"] <- "Right" ## 

ebdat[which(ebdat$d1== 9 | ## far right
            ebdat$d1==10),
      "polorient"] <- "FarRight" ##

ebdat[which(ebdat$d1== 97 | ## far right
            ebdat$d1==98),
      "polorient"] <- "Refused/DNt" ## 

table(ebdat$polorient)
## now compile:
## class; education? (neither in the data?)

## want:

colnames(ebdat)

table(ebdat$"qb7_2")

cols <- c("uniqid",
          "isocntry",
          "d63", ##social class
          "qb7_2", ## boder control
          "class",
          "trusttradm",
          "trustwebonly",
          "trustallm",
          "trustnom",
          "polorient",
          "mediatrust")


dem <- ebdat[,cols]

write_csv(dem,
     file="./Data/ebdatDemographVaribs.csv")
