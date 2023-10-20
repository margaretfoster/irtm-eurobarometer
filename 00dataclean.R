library(tidyverse)
library(stats)


## 1. Load and Subset Data:

ebdat <- read_csv(paste0("./Data/",
                         "SI395.csv"))

MCodes <- read_csv(paste0("./data/",
                           "Immigration_EB_MCodesV2.csv"))

## V2 correctes for a couple of miscodes, notabley in "qb7_2" (EU border control)

## Only keep M-Codes with loadings or outcomes:
MCodes$encoding <- rowSums(abs(MCodes[,4:9])) 
MCodes <- MCodes[which(MCodes$encoding > 0),]

dim(MCodes) ## 355 x 10

head(MCodes)
sum(abs(MCodes[,5]))

## Subset instrument:
## Want responses starting at qa1a_1
## (column 58)

colnames(ebdat)

ebdatsub <- ebdat[,c(58:413)]

summary(ebdatsub)

## 2. One Hot Encoding for instrument

library(fastDummies)

## Convert numeric ordinal responses to factors
ebdatsub <- lapply(ebdatsub[,], factor)

ebbinary <- dummy_cols(.data=ebdatsub,
                       remove_selected_columns=TRUE)

## 3. Make formatting of EB responses consisent
## with formatting of M-Code object:

## A- Convert . to _:

colnames(ebbinary) <- gsub(x=colnames(ebbinary),
                           pattern="\\.",
                           replacement="_")

## B - Clean up column instrument column names:
colnames(ebbinary) <- gsub(x=colnames(ebbinary),
                           pattern=".data.",
                               replacement="")


## B- Lowercase

MCodes$"QMap" <- tolower(MCodes$"QMap")

colnames(ebbinary) <- tolower(colnames(ebbinary))

colnames(ebbinary)
##summary(ebbinary)

## 4. Confirm overlap of coded and instrument columns:
## Verify no coded loadings
## that are missing from the instrument data
## (NB: if length of missing != 0 might have to go in and do a lot of hand-standardizing)

missing <- setdiff(tolower(MCodes$'QMap'),
                   tolower(colnames(ebbinary)))

missing

cat("Num coded loadings missing from instrument : ",
    length(missing))

##  5. Merge Data and Loadings

Y <- ebbinary ## data

colnames(MCodes)

##Combine data and M Codes:
## Produce a K-coded questions x R Responses data:
combine <- MCodes[,c(2, 4:9)] %>% ## question codes and loadings 
    inner_join(
        Y %>% 
        t() %>% 
        as.data.frame(stringsAsFactors = FALSE) %>% 
        type_convert() %>%
        rownames_to_column(var = "question"),
        by = c("QMap" = "question"  )
    ) 

dim(combine) ## 355 x 38728

## 6. Create M-Matrix:
## List of dxd matricies

d <- 6 #number of coded dimensions

loadings <- 2:7 ## columns in combine with the loadings
## NB: need to not select metadata if you have any

M <- array(NA, c(d, d, nrow(combine)))
for (i in 1:nrow(combine)) {
    M[,,i] <- diag(combine[i,
                           loadings])
}

## Verify dimensions as expected:
cat("\n\nK :", dim(M))

## 7. Rebuild Y:
## Creates df of dim K coded loadings  x
## (M metadata + R responses)

# rebuild Y
Y <- combine[, (d+2):ncol(combine)]%>%
    t() %>%
    as.data.frame()

Y <- as.data.frame(sapply(Y, as.numeric))

question <- combine[,1] %>%
    as.data.frame() ## this takes the question name

colnames(Y) <- question[,1]

Y[1:10, 1:10]

class(Y[2,2])

Y[1:5, 1:5]


saveRDS(M,
        file="EB_M.rds")

saveRDS(Y,
        file="EB_Y.rds")

