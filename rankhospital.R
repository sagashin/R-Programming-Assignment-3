setwd("C:/Users/S.Sagara/Documents/Data Science/coursera/R specialization/R programming Assigment3")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
nrow(outcome)
names(outcome)
str(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        st <- unique(df[,7])
        if(sum(state==st)==0){
                stop("invalid state")
        }
        oc <- c("heart attack", "heart failure","pneumonia")
        if(sum(outcome==oc)==0){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        st_flg <- df[,7]==state
        df2 <- df[st_flg,]
        if(outcome == "heart attack"){
                naf <- df2[,11]=="Not Available"
                df2 <- df2[!naf,]
                nrow <- nrow(df2)
                if(num == "best"){
                        rnk <- 1
                } else if(num == "worst"){
                        rnk <- nrow
                } else if (num > nrow){
                        rnk <- 0
                } else{
                        rnk <- num
                }
                if(rnk == 0){
                        return(NA)
                } else{
                        out <- df2[order(as.numeric(df2[,11]),df2[,2]),]
                        return(out[rnk,2])
                }
        }
        else if(outcome == "heart failure"){
                naf <- df2[,17]=="Not Available"
                df2 <- df2[!naf,]
                nrow <- nrow(df2)
                if(num == "best"){
                        rnk <- 1
                } else if(num == "worst"){
                        rnk <- nrow
                } else if (num > nrow){
                        rnk <- 0
                } else{
                        rnk <- num
                }
                if(rnk == 0){
                        return(NA)
                } else{
                        out <- df2[order(as.numeric(df2[,17]),df2[,2]),]
                        return(out[rnk,2])
                }        
        }
        else if(outcome == "pneumonia"){
                naf <- df2[,23]=="Not Available"
                df2 <- df2[!naf,]
                nrow <- nrow(df2)
                if(num == "best"){
                        rnk <- 1
                } else if(num == "worst"){
                        rnk <- nrow
                } else if (num > nrow){
                        rnk <- 0
                } else{
                        rnk <- num
                }
                if(rnk == 0){
                        return(NA)
                } else{
                        out <- df2[order(as.numeric(df2[,23]),df2[,2]),]
                        return(out[rnk,2])
                }        
        }
}
rankhospital("TX", "heart failure", 4)

