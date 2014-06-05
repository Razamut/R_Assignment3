## This function gives the name of the best hospital for a given state and outcome

best <- function(state, outcome){
        ## For a given state and outcome, this function will name the best hospital
        
        my_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomevec <- c("heart attack", "heart failure", "pneumonia")
        if(all(my_file[,7] != state)){
                stop("invalid state")
        }
        if(all(outcomevec != outcome)){
                stop("invalid outcome")
        }
        colList <- c(11,17,23)
        for(j in seq_along(outcomevec)){
                if(outcome == outcomevec[j]){
                        my_data <- subset(my_file, my_file$State == state, select = c(2,colList[j]))
                        my_muffler <- suppressWarnings(as.numeric(my_data[,2]))
                        my_data2 <- subset(my_data, my_muffler == min(my_muffler,na.rm = T))
                        my_data3 <- sort(my_data2[,1])
                }
        }
        return(my_data3[1])
}