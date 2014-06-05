## For a given state, outcome and rank, the below function will provide the name of 
## the hospital corresponding to those variables

rankhospital <- function(state, outcome, num = "best"){
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
                        my_data2 <- order(suppressWarnings(as.numeric(my_data[,2])),my_data[,1], na.last = NA)
                        my_data3 <- my_data[my_data2,]
                        if(num > length(my_data[,1])){
                                my_result <- NA
                        }
                        if(num == "worst"){
                                my_numb <- as.numeric(my_data3[,2])
                                my_data4 <- subset(my_data3, my_numb == max(my_numb))
                                my_result <- my_data4[1,1]
                        }
                        if(num == "best"){
                                my_numb <- as.numeric(my_data3[,2])
                                my_data4 <- subset(my_data3, my_numb == min(my_numb))
                                my_result <- my_data4[1,1]
                        }
                        if(num >0 & num <= length(my_data[,1])){
                                my_numb <- as.numeric(num)
                                my_data4 <- my_data3[1:my_numb,]
                                my_result <- my_data4[my_numb,1]
                        }
                }
        }
        return(my_result)
}
















