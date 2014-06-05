## The function below takes an outcome and a rank as inputs and provide the name
## of the hospital corresponding to those variables across the entire USA

rankall <- function(outcome, num = "best"){
        my_file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcomevec <- c("heart attack", "heart failure", "pneumonia")
        if(all(outcomevec != outcome)){
                stop("invalid outcome")
        }
        
        my_statelist <- levels(factor(my_file$State))
        colList <- c(11,17,23)
        for(j in seq_along(outcomevec)){
                if(outcome == outcomevec[j]){
                        my_data <- my_file[,c(2,colList[j],7)]
                        colnames(my_data) <- c("hospital", "rate", "state")
                        my_numb1 <- suppressWarnings(as.numeric(my_data$rate))
                        my_data2 <- subset(my_data, my_numb1 == my_numb1)
                        num_rows <- numeric()
                        for(k in seq_along(my_statelist)){
                                num_rows <- c(num_rows, nrow(subset(my_data2, my_data2$state == my_statelist[k])))
                        }
                        my_data7 <- data.frame()
                        my_ithna <- character()
                        my_na <- data.frame()
                        state_list <- character()
                        result1 <- data.frame()
                        my_result1 <- NULL
                        my_result2 <- NULL
                        for(k in seq_along(my_statelist)){
                                if(num > 0 & num <= num_rows[k]){
                                        my_data3 <- subset(my_data2, my_data2$state == my_statelist[k]) 
                                        my_data4 <- order(suppressWarnings(as.numeric(my_data3[,2])),my_data3[,1], na.last = NA)
                                        my_data5 <- my_data3[my_data4,]
                                        my_data6 <- my_data5[num,]
                                        my_data7 <- rbind(my_data7, my_data6)
                                        my_result1 <- my_data7[,c(1,3)]
                                        rownames(my_result1) <- my_result1[,2]
                                }
                        }
                        for(k in seq_along(my_statelist)){
                                if(num > num_rows[k]){
                                        my_data8 <- subset(my_data2, my_data2$state == my_statelist[k])
                                        my_data9 <- my_data8[1,]
                                        my_ithna[k] <- "NA"
                                        my_data9[,1] <- my_ithna[k]
                                        my_na <- rbind(my_na, my_data9)
                                        my_result2 <- my_na[,c(1,3)]
                                        rownames(my_result2) <- my_result2[,2]
                                }
                        }
                       
                        
                        my_data16 <- data.frame()
                        my_numb2 <- numeric()
                        if(num == "worst"){
                                for(k in seq_along(my_statelist)){
                                        my_data10 <- subset(my_data2, my_data2$state == my_statelist[k]) 
                                        my_numb2 <- suppressWarnings(as.numeric(my_data10[,2]))
                                        my_data12 <- subset(my_data10,  my_numb2 == max(my_numb2))
                                        my_data13 <- order(suppressWarnings(as.numeric(my_data12[,2])),my_data12[,1], na.last = NA)
                                        my_data14 <- my_data12[my_data13,]
                                        my_data15 <- my_data14[1,]
                                        my_data16 <- rbind(my_data16, my_data15)
                                        my_result3 <- my_data16[,c(1,3)]
                                        rownames(my_result3) <- my_result3[,2]
                                }
                                return(my_result3)
                        }
                        if(num == "best"){
                                for(k in seq_along(my_statelist)){
                                        my_data10 <- subset(my_data2, my_data2$state == my_statelist[k]) 
                                        my_numb2 <- suppressWarnings(as.numeric(my_data10[,2]))
                                        my_data12 <- subset(my_data10,  my_numb2 == min(my_numb2))
                                        my_data13 <- order(suppressWarnings(as.numeric(my_data12[,2])),my_data12[,1], na.last = NA)
                                        my_data14 <- my_data12[my_data13,]
                                        my_data15 <- my_data14[1,]
                                        my_data16 <- rbind(my_data16, my_data15)
                                        my_result3 <- my_data16[,c(1,3)]
                                        rownames(my_result3) <- my_result3[,2]
                                }
                                return(my_result3)
                        }
                        if(!is.null(my_result1) && !is.null(my_result2)){
                                result1 <- rbind(my_result1, my_result2)
                                result2 <- result1[,c(1,2)]
                                result3 <- order(suppressWarnings(result2[,2]),result2[,1], na.last = NA)
                                result4 <- result2[result3,]
                                return(result4)
                        } else if(is.null(my_result1) && !is.null(my_result2)) {
                                return(my_result2)
                        } else if(is.null(my_result2) && !is.null(my_result1)){
                                return(my_result1)
                        }
                }
        }
}

