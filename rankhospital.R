rankhospital <- function(state, outcome, num = "best"){
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        data[,11] <- suppressWarnings(as.numeric(data[,11]))
        data[,17] <- suppressWarnings(as.numeric(data[,17]))
        data[,23] <- suppressWarnings(as.numeric(data[,23]))
        data <- na.omit(data)
        if(state %in% data$State){
                state_data <- data[data$State == state,]
                if(outcome == "heart attack"){
                        a_rate <- state_data[,c(2,11)]
                        a_rate[,2] <- suppressWarnings(as.numeric(a_rate[,2]))
                        min_rate <- a_rate[order(a_rate[,2],a_rate[,1],
                                                 na.last=TRUE),1]
                        min_rate <- na.omit(min_rate)
                        #min_rate
                        if(num == "best"){min_rate[1]}
                        else if(num == "worst"){min_rate[length(min_rate)]}
                        else if(!num > length(min_rate) & num <= 0){
                                stop("NA")
                        }
                        else {min_rate[num]}
                        
                }
                else if (outcome == "heart failure"){
                        f_rate <- state_data[,c(2,17)]
                        f_rate[,2] <- suppressWarnings(as.numeric(f_rate[,2]))
                        min_ratef <- f_rate[order(f_rate[,2],f_rate[,1],
                                                  na.last=TRUE),1]
                        min_ratef <- na.omit(min_ratef)
                        #min_ratef
                        if(num == "best"){min_ratef[1]}
                        else if(num == "worst"){min_ratef[length(min_ratef)]}
                        else if(!num > length(min_ratef) & num <= 0){
                                stop("NA")
                        }
                        else {min_ratef[num]}
                        
                }
                else if(outcome == "pneumonia"){
                        p_rate <- state_data[,c(2,23)]
                        p_rate[,2] <- suppressWarnings(as.numeric(p_rate[,2]))
                        #min_ratep <- p_rate[which.min(p_rate[,2]),]
                        min_ratep <- p_rate[order(p_rate[,2],p_rate[,1],
                                                  na.last=TRUE),1]
                        min_ratep <- na.omit(min_ratep)
                        if(num == "best"){min_ratep[1]}
                        else if(num == "worst"){min_ratep[length(min_ratep)]}
                        else if(!num > length(min_ratep) & num <= 0){
                                stop("NA")
                        }
                        else {min_ratep[num]}
                }
                else {stop("Invalid Outcome")}
        }
        else {stop("Invalid State")}
        
}
