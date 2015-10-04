best <- function(state, outcome){
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
                        min_rate[1]
                        
                }
                else if (outcome == "heart failure"){
                        f_rate <- state_data[,c(2,17)]
                        f_rate[,2] <- suppressWarnings(as.numeric(f_rate[,2]))
                        min_ratef <- f_rate[order(f_rate[,2],f_rate[,1],
                                                  na.last=TRUE),1]
                        min_ratef <- na.omit(min_ratef)
                        min_ratef[1]
                        
                }
                else if(outcome == "pneumonia"){
                        p_rate <- state_data[,c(2,23)]
                        p_rate[,2] <- suppressWarnings(as.numeric(p_rate[,2]))
                        min_ratep <- p_rate[which.min(p_rate[,2]),]
                        #min_ratep <- p_rate[order(p_rate[,2], na.last=TRUE),1]
                        #min_ratep[1]
                        min_ratep[,1]
                        
                }
                else {stop("Invalid Outcome")}
        }
        else {stop("Invalid State")}
        
}
        