#R Programming, Coursera
#Assignment 3, week 4
#Robin Fondberg
#Uses the file "outcome-of-care-measures.csv", obtained from 
#ProgAssignment3-data.zip file containing the data for Programming Assignment 3 
#from the Coursera web site.


#Function 1
#Returns the hospital in a state with the lowest 30-days mortality for the 
#specified outcome. 
#Input: state=two letter abbreviated name of the state, outcome= one of 
#"heart attack", "heart failure", "pneumonia". 
#Output: hospital name

best <- function(state, outcome){
        #read data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        
        #select appropriate variables, rename them and make numeric values numeric
        data <- data[, c(2, 7, 11,17,23)]
        names(data) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
        for (i in 3:5){
                data[,i] <- as.numeric(data[,i])
        } 
        
        #select state and find min value, then return the name of the state
        stateData <- data[which(data$state==state), ]
        if (length(stateData[,"state"])==0) {
                cat(c("Error in best(",state,", ", outcome,"): invalid state"), sep="", "\n")
        }
        if (length(which(c("heart attack", "heart failure", "pneumonia") == outcome))==0){
                cat(c("Error in best(",state,", ", outcome,"): invalid outcome"), sep="")
        }
        if (length(stateData[,"state"])!=0 && 
            outcome %in% c("heart attack", "heart failure", "pneumonia")){
                minValue <- min(stateData[,outcome], na.rm = T)
                stateData[which(stateData[,outcome]==minValue), 1]
        }
}


#Function 2
#Returns the name of the hospital in a state with a specific ranking.  
#Input: state=two letter abbreviated name of the state; outcome= one of 
#"heart attack", "heart failure", "pneumonia"; num=ranking
#Output: hospital name

rankhospital <- function(state, outcome, num){
        #change best to 1
        if(num=="best"){
                num <- 1
        }
        
        #read data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", 
                         na.strings = "Not Available")
        
        #select appropriate variables, rename them and make numeric values numeric
        data <- data[, c(2, 7, 11,17,23)]
        names(data) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
        
        #check validity of state, outcome and num
        if(!(state %in% data$state)){
                stop("Invalid state")
        }
        if(!(outcome %in% c("heart failure", "heart attack", "pneumonia"))){
                stop("Invalid outcome")
        }
        
        #subset data according to state
        for (i in 3:5){
                data[,i] <- as.numeric(data[,i])
        } 
        data <- data[,c("name", "state", outcome)]
        data <- data[which(data$state==state), ]
        
        #add ranking 
        data <- data[with(data, order(data[,outcome], name)), ]
        data$rank <- 1:length(data$state)
        
        for(i in 1:length(data$state)){
                if(is.na(data[i, outcome])){
                        data[i, "rank"] <- NA
                }
        }
        
        #change worst to the largest number in rank
        if(num=="worst"){
                num <- max(data[, "rank"], na.rm=TRUE)
        }
        
        #return output; NA if num>length(data) and the name of the hospital otherwise
        if (num>sum(!is.na(data[,outcome]))){
                return(NA)
        }
        else{
                data[which(data$rank==num), 1]
        }
        
        
}


#Function 3
#Returns the hospitals in all states with the specified ranking 
#Input: state=two letter abbreviated name of the state, outcome= one of 
#"heart attack", "heart failure", "pneumonia"
#Output: dataframe with columns; hospital and state
#(didnt see that rankhospital shouldnt be used)

rankall <- function(outcome, num){
        #read data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        
        #select appropriate variables, rename them and make numeric values numeric
        data <- data[, c(2, 7, 11,17,23)]
        names(data) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
        for (i in 3:5){
                data[,i] <- as.numeric(data[,i])
        } 
        
        #create vector with all the states
        allStates <- unique(data[, "state"])
        
        #check validity of outcome 
        if(!(outcome %in% c("heart failure", "heart attack", "pneumonia"))){
                stop("Invalid outcome")
        }
        
        #create dataframe with all states and empty vector "hospital"
        df <- data.frame(hospital=NA, state=allStates)
        
        #find the values for vector hospital
        for(i in 1:length(df$state)){
                df[i, "hospital"] <- rankhospital(as.character(df$state[i]), outcome, num)   
        }
        
        #return df
        return(df)
}

