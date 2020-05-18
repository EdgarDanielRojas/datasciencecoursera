unzip('data/rprog_data_ProgAssignment3-data.zip')

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

head(outcome)

# Graph histogram of 30 day deathrates from heart attack
outcome[,11] <- as.numeric(outcome[,11])
# NAs may be produced but no problem
hist(outcome[,11])

best <- function(state, outcome) {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if (state %in% outcome_data[,7] == FALSE){
                stop('invalid state')
        }
        valid_outcome<- c('heart attack','heart failure','pneumonia')
        if (outcome %in% valid_outcome == FALSE){
                stop('invalid outcome')
        }
        indices_outcome = c(11,17,23)
        indice_to_consult = indices_outcome[match(outcome,valid_outcome)]
        ## Return hospital name in that state with lowest 30-day death
        state_cases = outcome_data[outcome_data[,7]==state,]
        state_min = min(as.numeric(state_cases[,indice_to_consult]),na.rm=TRUE)
        good_indices= as.numeric(state_cases[,indice_to_consult])==state_min
        remaining_cases = state_cases[good_indices,2]
        #print(remaining_cases)
        return(min(remaining_cases,na.rm = TRUE))
        # print(min(state_cases))
        ## rate
}

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if (state %in% outcome_data[,7] == FALSE){
                stop('invalid state')
        }
        valid_outcome<- c('heart attack','heart failure','pneumonia')
        if (outcome %in% valid_outcome == FALSE){
                stop('invalid outcome')
        }
        indices_outcome = c(11,17,23)
        indice_to_consult = indices_outcome[match(outcome,valid_outcome)]
        state_cases = outcome_data[outcome_data[,7]==state,]
        amountnotna <- length(which(!is.na(as.numeric(state_cases[,indice_to_consult]))))
        rank_to_consult <- 0
        if (is.numeric(num)){
                rank_to_consult = num
        }
        else if (is.character(num)){
                valid_num<- c('best','worst')
                if (num %in% valid_num == FALSE){
                        stop('invalid rank')
                }
                indices_num = c(1,amountnotna)
                rank_to_consult = indices_num[match(num,valid_num)]
        }
        else {
                stop('invalid rank')
                
        }
        if (rank_to_consult>amountnotna){
                return(NA)
        }
        non_na_states = state_cases[which(!is.na(as.numeric(state_cases[,indice_to_consult]))),]
        sorted_non_na_states = order(as.numeric(non_na_states[,indice_to_consult]),non_na_states[,2])
        ## Return hospital name in that state with the given rank
        return(non_na_states[sorted_non_na_states[rank_to_consult],2])
        ## 30-day death rate
}

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that num and outcome are valid
        valid_outcome<- c('heart attack','heart failure','pneumonia')
        if (outcome %in% valid_outcome == FALSE){
                stop('invalid outcome')
        }
        indices_outcome = c(11,17,23)
        indice_to_consult = indices_outcome[match(outcome,valid_outcome)]
        state_list <- unique(outcome_data[,7])
        ##
        state_rank <- data.frame()
        ## For each state, find the hospital of the given rank
        for(state in state_list){
                state_cases = outcome_data[outcome_data[,7]==state,]
                amountnotna <- length(which(!is.na(as.numeric(state_cases[,indice_to_consult]))))
                rank_to_consult <- 0
                if (is.numeric(num)){
                        rank_to_consult = num
                }
                else if (is.character(num)){
                        valid_num<- c('best','worst')
                        if (num %in% valid_num == FALSE){
                                stop('invalid rank')
                        }
                        indices_num = c(1,amountnotna)
                        rank_to_consult = indices_num[match(num,valid_num)]
                }
                else {
                        stop('invalid rank')
                        
                }
                if (rank_to_consult>amountnotna){
                        row <- c(NA,state)
                }else{
                        non_na_states = state_cases[which(!is.na(as.numeric(state_cases[,indice_to_consult]))),]
                        sorted_non_na_states = order(as.numeric(non_na_states[,indice_to_consult]),non_na_states[,2])
                        ## Return hospital name in that state with the given rank
                        row<- c(non_na_states[sorted_non_na_states[rank_to_consult],2],state)
                }
                state_rank <- rbind(state_rank, row)
        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        names(state_rank)<- c('hospital','state')
        state_rank[order(state_rank$state),]
}