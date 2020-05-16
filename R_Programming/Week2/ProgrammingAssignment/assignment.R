unzip('data/rprog_data_specdata.zip')

# Functions to create

pollutantmean <- function(directory,pollutant,id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either sulfate or nitrate
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  files_list <- list.files(directory, full.names=TRUE)
  dat <- data.frame()
  for (i in id) {                                
    #loops through the files, rbinding them together 
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  mean(dat[, pollutant], na.rm=TRUE)      #identifies the mean of the pollutant
                                          #while stripping out the NAs
}

complete <- function(directory,id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the 
  ## number of complete cases
  files_list <- list.files(directory, full.names=TRUE)
  dat <- data.frame()
  for (i in id) {                                
    #loops through the files, rbinding them together
    file_data <- read.csv(files_list[i])
    amountnotna <- length(which(complete.cases(file_data)))
    row <- c(i,amountnotna)
    dat <- rbind(dat, row)
  }
  names(dat)<- c('id','nobs')
  dat
}

corr <- function(directory,threshold = 0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all 
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## Note: Do not round the result!
  files_list <- list.files(directory, full.names=TRUE)
  cor_vector <- vector('numeric')
  amount_files = length(files_list)
  for (i in 1:amount_files){
    if (complete(directory,i)['nobs']>threshold){
      file_data <- read.csv(files_list[i])
      indexes <- complete.cases(file_data)
      rows<-file_data[indexes,]
      corre <- cor(rows[,'nitrate'],rows[,'sulfate'])
      cor_vector <- c(cor_vector, corre)
    }
  }
  cor_vector
}
