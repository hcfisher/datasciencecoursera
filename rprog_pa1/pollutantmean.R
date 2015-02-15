
filename <- function(directory, i) {
    id_new <-formatC(i,width=3,flag="0") 
    filename <- paste(directory, "/", id_new, ".csv", sep = "")
    return(filename)
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    values_vector <- numeric()
    len <- length(id)
    
    for (i in 1:len) {
        filename <- filename(directory, id[i])
        data <- read.csv(filename)
        values_vector <- c(values_vector, data[,pollutant])
    }
    
    return(mean(values_vector, na.rm=TRUE))
}



