

filename <- function(directory, i) {
    id_new <-formatC(i,width=3,flag="0") 
    filename <- paste(directory, "/", id_new, ".csv", sep = "")
    return(filename)
}

complete <- function(directory, id = 1:332) {
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
    
    output <- data.frame(id=numeric(0), nobs=numeric(0))
    
    len <- length(id)
    for (i in 1:len) {
        filename <- filename(directory, id[i])
        data <- read.csv(filename)
        output <- rbind(output,c(id[i],sum(complete.cases(data))))
        }
    colnames(output) <- c("id","nobs")
    return(output)
}

