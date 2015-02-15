corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    location_obs <- complete(directory)
    rel_locations <- location_obs[location_obs$nobs>threshold,]
    
    cr <- numeric()
    
    len <- length(rel_locations$id)
    
    if (len==0) {
        return(cr)
    } else {
    
    for (i in 1:len) {
        filename <- filename(directory, rel_locations$id[i])
        data <- read.csv(filename)
    
        rel_cor <- cor(data$sulfate, data$nitrate, use="complete.obs")
        cr <- c(cr, rel_cor)
    }
    return(cr)
    }
}


