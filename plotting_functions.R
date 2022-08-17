#!/usr/bin/env Rscript




##---------------------------------------------------------------
##                          Section 1                          --
##                 Histogram Related Functions                 --
##---------------------------------------------------------------

determineBinWidth <- function(sample_data, observations) {
  
  ##-- Utilizes the Freedman-Diaconis Rule
  numerator <- (2 * IQR(sample_data))
  denomenator <- pracma::nthroot(observations, 3)
  bin_width <- ( (numerator)/(denomenator) )

  return(bin_width)
}

scotts_binwidth <- function(sample_data, observations) {
  ##-- Utilizes Scott's Normal Reference Rule to identify optimal bin-width.
  numerator <- (3.49 * (sd(sample_data)))
  denomenator <- pracma::nthroot(observations, 3)
  bin_width <- ( (numerator)/(denomenator) )

  return(bin_width)
}

# doanes_binwidth <- function(sample_data. observations)

determineBinNumbers <- function(bin_width, sample_data) {

    numerator <- (max(sample_data) - min(sample_data))  
    num_bins <- ( (numerator)/(bin_width) )
    num_bins <- ceiling(num_bins)

    return(num_bins)

}

determineBinNumbers_excel <- function(observations) {
  ##-- Uses the method employed within the Excel Analysis Toolpack.
    num_bins <- pracma::nthroot(observations, 2)
    num_bins <- ceiling(num_bins)

    return(num_bins)
}


##---------------------------------------------------------------
##                          Section 2                          --
##                    Outlier Determination                    --
##---------------------------------------------------------------