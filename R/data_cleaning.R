#' Clean Earthquake Location Field
#' \code{eq_location_clean} takes a data frame of NOAA earthquake data and cleans
#' the \code{LOCATION_NAME} field.
#' This function cleans the \code{LOCATION_NAME} observation in a NOAA
#' Significant Earthquakes data set.  It removes the country name (of form
#' \code{COUNTRY:}) from the \code{LOCATION_NAME} data (unless the country is
#' the only name present in that field), and converts the remainder of the field
#' to Title Case. This will make the location name easier to read when plotting
#' and mapping.
#' @import stringr
#' @param x A data frame of NOAA significant earthquake data, similar to what
#'   can be loaded with \code{data(earthquake)}.
#' @return dataframe A cleaned version of the earthquake data with cleaned version of \code{LOCATION_NAME}
#' column (removing country name, seperated with location name with ":")
eq_location_clean<- function(x){
  
  x$LOCATION_NAME <- stringr::str_to_title(with(x, gsub("^.*?:","",x$LOCATION_NAME)))
  return(x)
} 

#' Clean Earthquake data 
#' \code{eq_clean_data} takes a data frame of NOAA earthquake data and cleans it
#' up for further analysis.
#'  This function takes a data frame of NOAA earthquake data, which may be
#' obtained from NOAA's
#' \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{
#' EarthQuake Database}, or loaded into the envirnoment with \code{data(earthquake)}
#' and cleans it up for further analysis.  In
#' particular, it creates a \code{DATE} feature by combing the \code{YEAR},
#' \code{MONTH}, and \code{DAY} features, and makes sure the \code{LATITUDE} and
#' \code{LONGITUDE} features are of type \code{numeric}.
#' @param x A data frame of NOAA significant earthquake data, similar to what
#'   can be loaded with \code{data(earthquake)}.
#' @return dataframe. A cleaned version of the earthquake data with a new column \code{DATE}.

eq_clean_data <- function(x){
  x$DATE <- with(x,as.Date(paste(YEAR, MONTH, DAY, sep="-"),"%Y-%m-%d"))
  x$LATITUDE <- as.numeric(x$LATITUDE)
  x$LONGITUDE <- as.numeric(x$LONGITUDE)
  x$DEATHS <- as.numeric(x$DEATHS)
  x$TOTAL_DEATHS <- as.numeric(x$TOTAL_DEATHS)
  x$EQ_PRIMARY <- as.numeric(x$EQ_PRIMARY)
  
  return(x)
}

#' Load Pre-processed Earthquakes Data
#'
#' \code{eq_load_data} is to load pre-processed NOAA dataset.
#' \code{\link{earthquake}} data supplied with this package.
#'
#' This function is to load the \code{\link{earthquake}} data
#' supplied with this package.
#' @return A \code{datframe} of NOAA Significant Earthquakes Data, cleaned with
#'   the functions \link{eq_clean_data} and \link{eq_location_clean}. 
#'   
#' @export
#'
#' @examples
#' library(dplyr)
#' data <- eq_load_data()
#' 
eq_load_data <- function(){
  data <- get("earthquake") 
  data_new <-data %>%
    eq_clean_data() %>%
    eq_location_clean()
  return(data_new)
}
