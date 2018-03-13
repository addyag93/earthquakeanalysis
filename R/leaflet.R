#' Interactive Earthquake Map
#'
#' \code{eq_map} creates an interactive \code{\link[leaflet]{leaflet}} map
#' showing the location of earthquakes in the given \code{earthquake} data set.
#'
#' This function shows an interactive map of the location of the earthquakes in
#' the given \code{\link{earthquake}} data.  The size of the circles are
#' proportional to the magnitude of the earthquakes (in the \code{EQ_PRIMARY})
#' variable. The map is interactive, and when you click on a link, the popup
#' shows the annotation as specified by the \code{annot_col} variable.
#' @param x The \code{\link{earthquake}} data frame.
#' @param annot_col The column to use for the popup annotation.
#' @seealso \code{\link{eq_create_label}} to create a more useful popup
#'   annotation in the \code{earthquake} data frame.
#'
#' @return Returns an interactive \code{leaflet} map.
#' @export
#'
#' @examples
#' library(lubridate); library(dplyr)
#'
#' ## use 'DATE' as annotation
#' earthquakes <- eq_load_data()
#' earthquakes %>%
#'  dplyr::filter(COUNTRY == 'JAPAN') %>%
#'  dplyr::filter(lubridate::year(DATE) >= 2000)
#'
#' eq_map(earthquakes, annot_col = 'DATE')
#' 
eq_map <- function(x, annot_col ="DATE"){
  x<- x %>%
    dplyr::mutate_(popup_col = as.name(annot_col))
  
  map_leaflet <- leaflet::leaflet (x) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = ~LONGITUDE,
      lat = ~LATITUDE,
      radius = ~EQ_PRIMARY,
      color = "blue",
      stroke = FALSE,
      weight =2 , 
      popup = ~ as.character(popup_col)
    )
  return(map_leaflet)
  
}

#' Popup Label for Earthquake Map
#'
#' \code{eq_create_label} creates a more descriptive and HTML-formatted popup
#' label to be used in \code{\link{eq_map}}.
#'
#' This function creates a vector of HTML-formatted labels using supplied
#' \code{\link{earthquake}} data. Variables:
#' \itemize{
#'   \item DATE
#'   \item LOCATION_NAME (as cleaned in the \code{\link{eq_location_clean}}
#'   function).
#'   \item EQ_PRIMARY (earthquake magnitude)
#'   \item TOTAL_DEATHS
#' }
#' 
#' @param x The \code{\link{earthquake}} data frame.
#'
#' @return A vector with the HTML-formatted labels. You should include this
#' vector with the data frame that is sent to \code{\link{eq_map}}.
#' 
#' @export
#' 
#' @examples
#' library(dplyr); library(lubridate)
#' earthquakes <- eq_load_data()  
#' earthquakes %>%
#'  dplyr::filter(COUNTRY == 'INDIA') %>%
#'  dplyr::filter(lubridate::year(DATE) >= 2000) %>%
#'  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = 'popup_text')
#' 
eq_create_label <- function(x){
  
  label_assign <- function(date, location, magnitude, tdeaths) {  
    pop_text <- ''
    if (!(is.na(date))) {
      pop_text <- paste0(pop_text,'<b>Date:</b> ',as.Date(date, origin = '1970-01-01'),'<br/>')
    }
    if (!(is.na(location))) {
      pop_text <- paste0(pop_text, '<b>Location:</b> ', location, '<br/>')
    }
    if (!(is.na(magnitude))) {
      pop_text <- paste0(pop_text, '<b>Magnitude:</b> ', magnitude, '<br/>')
    }
    if (!(is.na(tdeaths))) {
      pop_text <- paste0(pop_text, '<b>Total Deaths:</b> ', tdeaths, '<br/>')
    }
    
    pop_text
  }
  
  x <- x %>%
    dplyr::mutate_(popup_text = ~purrr::pmap_chr(list(DATE,
                                                      LOCATION_NAME, 
                                                      EQ_PRIMARY, 
                                                      TOTAL_DEATHS),
                                                 label_assign))
  
  x$popup_text
}