# geom class

#' Earthquake Timeline
#' \code{geom_timeline} shows a timeline of NOAA Significant earthquakes.
#' The size of the points is relatative to the earthquakes' magnitude, and
#' the color is related to the total number of deaths.
#' @param mapping data mapping
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If TRUE NA are ignored
#' @param show.legend ogical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ... other parameters
#' 
#' @section Aesthetics:
#' \code{geom_timeline} undertands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'  \item x: recommend \code{DATE}
#'  \item y: recommend \code{COUNTRY}
#'  \item size: recommend \code{EQ_PRIMARY}
#'  \item color: recommend \code{TOTAL_DEATHS}
#'  \item alpha
#'  \item shape
#' }
#' 
#' @export
#' 
#' @examples
#' library(dplyr); library(ggplot2)
#' quakes <- eq_load_data()
#'
#' quakes %>%
#'   dplyr::filter(COUNTRY %in% c('USA', 'INDIA')) %>%
#'   dplyr::filter(DATE > '2005-05-01') %>%
#'   ggplot() +
#'   geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
#'                     size = EQ_PRIMARY)) +
#'   scale_size_continuous(name = 'Richter scale value') +
#'   scale_color_continuous(name = 'Deaths') 
#'   

geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes =c("x"),
                                 default_aes = ggplot2::aes(
                                   y=0,
                                   size=2,
                                   color="grey50",
                                   alpha= 0.2,
                                   shape=19,
                                   stroke=0.5,
                                   fill = NA
                                 ),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord){
                                   ## Transform the data first
                                   coords <- coord$transform(data, panel_scales)
                                   coords$size <- coords$size / max(coords$size) * 2
                                   ## Construct a grid grob
                                   grid::pointsGrob(
                                     x = coords$x,
                                     y = coords$y,
                                     pch = coords$shape,
                                     gp = grid::gpar(
                                       col = coords$colour,
                                       alpha = coords$alpha,
                                       cex = coords$size
                                     )
                                   )
                                 }
)