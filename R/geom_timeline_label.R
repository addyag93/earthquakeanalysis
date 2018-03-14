#' Label Earthquakes on Timeline
#'
#' \code{geom_timeline_label} works best when used with
#' \code{\link{geom_timeline}}, labeling the top \code{n} earthquakes, by
#' magnitude, with a specified label field.  By default, the labels are for
#' the top 5 earthquakes for each country specified, however, the user may
#' adjust this with the \code{n_max} aesthetic.
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
#' \code{geom_timeline_label} undertands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'  \item \strong{x}: recommend \code{DATE}
#'  \item \strong{label}: recommend \code{LOCATION_NAME}
#'  \item \strong{magnitude}: recommend \code{EQ_PRIMARY}
#'  \item y: recommend \code{COUNTRY}
#'  \item n_max: default 5. Top \code{n} earthquakes to label,
#'        sorted by magnitude.
#'  \item color
#'  \item linetype
#'  \item size
#'  \item alpha
#' }
#'
#' @export
#'
#' @examples
#' library(dplyr); library(ggplot2)
#' earthquakes <- eq_load_data()
#'
#' earthquakes %>%
#'   dplyr::filter(COUNTRY %in% c('USA', 'INDIA')) %>%
#'   dplyr::filter(DATE > '2005-05-01') %>%
#'   ggplot() +
#'   geom_timeline(aes(x = DATE, y = COUNTRY, color = TOTAL_DEATHS,
#'                     size = EQ_PRIMARY)) +
#'   geom_timeline_label(aes(x = DATE, y = COUNTRY, magnitude = EQ_PRIMARY,
#'                          label = LOCATION_NAME, n_max = 5)) +
#'   scale_size_continuous(name = 'Richter scale value') +
#'   scale_color_continuous(name = 'Deaths') +
#'   theme_eq()
#'   
geom_timeline_label <-function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
GeomTimelineLabel <-ggplot2::ggproto("GeomTimelineLabel",ggplot2::Geom,
                                     required_aes = c("x", "label", "magnitude"),
                                     default_aes = ggplot2::aes(
                                       n_max = 10,
                                       y = 0,
                                       color = "blue",
                                       size = 0.5,
                                       linetype = 1,
                                       alpha = 0.2
                                     ),
                                     draw_key = ggplot2::draw_key_point,
                                     draw_panel = function(data, panel_scales, coord) {
                                       n_max<- data$n_max[1]
                                       data <- data %>%
                                         dplyr::mutate(magnitude = magnitude / max(magnitude) * 2) %>%
                                         dplyr::group_by(group) %>%
                                         dplyr::top_n(n_max, magnitude)
                                       data$xend <- data$x
                                       data$yend <- data$y + 0.2
                                       geom_vertical_line <- ggplot2::GeomSegment$draw_panel(unique(data), panel_scales, coord)
                                       
                                       
                                       data$y <- data$yend + 0.03
                                       data$fontface <- 10
                                       data$lineheight <- 2
                                       data$angle <- 30
                                       data$hjust <- 'left'
                                       data$vjust <- 'top'
                                       data$family <- 'sans'
                                       data$size <- 2
                                       data$colour <- 'red'
                                       geom_text <- ggplot2::GeomText$draw_panel(unique(data), panel_scales, coord)
                                       
                                       ggplot2:::ggname('geom_timeline_label', grid::grobTree(geom_vertical_line, geom_text))
                                     }
)