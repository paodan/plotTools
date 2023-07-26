#' Plot errorbar by providing error ranges
#' @name geom_errorbar2
#' @param mapping	Set of aesthetic mappings created by aes(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#'
#' @param data The data to be displayed in this layer. There are three options:
#'
#'   If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot().
#'
#' A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify() for which variables will be created.
#'
#' A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data. A function can be created from a formula (e.g. ~ head(.x, 10)).
#'
#' @param stat
#' The statistical transformation to use on the data for this layer, either as a ggproto Geom subclass or as a string naming the stat stripped of the stat_ prefix (e.g. "count" rather than "stat_count")
#'
#' @param position Position adjustment, either as a string naming the adjustment (e.g. "jitter" to use position_jitter), or the result of a call to a position adjustment function. Use the latter if you need to change the settings of the adjustment.
#' @param errorbar_direction the direction of the error bars, either
#' "updown", "up", or "down".
#' @param na.rm	If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param orientation	The orientation of the layer. The default (NA) automatically determines the orientation from the aesthetic mapping. In the rare event that this fails it can be given explicitly by setting orientation to either "x" or "y". See the Orientation section for more detail.
#' @param show.legend	logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes	If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param import rlang
#' @param import ggplot2
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' df <- data.frame(
#'   trt = factor(c(1, 1, 2, 2)),
#'   resp = c(1, 5, 3, 4),
#'   group = factor(c(1, 2, 1, 2)),
#'   upper = c(1.1, 5.3, 3.3, 4.2),
#'   lower = c(0.8, 4.6, 2.4, 3.6)
#' )
#'
#' p = ggplot(df, aes(trt, resp, colour = group))
#'
#' # default errorbar in ggplot2
#' p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)
#'
#' # also default errorbar in ggplot2
#' p + geom_errorbar2(aes(ymin = lower, ymax = upper), errorbar_direction = "updown", width = 0.2)
#'
#' # upper errorbar
#' (p2 = p + geom_errorbar2(aes(ymax = upper), errorbar_direction = "up", width = 0.2))
#'
#' # lower errorbar
#' (p3 = p + geom_errorbar2(aes(ymin = lower), errorbar_direction = "down", width = 0.2))
#'
#' # upper errorbar first and then lower errorbar
#' (p4 = p +
#'     geom_errorbar2(aes(ymax = upper), errorbar_direction = "up", width = 0.2)+
#'     geom_errorbar2(aes(ymin = lower), errorbar_direction = "down", width = 0.2))
#'
#'
#' ## plot error bar using stat_summary function
#' df = ToothGrowth
#' df$dose = as.factor(df$dose)
#' p = ggplot(df, aes(x=dose, y=len)) +
#'   geom_dotplot(binaxis='y', stackdir='center', binwidth = 1, dotsize = .5)
#' p
#'
#' # Use geom_errorbar()
#' p + stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
#'                  geom="errorbar", color="red", width=0.2) +
#'   stat_summary(fun.y=mean, geom="point", color="red")
#'
#' mean_se = function(x, na.rm = TRUE){
#'   if (na.rm)  x = x[!is.na(x)]
#'   n = length(x)
#'   if (n == 0)
#'     return(c(y = NA, ymin = NA, ymax = NA))
#'   xbar = sum(x)/n
#'   se = sd(x)/sqrt(n)
#'   # the name of the elements in the following vector must be "y", "ymin", and "ymax".
#'   c(y = xbar, ymin = xbar - se, ymax = xbar + se)
#' }
#'
#' p + stat_summary(fun.data=mean_se,
#'                  geom=GeomUperrorbar, color="red", width=0.2) +
#'   stat_summary(fun.y=mean, geom="point", color="blue")
#' }
geom_errorbar2 <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           errorbar_direction = c("updown", "up", "down"),
                           na.rm = FALSE,
                           orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  errorbar_direction = match.arg(errorbar_direction)

  if(errorbar_direction == "updown"){
    Geom_ErrorBar = ggplot2:::GeomErrorbar
  } else if(errorbar_direction == "up"){
    Geom_ErrorBar = GeomUperrorbar
  } else if(errorbar_direction == "down"){
    Geom_ErrorBar = GeomDownerrorbar
  } else {
    stop("Unknown errorbar_direction")
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = Geom_ErrorBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname geom_errorbar2
#' @export
GeomUperrorbar = ggproto("GeomUperrorbar", Geom,
                         default_aes = aes(colour = "black", linewidth = 0.5,
                                           linetype = 1, width = 0.5,
                                           alpha = NA),
                         draw_key = draw_key_path,
                         required_aes = c("x", "y", "ymax"),


                         setup_params = function(data, params) {
                           # GeomLinerange$setup_params(data, params)
                           params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE)
                           params
                         },

                         extra_params = c("na.rm", "orientation"),

                         setup_data = function(data, params) {
                           `%||%` = getFromNamespace("%||%", "ggplot2")

                           data$flipped_aes <- params$flipped_aes
                           data <- flip_data(data, params$flipped_aes)

                           data$width <- data$width %||%
                             params$width %||% (resolution(data$x, FALSE) * 0.9)
                           data = transform(data, xmin = x - width / 2, xmax = x + width / 2, width = NULL)
                           flip_data(data, params$flipped_aes)
                         },
                         draw_panel = function(data, panel_scales, coord, width = NULL, flipped_aes = FALSE) {
                           check_linewidth = getFromNamespace("check_linewidth", "ggplot2")
                           data <- check_linewidth(data, snake_class(self))
                           data <- flip_data(data, flipped_aes)

                           x = as.vector(rbind(data$xmin, data$xmax, NA, data$x,    data$x))
                           y = as.vector(rbind(data$ymax, data$ymax, NA, data$ymax, data$y))
                           data = data.frame(
                             x = x,
                             y = y,
                             colour = rep(data$colour, each = 5),
                             alpha = rep(data$alpha, each = 5),
                             linewidth = rep(data$linewidth, each = 5),
                             linetype = rep(data$linetype, each = 5),
                             group = rep(1:(nrow(data)), each = 5),
                             stringsAsFactors = FALSE,
                             row.names = 1:(nrow(data) * 5)
                           )
                           data = flip_data(data, flipped_aes)
                           GeomPath$draw_panel(data, panel_scales, coord)
                         },

                         rename_size = TRUE)


#' @rdname geom_errorbar2
#' @export
GeomDownerrorbar = ggproto("GeomDownerrorbar", Geom,
                           default_aes = aes(colour = "black", linewidth = 0.5,
                                             linetype = 1, width = 0.5,
                                             alpha = NA),
                           draw_key = draw_key_path,
                           required_aes = c("x", "y", "ymin"),

                           setup_params = function(data, params) {
                             params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE)
                             params
                           },

                           extra_params = c("na.rm", "orientation"),

                           setup_data = function(data, params) {
                             `%||%` = getFromNamespace("%||%", "ggplot2")

                             data$flipped_aes <- params$flipped_aes
                             data <- flip_data(data, params$flipped_aes)

                             data$width <- data$width %||%
                               params$width %||% (resolution(data$x, FALSE) * 0.9)
                             data = transform(data, xmin = x - width / 2, xmax = x + width / 2, width = NULL)
                             flip_data(data, params$flipped_aes)
                           },
                           draw_panel = function(data, panel_scales, coord, width = NULL, flipped_aes = FALSE) {
                             check_linewidth = getFromNamespace("check_linewidth", "ggplot2")
                             data <- check_linewidth(data, snake_class(self))
                             data <- flip_data(data, flipped_aes)

                             data = data.frame(
                               x = as.vector(rbind(data$xmin, data$xmax, NA, data$x,    data$x)),
                               y = as.vector(rbind(data$ymin, data$ymin, NA, data$ymin, data$y)),
                               colour = rep(data$colour, each = 5),
                               alpha = rep(data$alpha, each = 5),
                               linewidth = rep(data$linewidth, each = 5),
                               linetype = rep(data$linetype, each = 5),
                               group = rep(1:(nrow(data)), each = 5),
                               stringsAsFactors = FALSE,
                               row.names = 1:(nrow(data) * 5)
                             )
                             data = flip_data(data, flipped_aes)
                             GeomPath$draw_panel(data, panel_scales, coord)
                           },
                           rename_size = TRUE)

