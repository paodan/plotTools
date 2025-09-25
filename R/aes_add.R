#' add aes elements
#' @param aes an aesthetic mapping
#' @param ... the elements to be added to aes
#' @importFrom rlang enquos inject
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' col = "x3"
#' shp = "x4"
#' ae = aes(x = x1, y = x2, color = .data[[col]])
#' print(ae)
#' ae2 = aes_add(ae, shape = .data[[shp]])
#' print(ae2)
#' }

aes_add = function(aes, ...){
  dots <- enquos(...)
  args <- c(aes, dots)
  local({
    aes <- function(x, y, ...) NULL
    inject(aes(!!!args))
  })
  new_aesthetic = getFromNamespace("new_aesthetic", "ggplot2")
  check_object = getFromNamespace("check_object", "ggplot2")
  new_aes <- function(x, env = globalenv()) {
    check_object(x, is.list, "a {.cls list}")
    x <- lapply(x, new_aesthetic, env = env)
    structure(x, class = "uneval")
  }

  # new_aes = getFromNamespace("new_aes", "ggplot2")
  rename_aes = getFromNamespace("rename_aes", "ggplot2")

  aes <- new_aes(args, env = parent.frame())
  rename_aes(aes)
}
