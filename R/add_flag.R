#' Add repelled row annotation to a heat map
#' @param pheatmap a pheatmap object
#' @param kept_labels a vector of characters, the row names to be annotated
#' @param repel_degree numeric, the extent to repel the annotation, the higher the
#' number is, the farther the adjacent row names are. Default is 0.
#' @return a new pheatmap object, in which the gtable is updated.
#' @importFrom lazyeval lazy_eval
#' @import grid
#' @import pheatmap
#' @importFrom gtable gtable_add_grob
#' @export
#' @examples
#' \dontrun{
#'
#' (expr = createExprData(100, 6))
#' library(pheatmap)
#' pm = pheatmap(expr, cluster_row = FALSE, scale = "row")
#'
#' add_flag(pm, kept_labels = c("G001", "G023", "G059"))
#' add_flag(pm, kept_labels = rownames(expr)[1:5], repel_degree = 0)
#' add_flag(pm, kept_labels = rownames(expr)[1:5], repel_degree = .5)
#' add_flag(pm, kept_labels = rownames(expr)[1:5], repel_degree = 1)
#' }
add_flag <- function(pheatmap,
                     kept_labels,
                     repel_degree = 0) {

  # repel_degree = number within [0, 1], which controls how much
  #                space to allocate for repelling labels
  ## repel_degree = 0: spread out labels over existing range of kept labels
  ## repel_degree = 1: spread out labels over the full y-axis

  heatmap <- pheatmap$gtable

  new_label <- heatmap$grobs[[which(heatmap$layout$name == "row_names")]]

  # keep only labels in kept_labels, replace the rest with ""
  new_label$label <- ifelse(new_label$label %in% kept_labels,
                            new_label$label, "")

  # calculate evenly spaced out y-axis positions
  repelled_y <- function(d, d_select, k = repel_degree){
    # d = vector of distances for labels
    # d_select = vector of T/F for which labels are significant

    # recursive function to get current label positions
    # (note the unit is "npc" for all components of each distance)
    strip_npc <- function(dd){
      if(!"unit.arithmetic" %in% class(dd)) {
        return(as.numeric(dd))
      }

      d1 <- strip_npc(dd$arg1)
      d2 <- strip_npc(dd$arg2)
      fn <- dd$fname
      return(lazyeval::lazy_eval(paste(d1, fn, d2)))
    }

    full_range <- sapply(seq_along(d), function(i) strip_npc(d[i]))
    selected_range <- sapply(seq_along(d[d_select]), function(i) strip_npc(d[d_select][i]))

    return(unit(seq(from = max(selected_range) + k*(max(full_range) - max(selected_range)),
                    to = min(selected_range) - k*(min(selected_range) - min(full_range)),
                    length.out = sum(d_select)),
                "npc"))
  }
  new_y_positions <- repelled_y(new_label$y,
                                d_select = new_label$label != "")
  new_flag <- segmentsGrob(x0 = new_label$x,
                           x1 = new_label$x + unit(0.15, "npc"),
                           y0 = new_label$y[new_label$label != ""],
                           y1 = new_y_positions)

  # shift position for selected labels
  new_label$x <- new_label$x + unit(0.2, "npc")
  new_label$y[new_label$label != ""] <- new_y_positions

  # add flag to heatmap
  heatmap <- gtable_add_grob(x = heatmap,
                             grobs = new_flag,
                             t = 4,
                             l = 4
  )

  # replace label positions in heatmap
  heatmap$grobs[[which(heatmap$layout$name == "row_names")]] <- new_label

  # plot result
  grid.newpage()
  grid.draw(heatmap)

  # return a copy of the heatmap invisibly
  pheatmap$gtable = heatmap
  return(invisible(pheatmap))
}
