#' Make animation based on gif figures
#' @param imgs image files
#' @param pattern if imgs is not provided, then pattern and path are used to find
#' the image file names, in this case, the order of the frames in the animation
#' is the same as the file name order
#' @param path the directory of the image files
#' @param fps integer, frames per second
#' @param outputgif output gif animation file
#' @param optimize logical, optimize the gif animation by storing only the differences
#' between frames. Input images must be exactly the same size. The default is TRUE.
#' @param ... other parameters in `image_write` function from `magick` package.
#' @importFrom gtools mixedsort
#' @import magick
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(funcTools)
#' x = seq(0, 2*pi, length = 200)
#' y = sin(x)
#' data = data.frame(x, y)
#' folder = tempdir()
#' files = basename(tempfile())
#' pngs = fileNameFactory(folder, ".png")
#'
#' # Plotting figures
#' for(mi in 1:nrow(data)){
#'   g = ggplot(data[mi,], aes(x, y))+
#'   geom_point(color = "red")+
#'   geom_line(data = data)+
#'   coord_fixed(ratio = 2, xlim = c(0, 2*pi), ylim = c(-1, 1))
#'   ggsave(pngs("png2/", files, "_", mi), g, width = 4, height = 4)
#' }
#'
#' # Making the animation
#' gif = makeAnimation(path = paste0(folder, "/png2"), fps = 50,
#'               outputgif = paste0(folder, "/output.gif"))
#' gif$img_animated
#'
#'
#' # Making the animation just for changing some parameters
#' gif2 = makeAnimation(imgs = gif$img_joined, fps = 50,
#'               outputgif = paste0(folder, "/output_2.gif"))
#' gif2$img_animated
#'
#' # Making the animation without compressing the images
#' makeAnimation(path = paste0(folder, "/png2"), fps = 50,
#'               outputgif = paste0(folder, "/output2.gif"), optimize = FALSE)
#' }
#' @export
makeAnimation = function(imgs, pattern = NULL, path = NULL, fps = 2,
                         outputgif = "output.gif", optimize = TRUE, ...){
  if(missing(imgs)){
    stopifnot(!is.null(path))
    imgs = dir(path, pattern = pattern, full.names = T)
    if(length(imgs) < 2){
      stop("Number of imags is less than 2. Include more images.")
    }
  }
  if(is.character(imgs)){
    res = makeAnimationMethods.default(imgs = imgs, pattern = pattern, path = path, fps = fps,
                                       outputgif = outputgif, optimize = optimize, ...)
  } else {
    res = makeAnimationMethods(imgs = imgs, pattern = pattern, path = path, fps = fps,
                         outputgif = outputgif, optimize = optimize, ...)
  }

  return(invisible(res))
}


makeAnimationMethods = function(imgs, pattern = NULL, path = NULL, fps = 2,
                                outputgif = "output.gif", optimize = TRUE, ...){
  UseMethod("makeAnimationMethods")
}


#' @export
makeAnimationMethods.default = function(imgs, pattern = NULL, path = NULL, fps = 2,
                                 outputgif = "output.gif", optimize = TRUE, ...){

  factors100 = seq_len(100)[which(100 %% (seq_len(100)) == 0)]
  if(!fps %in% (factors100)){
    stop("fps must be a factor of 100: ", paste0(factors100, collapse = ", "))
  }
  dir.create(dirname(outputgif),F)
  if(missing(imgs)){
    stopifnot(!is.null(path))
    imgs = list.files(path, pattern, full.names = TRUE)
    imgs = mixedsort(imgs)
  }

  img_list = list()
  pb = txtProgressBar(min = 0, max = length(imgs), initial = 0)
  on.exit(close(pb))
  for(mi in seq_along(imgs)){
    # cat(mi, "\t")
    img_list[[mi]] = image_read(imgs[mi])
    setTxtProgressBar(pb,mi)
  }

  ## join the images together
  cat("\nJoining images together ...")
  img_joined <- image_join(img_list)


  res = .processAnimation(img_joined, fps = fps,
                          outputgif = outputgif, optimize = optimize, ...)
  return(invisible(res))
}


#' @export
"makeAnimationMethods.magick-image" = function(imgs, pattern = NULL, path = NULL, fps = 2,
                                 outputgif = "output.gif", optimize = TRUE, ...){

  factors100 = seq_len(100)[which(100 %% (seq_len(100)) == 0)]
  if(!fps %in% (factors100)){
    stop("fps must be a factor of 100: ", paste0(factors100, collapse = ", "))
  }
  dir.create(dirname(outputgif),F)

  res = .processAnimation(imgs, fps = fps,
                    outputgif = outputgif, optimize = optimize, ...)
  return(invisible(res))
}

.processAnimation = function(img_joined, fps = 2,
                             outputgif = "output.gif", optimize = TRUE, ...){
  ## animate at fps frames per second
  img_animated <- image_animate(img_joined, fps = fps, optimize = optimize)
  cat("\tDone!\nOutput GIF animation ...")

  ## save to disk
  image_write(image = img_animated, path = outputgif, ...)
  return(invisible(list(img_joined = img_joined,
                        img_animated = img_animated)))
}
