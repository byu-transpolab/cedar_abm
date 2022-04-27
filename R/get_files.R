#' Get the GTFS File
#'
#' @param url GTFS url
#'
#'
get_gtfs <- function(url){
  file <- "r5/gtfs.zip"
  if(!file.exists(file)){
    download.file(url, destfile = file)
  }
  return(file)
}
