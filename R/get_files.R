#' Get the GTFS File
#'
#' @param url GTFS url
#' @return path to the gtfs file
#' @par
#'
get_gtfs <- function(url){
  file <- "r5/gtfs.zip"
  if(!dir.exists(dirname(file))){
    dir.create(dirname(file))
  }
  if(!file.exists(file)){
    download.file(url, destfile = file)
  }
  return(file)
}



#' Get the OSM PBF file
#'
#' @param url URL to the Geofabrik.de osm pbf planet file
#' @param bb named vector with xmin, ymin, xmax, and ymax
#'   latitudes and longitudes for clipping the pbf file
#'
#' @return path to clipped pbf
#'
get_osmpbf <- function(url, bb){

  file <- "r5/region.osm.pbf"
  if(!dir.exists(dirname(file))){
    dir.create(dirname(file))
  }
  # check if otp file is already there.
  if(!file.exists(file)){

    # if not, download file from osm
    geofabrik_file <- "r5/geofabrik.osm.pbf"
    if(!file.exists(geofabrik_file)){
      # get the osm pbf file from geofabrik
      download.file(url, geofabrik_file)
    }

    # osmosis --read-pbf file=ohio.osm.pbf --bounding-polygon file=input.poly --tf accept-ways boundary=administrative --used-node --write-xml output.osm
    system2("/opt/homebrew/bin/osmosis",
            args = c(
              stringr::str_c("--read-pbf file=", geofabrik_file, sep = ""),
              stringr::str_c(
                "--bounding-box",
                " top=", bb$ymax,
                " left=", bb$xmin,
                " bottom=", bb$ymin,
                " right=", bb$xmax,
                sep = ""
              ),
              "--tf accept-ways highway=*",
              "--used-node",
              stringr::str_c("--write-pbf file=", file, sep = "")
            ))

    file.remove(geofabrik_file)
  } else {
    message(file, " already available")
  }


  return(file) # to use file target, need to return path to data.
}

#' Get the bounding box for a set of PUMAs
#'
#' @param pumas a list of pumas
#'
#' @return A named vector with the bounding box.
#'
get_bb <- function(pumas){

  # find list of states in the pumas supplied
  states <- unique(substr(pumas, 1, 2))

  # get the shape of all pumas
  tigris::pumas(state = states, year = 2020) %>%
    dplyr::filter(GEOID10 %in% pumas) %>%
    # get the bounding box
    sf::st_bbox(pumas_sf)
}
