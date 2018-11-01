#' A data set representing the number of observed transitions that occurred between distinct phenological classes, as determined by k-means clustering o f MODIS NDVI. Each row indicates the number ($count) of MODIS pixels withing a geographical area ($cell) that changed phenological states from one class ($phenofr) to another ($phenoto). The data represent an area covering 450 MODIS pixels. The hexagon is  5937.2 acres (2402.7 ha) 9.3 square miles (24.0 sq km) or about 3 miles across (5 km).  Each MODIS pixel is ~242x242 meters.
#'
#' A dataset representing phenological transitions for one FIA-sized hexagon.
#'
#' @format A data frame with 693 rows and 4 variables:
#' \describe{
#'   \item{cell}{Cell number}
#'   \item{phenofr}{Phenological cluster class in the prior year}
#'   \item{phenoto}{Phenological cluster class in the subsequent year}
#'   \item{count}{The number of pixels that transitioned from the phenofr to the phenoto class}
#'   ...
#' }
#' @source \url{http://dx.doi.org/10.3334/ORNLDAAC/1299}
"transitions"
