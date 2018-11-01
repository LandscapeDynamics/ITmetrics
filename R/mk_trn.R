#' @name mk_trn
#' @title Tabulate from-to transitions by cell
#' @description Given 3 vectors of equivalent length representing the year
#' (or time identifier), cell ID, and group (cluster) assignment, make a 
#' transition table consisting of 4 columns indicating the year, cell,
#' from-group (this year's group), and to-group (next year's group).
#' @param time A numeric vector of values indicating the years (or
#' sampling points) at which each group assigment was made (e.g. 1, 2,
#' 3,...).
#' @param cell A numeric vector of values indicating the cell number
#' for each group assignment.
#' @param group A numeric vector of values indicating the group
#' membership of each cell in each year.
#' \code{x} into.
#' @return Returns a data frame with four columns representing the year
#' cell, from-group (this year's group), and to-group (next year's group).
#' @examples
#' # Test 1, Robust test of algorithm
#' # Make example in which only cells 2,3,4,5 experience transitions
#' #   (cells 1,6,7 only contain one value)
#' t <- c(1,1,2,1,2,3,1,2,3,1,2,1,1) # Time values
#' c <- c(1,2,2,3,3,3,4,4,4,5,5,6,7) # Cell IDs
#' g <- c(1,1,2,1,2,3,3,2,1,1,2,1,1) # Cluster groups
#' mk_trn(time=t, cell=c, group=g) # Make transition table
#' # Test 2, Test data table input
#' tmp <- data.frame(cell=rep(1:2, each=3),
#'		     time=rep(1:3, times=2),
#'		     cluster=c(letters[1:3], LETTERS[1:3])) # Data table
#' tmp # Print to screen
#' mk_trn(time=tmp$time, cell=tmp$cell, group=tmp$cluster) # Make trans tbl
#' @author Bjorn J. Brooks, Lars Y. Pomara, Danny C. Lee
#' @references PAPER TITLE.
#' @export

mk_trn <- function(time, cell, group) {
  # First find the indices of all the cells that occur at least twice
  dup1 <- duplicated(cell)                       # Log vec of repeated cell IDs
                                                 #  excluding 1st duplicate
  dup2 <- duplicated(cell, fromLast = TRUE)      # Log vec excl. last dup.

  # Combine so that only elements that reapeat at least once are included
  dup <- dup1 | dup2                             # Log vec of repeat cell IDs

  time_new <- time[dup]                          # Exclude single occurrence
                                                 #  (i.e., no transitions)
  cell_new <- cell[dup]                          # Exclude single occ. cells

  group_new <- group[dup]                        # Exclude single occ. cells

  idx <- duplicated(cell_new, fromLast = TRUE)   # Indx that excludes last dup

  ne <- length(cell_new)                         # No. of transitions

  tmp <- data.frame(cell = cell_new[1:(ne-1)],   # Make list of transitions
		    time = time_new[1:(ne-1)],   #  by joining group_new to
		    fr = group_new[1:(ne-1)],    #  itself after offsetting
		    to = group_new[2:ne])        #  (a simple "trick")

  output <- tmp[idx,]                            # Exlude last transition
                                                 #  (not actually a trans.
                                                 #  within the same cell)
  return(output)
}
