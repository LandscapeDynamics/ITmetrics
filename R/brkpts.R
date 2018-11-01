#' @name brkpts
#' @title Calculate the breakpoints of a vector of values
#' @description Calculate the breakpoints that partition a vector of
#' values into bins that are probabilistically equivalent.
#' @importFrom ggplot2 cut_number
#' @param x A numeric vector of values to find bin breakpoints for.
#' @param n A numeric value representing the number of bins.
#' \code{x} into.
#' @details Calculate the breakpoints that partition a vector of
#' values into bins that are probabilistically equivalent. That is find
#' \eqn{b} breakpoints (upper limits) such that the proportion of values
#' in each bin is equivalent. Let \eqn{k} be the number of values in
#' set \eqn{X} and \eqn{n} be the number of bins. Then each bin has
#' \eqn{\frac{k}{n}}{k/n} values. Further, the breakpoints separating
#' bins \eqn{\{b_i, b_{i+1}, \dots, b_n\}}{{b_i, b_i+1, \dots, b_n}}
#' are distributed such that the probability \eqn{p} of drawing a value
#' from a bin \eqn{b_i} is equivalent across all bins. That is:
#' \deqn{p(b_i) = p(b_{i+1}) = \dots = p(b_n)}{p(b_i) = p(b_i+1) = \dots = p(b_n)}
#' @return Returns a vector of values representing the breakpoints
#' that divide input vector \code{x} into bins, which have an equivalent
#' likelihood of being drawn.
#' @examples
#' x <- rnorm(10^3)  # Make random values
#' b <- brkpts(x,10) # Find 10 probabilistically equivalent breakpoints
#' @author Bjorn J. Brooks, Lars Y. Pomara, Danny C. Lee
#' @references PAPER TITLE.
#' @export

brkpts <- function(x,n) {

	breaks <- unique(cut_number(x,n=n))      # Calc. probabalistically
	                                         #  equivalent bins

	brk <- gsub(']', '',                     # Parse to only include 
		    gsub('^.*,', '', breaks))    #  max num for ranges

	brk <- as.numeric(brk)                   # Convert to numeric

	brk <- na.omit(brk)                      # Remove NA's

	brk <- sort(brk)                         # Sort

	brk <- c(min(x,na.rm=TRUE),brk)          # Add val for lowest brkpt

	return(brk)
}
