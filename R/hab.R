#' @name hab
#' @title Calculate Shannon diversity of all transitions.
#' @description Calculate measures of transition diversity using the
#' Shannon index. Note that the formulas are conditional to omit zero
#' probability values from the calculation.
#' @param jd A matrix indicating the joint distribution across all
#' interactions of \eqn{X} and \eqn{Y} in the form:
#' \if{html}{
#'   \tabular{ccccc}{
#'   p(x,y) \tab      \tab Y    \tab      \tab    \cr
#'          \tab 0.06 \tab 0.06 \tab 0.06 \tab \dots\cr
#'   X      \tab 0.14 \tab 0.14 \tab 0.14 \tab \dots\cr
#'          \tab 0.12 \tab 0.12 \tab 0.14 \tab \dots\cr
#'          \tab \dots  \tab \dots  \tab \dots  \tab \dots
#'   }
#' }
#' \if{latex}{
#'   \deqn{
#'     \left(
#'       \begin{array}{cccc}
#'   0.06 & 0.06 & 0.06 & \dots \\
#'   0.14 & 0.14 & 0.14 & \dots \\
#'   0.12 & 0.12 & 0.14 & \dots \\
#'   \vdots & \vdots & \vdots & \ddots
#'         \end{array}\right)
#'   }
#' }
#' @details Element-wise multiply matrix \code{jd} by logarithm base 2
#' \code{jd} and sum.
#' \deqn{\sum_i \sum_j -p(x_i,y_j) * log2 p(x_i,y_j)}{\sum -p(x_i,y_j) * log2 p(x_i,y_j) = -p(1,1) * log2 p(1,1) + -p(1,2) * log2 p(1,2) + \dots + -p(i,j) * log2 p(i,j)}
#' @return Returns a value indicating the Shannon diversity of all transitions.
#' @examples
#' data(transitions)             # Load example data
#' b <- brkpts(transitions$phenofr, # Find 10 probabilistically
#'             10)                  #  equivalent breakpoints
#' m <- xt(transitions,          # Make transition matrix
#'         fr.col=2, to.col=3,
#'         cnt.col=4, brk=b)
#' jd <- jpmf(m)                 # Joint distribution
#' hab(jd)                       # Shannon diversity
#' @author Bjorn J. Brooks, Lars Y. Pomara, Danny C. Lee
#' @references PAPER TITLE.
#' @export

hab <- function(jd) {
  output <- -sum(jd * log2(jd), na.rm=TRUE)      # Shannon diversity

  return(output)
}
