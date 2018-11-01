#' @name mi
#' @title Calculate mutual information of transitions.
#' @description Calculate the average mutual information of transitions.
#' @param pxy a matrix indicating the joint distribution across
#' all interactions of \eqn{X} and \eqn{Y} (must sum to 1) in the form:
#' \if{html}{
#'   \tabular{ccccc}{
#'   p(x,y) \tab      \tab X    \tab      \tab    \cr
#'          \tab 0.06 \tab 0.06 \tab 0.06 \tab \dots\cr
#'   Y      \tab 0.14 \tab 0.14 \tab 0.14 \tab \dots\cr
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
#' @details Calculate matrix product of \code{pxy} and log base 2 of the
#' ratio of \code{pxy} over the standard product of \code{px} and \code{py},
#' then sum.
#' \deqn{\sum_i \sum_j -p(x_i,y_j) * log2 p(x_i,y_j)}{\sum -p(x_i,y_j) * log2 p(x_i,y_j) = -p(1,1) * log2 p(1,1) + -p(1,2) * log2 p(1,2) + \dots + -p(i,j) * log2 p(i,j)}
#' @return Returns a value indicating the Shannon diversity of all transitions.
#' @examples
#' data(transitions)                # Load example data
#' b <- brkpts(transitions$phenofr, # Find 10 probabilistically
#'             10)                  #  equivalent breakpoints
#' m <- xt(transitions,             # Make transition matrix
#'         fr.col=2, to.col=3,
#'         cnt.col=4, brk=b)
#' pxy <- jpmf(m)                   # Joint distribution
#' mi <- mi(pxy)                    # Avg mutual information of X and Y
#'
#' # Now calculate MI using a second method (answers should be identical)
#' mi2 <- sum(pxy * log2( cpf(pxy, margin='p(col|row)') / colSums(pxy)),
#'            na.rm=TRUE)
#' @author Bjorn J. Brooks, Lars Y. Pomara, Danny C. Lee
#' @references PAPER TITLE.
#' @export

mi <- function(pxy) {
  output <- sum(pxy *                            # Mutual information
		log2(pxy / (rowSums(pxy) %*% t(colSums(pxy)))),
		na.rm=TRUE)

  #output <- sum(pxy * log2( cpf(pxy, margin='p(row|col)') / rowSums(pxy)),
  #              na.rm=TRUE) # 2nd method for calculating MI
  #output <- sum(pxy * log2( cpf(pxy, margin='p(col|row)') / colSums(pxy)),
  #              na.rm=TRUE) # 3rd method for calculating MI [INCORRECT ANSWER]

  return(output)
}
