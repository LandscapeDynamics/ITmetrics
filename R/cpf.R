#' @name cpf
#' @title Calculate conditional probabilities
#' @description Calculate conditional probabilities of a matrix from
#' the joint and marginal probabilities.
#' @param pxy A matrix, array, or data frame of numeric values
#' representing the joint distribution across all interactions of \eqn{X}
#' and \eqn{Y} (must sum to 1) in the form:
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
#' \deqn{
#'   \left(
#'     \begin{array}{cccc}
#'   0.06 & 0.06 & 0.06 & \dots \\
#'   0.14 & 0.14 & 0.14 & \dots \\
#'   0.12 & 0.12 & 0.14 & \dots \\
#'   \vdots & \vdots & \vdots & \ddots
#'       \end{array}\right)
#' }
#' }
#' @param margin a character argument that can be either "p(row|col)" or
#' "p(col|row)" indicating if the desired conditional probability is
#' p(row|col) or p(col|row). In these example data p(row|col) corresponds
#' to p(to|from).
#' @details \code{cpf} calculates the conditional probabilities
#' \eqn{p(Y|X)} across all discrete interactions of the two variables.
#' Let \eqn{PXY} be a matrix of joint probabilities with rows \eqn{j}
#' and columns \eqn{i} and let \eqn{PX_i} represent a vector
#' of marginal probabilities. The probability of event \eqn{Y} under
#' the condition that \eqn{X} has already occurred is found by:
#' \deqn{\frac{PXY_{i,j}}{PX_i}= PX\_Y_{i,j}}{PXX_i,j / PX_i = PX_Yi,j}
#' wherever \eqn{PX_i} is greater than zero.
#' @return Returns a matrix of conditional probabilities with the same
#' number of rows and columns as input matrix \eqn{PXY} and has the form:
#' \if{html}{
#'   \tabular{ccccc}{
#'   p(y|x) \tab      \tab X    \tab      \tab    \cr
#'          \tab 0.19  \tab 0.19  \tab 0.18  \tab \dots\cr
#'   Y      \tab 0.44  \tab 0.44  \tab 0.41  \tab \dots\cr
#'          \tab 0.38  \tab 0.38  \tab 0.41  \tab \dots\cr
#'          \tab \dots \tab \dots \tab \dots \tab \dots
#'   }
#' }
#' \if{latex}{
#' \deqn{
#'   \left(
#'     \begin{array}{cccc}
#' 0.19  & 0.19  & 0.18 & \dots \\
#' 0.44  & 0.44  & 0.41 & \dots \\
#' 0.38  & 0.38  & 0.41 & \dots \\
#' \vdots & \vdots & \vdots & \ddots
#'       \end{array}\right)
#' }
#' }
#' @examples
#' data(transitions)                 # Load example data
#' b <- brkpts(transitions$phenofr,  # 4 probabilistically
#'             4)                    #  equivalent breakpoints
#' m <- xt(transitions,              # Make transition matrix
#'         fr.col=2, to.col=3,
#'         cnt.col=4, brk=b)
#' pxy <- jpmf(m)                    # Joint distribution
#' r_c <- cpf(pxy,                   # Conditional probabilites
#'            margin='p(row|col)')   #  (row | col)
#' colSums(r_c)                      # Check that each column sums to 1
#' @author Bjorn J. Brooks, Lars Y. Pomara, Danny C. Lee
#' @references PAPER TITLE.
#' @export

cpf <- function(pxy, margin) {
  if (margin == 'p(row|col)') {
#    output <- t(pxy) / colSums(pxy)
    output <- t(t(pxy) / colSums(pxy))           # Calc. conditional prob's
                       # (& transpose matrix & product to align output dim's)
  } else if (margin == 'p(col|row)') {
    output <- pxy / rowSums(pxy)                 # Calc. conditional prob's
  } else {
    stop('Input argument "margin" is incorrect.')
  }

  # Conditional probabilities calculated where marginal probabilites are
  #	zero will result in NaN. Change these to zeros.
  output[is.na(output)] <- 0

  return(output)
}
