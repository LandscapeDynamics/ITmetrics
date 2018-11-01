#' @name jpmf
#' @title Calculate the joint distribution
#' @description Calculate the joint probability mass function (joint
#' distribution) from a two-way table of frequencies.
#' @param m A matrix of numeric values in the
#' form of a two-way table of frequencies:
#' \if{html}{
#'   \tabular{ccccc}{
#'        \tab     \tab Fr  \tab     \tab    \cr
#'        \tab 40  \tab 38  \tab 37  \tab \dots\cr
#'   To   \tab 89  \tab 89  \tab 87  \tab \dots\cr
#'        \tab 75  \tab 74  \tab 89  \tab \dots\cr
#'        \tab \dots \tab \dots \tab \dots \tab \dots
#'   }
#' }
#' \if{latex}{
#' \deqn{
#'   \left(
#'     \begin{array}{cccc}
#' 40 & 38 & 37 & \dots \\
#' 89 & 89 & 87 & \dots \\
#' 75 & 74 & 89 & \dots \\
#' \vdots & \vdots & \vdots & \ddots
#'       \end{array}\right)
#' }
#' }
#'
#' \code{m} can be constructed using \code{\link{xt}}.
#' @details \code{jpmf} calculates the joint distribution from a matrix
#' \eqn{M} of frequencies. The joint distribution for discrete variables
#' \eqn{X} and \eqn{Y} contained in
#' \eqn{M} is denoted by \eqn{p(x,y)}, which is defined as:
#' \deqn{P(X = x, Y = y) = p(x,y)}{P(X = x, Y = y) = p(x,y)}
#' The joint probability at any intersection \eqn{i,j} in \eqn{M}
#' is calculated as:
#' \deqn{p(x_i,y_j) = \frac{M_{i,j}}{\sum M}}{p(x_i,y_j) = M_i,j / \sum M}
#' Further, across all \eqn{i} and \eqn{j} the Axioms of probability
#' indicate that
#' \deqn{\sum_i \sum_j p(x_i,y_j) = 1}{\sum p(x_i,y_j) = p(1,1) + p(1,2) + \dots + p(i,j) = 1}
#' @return Returns a matrix with the same dimensions as input \eqn{M}
#' indicating the joint distribution across all interactions of \eqn{X}
#' and \eqn{Y} in the form:
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
#' @examples
#' data(transitions)             # Load example data
#' b <- brkpts(transitions$phenofr, # Find 10 probabilistically
#'             10)                  #  equivalent breakpoints
#' m <- xt(transitions,          # Make transition matrix
#'         fr.col=2, to.col=3,
#'         cnt.col=4, brk=b)
#' pxy=jpmf(m)                   # Joint distribution
#' sum(pxy)                      # Check that the joint distribution (PAB) sum to 1
#' @author Bjorn J. Brooks, Lars Y. Pomara, Danny C. Lee
#' @references PAPER TITLE.
#' @export

jpmf <- function(m) {
  return(prop.table(m))                          # Joint distribution
}
