#' @name prjv
#' @import expm
#' @title Project a vector using a transition matrix.
#' @description Project a vector using a transition matrix.
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
#' @param v A vector of numeric values with the same number of values as columns in \code{m}. \code{v} could be the initial distribution of frequencies \code{m}.
#' @details \code{prjv} calculates the...
#' @return Returns ...
#' @examples
#' data(transitions)                # Load example data
#' b <- brkpts(transitions$phenofr, # Find 10 probabilistically
#'             10)                  #  equivalent breakpoints
#' m <- xt(transitions,             # Make transition matrix
#'         fr.col=2, to.col=3,
#'         cnt.col=4, brk=b)
#' pxy <- jpmf(m)                   # Joint distribution
#' rmd <- rowSums(pxy)              # Row marginal distribution
#' cmd <- colSums(pxy)              # Column marginal distribution
#' r_c <- cpf(pxy,                  # Transition matrix
#'            margin='p(row|col)')  #  (row | col)
#' r_c.prj <- prjm(r_c,10^3)        # Project matrix 1,000 steps
#' seqv <- prjv(r_c.prj,cmd)        # Stable equilibrium vector
#' @author Bjorn J. Brooks, Lars Y. Pomara, Danny C. Lee
#' @references PAPER TITLE.
#' @export

prjv <- function(m,v) {
    output <- m %*% v                            # Project vec v by matrix m

    return(output)
}
