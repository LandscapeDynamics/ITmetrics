#' @name xt
#' @title Construct a two-way frequency table (matrix) from a 2 or 3
#' variable array of values
#' @description Construct a two-way frequency table (matrix) by reforming and
#' aggregating counts into bins.
#' @importFrom stats xtabs aggregate na.omit
#' @param trn An array or data frame with two or three columns
#' (either from, to or from, to, count). Each row should at
#' least designate the from-group & to-group (count is optional).
#' \if{html}{
#'   \tabular{ccc}{
#'   From   \tab To   \tab Count \cr
#'   x1  \tab x1  \tab 40  \cr
#'   x1  \tab x2  \tab 89  \cr
#'   ... \tab ... \tab ...
#'   }
#' }
#' \if{latex}{
#' \deqn{
#'   \left(
#'     \begin{array}{cccc}
#' x1 & x1 & 40 & \dots \\
#' x1 & x2 & 89 & \dots \\
#' ... & ... & ... & \dots \\
#' \vdots & \vdots & \vdots & \ddots
#'       \end{array}\right)
#'   }
#' }
#' @param fr.col The column number of \code{t} for the `from' variable, which
#' will be reformed to vary across columns in the output matrix.
#' @param to.col The column number of \code{t} for the `to' variable, which
#' will be reformed to vary across rows in the output
#' matrix.
#' @param cnt.col (optional) The column number of \code{t} for the `count'
#' variable. If unspecified then each row will be counted as one
#' observation.
#' @param brk (optional) Either a value indicating the number of bins to
#' aggregate \code{t} into, or a vector specifying the breakpoints to use
#' if probabilistically equivalent bin ranges are desired. Equivalent
#' ranges can be calculated using \code{\link{brkpts}}.
#' @return Returns a two-way table of frequencies in the form:
#' \if{html}{
#'   \tabular{ccccc}{
#'        \tab     \tab Fr  \tab    \tab      \cr
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
#'   }
#' }
#' @details xt requires an array or data frame of numeric values and returns
#' a two-way table of frequencies that can be optionally grouped into bins
#' of a specified range.
#' @examples
#' data(transitions)                # Load example data
#' b <- brkpts(transitions$phenofr, # 10 probabilistically
#'             10)                  #  equivalent breakpoints
#' m <- xt(transitions, fr.col=2,   # Construct a two-way table
#'         to.col=3, cnt.col=4,
#'         brk=b)
#' @author Bjorn J. Brooks, Lars Y. Pomara, Danny C. Lee
#' @references PAPER TITLE.
#' @export

xt <- function (trn,fr.col,to.col,cnt.col=NULL,brk=NULL) {
  NR <- nrow(trn)                                # Num of rows
  NC <- ncol(trn)                                # Num of columns

  if (NR > 1 & NC > 2 &                          # If all args specified...
      length(c(fr.col,to.col)) == 2 &
      is.null(cnt.col) == FALSE & is.null(brk) == FALSE) {

    trn2 <- aggregate(trn[,cnt.col],             # Sum fr.col counts into bins
                      by=list(fr=cut(trn[,fr.col],
				     brk, include.lowest=TRUE),
			      to=trn[,to.col]),
		      FUN=sum, na.rm=TRUE)

    colnames(trn2)[3] <- 'count'                 # Set column name

    trn2 <- aggregate(trn2$count,                # Sum to.col counts into bins
                      by=list(to=cut(trn2$to,
				     brk, include.lowest=TRUE),
			      fr=trn2$fr),
		      FUN=sum, na.rm=TRUE)

    colnames(trn2)[3] <- 'count'                 # Set column name

  ### Else if no count data then assume each row indicates a count of 1
  } else if (NR > 1 & NC > 2 &
             length(c(fr.col,to.col)) == 2 &
             is.null(cnt.col) == TRUE & is.null(brk) == FALSE) {

    trn$count <- rep(1,NR)                       # Assign cnt of 1 for each row

    trn2 <- aggregate(trn$count,                 # Sum fr.col counts into bins
		      by=list(fr=cut(trn[,fr.col],
				     brk, include.lowest=TRUE),
			      to=trn[,to.col]),
		      FUN=sum,na.rm=TRUE)

    colnames(trn2)[3] <- 'count'                 # Set column name

    trn2 <- aggregate(trn2$count,                # Sum to.col counts into bins
		      by=list(to=cut(trn2$to,
				     brk,include.lowest=TRUE),
			      fr=trn2$fr),
		      FUN=sum,na.rm=TRUE)

    colnames(trn2)[3] <- 'count'                 # Set column name

  ### Else if no breakpoints then do not aggregate into bins. Tabulate
  ###  along all variable values.
  } else if (NR > 1 & NC > 2 &
             length(c(fr.col,to.col)) == 2 &
             is.null(cnt.col) == FALSE & is.null(brk) == TRUE) {

    trn2 <- data.frame(fr=trn[,fr.col],          # Reform into
		       to=trn[,to.col],          #  data frame
		       count=trn[,cnt.col])

  # Else if no count data and no breakspoints then assume each row
  # indicates a count of 1 and do not aggregate into bins
  } else if (NR > 1 & NC >= 2 &
             length(c(fr.col,to.col,cnt.col)) == 2 &
             is.null(cnt.col) == TRUE & is.null(brk) == TRUE) {

    trn2 <- data.frame(fr=trn[,fr.col],         # Reform into
		       to=trn[,to.col],         #  data frame
		       count=rep(1,NR))
  } else {

    stop('Input arguments do not conform.')

  }

  output <- xtabs(count ~ to + fr, data=trn2) # count ~ arg3(rows) + arg2(cols)

  ### Ensure that matrix has same row & column names (square matrix)
  mrNames <- setdiff(colnames(output), rownames(output)) # Missing row names
  mcNames <- setdiff(rownames(output), colnames(output)) # Missing col names
  NUMmr <- length(mrNames)                               # Num of miss rows
  NUMmc <- length(mcNames)                               # Num of miss cols
  if (NUMmr == 0 & NUMmc == 0) {

    return(output)

  } else { # If not square matrix then add row/col of zeros

    if (NUMmr > 0) {
      mr <- array(0, c(NUMmr,ncol(output)))
      output <- rbind(output, mr)                        # Add missing rows
      rownames(output)[(nrow(output)-NUMmr+1):nrow(output)] <- mrNames
    }
    if (NUMmc > 0) {
      mc <- array(0, c(nrow(output),NUMmc))
      output <- cbind(output, mc)                        # Add missing cols
      colnames(output)[(ncol(output)-NUMmc+1):ncol(output)] <- mcNames
    }

    return(output)

  }
}
#Calculate Shannon diversity
#pxy.log2 <- replace(pxy.log2, is.infinite(pxy.log2), 0) # Replace Inf with 0
#hab <- -sum(ptbl %*% log2(ptbl)) # Shannon diversity (of joint distribution)
