\encoding{UTF-8}
\name{ITmetrics-package}
\alias{ITmetrics}
\docType{package}
\title{Functions for calculating Information Theoretic measures from categorical data}
\description{
A set of functions for calculating...
}
\details{
\code{ITmetrics} provides a set of functions designed to express...
}

\section{Brief overview}{
\code{ITmetrics} provides a set of functions for interpretation of ....

% Example figure 1
Example ...\cr
Figure 1. \cr
%\if{html}{\figure{tspolar.png}{options: width='600px' alt="Figure: tspolar.png"}}
%\if{latex}{\figure{tspolar.pdf}{options: width=5.56in}}

}

\author{
Danny C. Lee, Bjorn J. Brooks, Lars Y. Pomara

Maintainer: Bjorn J. Brooks <bjorn@geobabble.org>
}
\references{
The forthcoming publication will give a detailed thematic description of how these functions can be applied to satellite remote sensing data sets:

TITLE. in composition.

}
\keyword{package}
\examples{
### Example 1, Calculate projection matrix for a polygon of ~450 MODIS pixels
library(ITmetrics)
data(transitions)                       # Load example data
b <- brkpts(transitions$phenofr, # 4 probabilistically
            4)                   #  equivalent breakpoints
m <- xt(transitions, fr.col=2,   # Construct a two-way table
        to.col=3, cnt.col=4,
        brk=b)
# Each col & row of matrix, m, will contain proportionately same num vals
pxy <- jpmf(m)                      # Joint distribution
rmd <- rowSums(pxy)                 # Row marginal distribution
cmd <- colSums(pxy)                 # Column marginal distribution
r_c <- cpf(pxy,margin='p(row|col)') # Cond.probs (row | col) of matrix m
colSums(r_c)                    # Check that each column sums to 1
r_c.prj <- prjm(r_c,10^3)          # Project joint pr matrix 1,000 steps


# Test that matrix has an equivalent number of non-zero marginal sums
if (length(rmd[rmd>0]) == length(cmd[cmd>0])) {
  rmd.prj <- prjv(rmd,r_c.prj)     # Project r by the prj mtrx to get stable eq vec
}
# Compare RMD & RMD-when-projected 1,000 steps by transition matrix
rmd
rmd.prj
### Example 1, Calculate projection matrix for a polygon of ~450 MODIS pixels
library(ITmetrics)
data(transitions)                       # Load example data
b <- brkpts(transitions$phenofr, # 4 probabilistically
            4)                   #  equivalent breakpoints
m <- xt(transitions, fr.col=2,   # Construct a two-way table
        to.col=3, cnt.col=4,
        brk=b)
# Each col & row of matrix, m, will contain proportionately same num vals
pxy <- jpmf(m)                       # Joint distribution
rmd <- rowSums(pxy)                   # Row marginal distribution
cmd <- colSums(pxy)                   # Column marginal distribution
r_c <- cpf(pxy,margin='p(row|col)') # Cond.probs (row | col) of matrix m
r_c.prj <- prjm(r_c,10^3)          # Project matrix 1,000 steps

# Test that matrix has an equivalent number of non-zero marginal sums
if (length(rmd[rmd>0]) == length(cmd[cmd>0])) {
  seqv <- prjv(r_c.prj,cmd)       # Iterate cmd by the prj mtrx to get stable eq vec
}
# Compare RMD & RMD-when-projected 1,000 steps by transition matrix
rmd
seqv
# \% change in row marginal distribution when projected 1,000 steps
100*(seqv-rmd)/rmd

}

