% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify.R
\name{setmodify}
\alias{setmodify}
\title{modifies data.table in place}
\usage{
setmodify(dat, x, ...)
}
\arguments{
\item{dat}{\code{\link[=data.table]{data.table()}} object}

\item{x}{\code{dcmodify::modifier} object.}

\item{...}{not used}
}
\description{
modifies data.table in place, alias for \code{modify} with \code{copy=TRUE} and
\code{sequential=TRUE}. It follows the naming convention in \code{data.table} to
prefix methods that change objects (byreference) with \code{set}.
}
\examples{
library(data.table)

m <- modifier( if (age > 130) age = 130
             , income[age < 12] <- 0
             )

dat <- fread(text =
"age, income
 140,  300
  11, 2000
  25, 3000"
)

# modify a copy of the data
dat_m <- modify(dat, m, copy = TRUE)
print(dat_m)

# the data it self
setmodify(dat, m)
print(dat)
}
\seealso{
Other modify: 
\code{\link{modify,data.table,modifier-method}}
}
\concept{modify}
