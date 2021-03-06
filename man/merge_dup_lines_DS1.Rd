% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_dup_lines_DS1.R
\name{merge_dup_lines_DS1}
\alias{merge_dup_lines_DS1}
\title{Merge duplicated lines from data stream 1}
\usage{
merge_dup_lines_DS1(dat_dup, cols_to_keep, rule = c("median"))
}
\arguments{
\item{dat_dup}{dataframe containing duplicated lines to be merged (could contain a single row)}

\item{cols_to_keep}{vector of column names to appear in merged dataset}

\item{rule}{string to specify how merging should be made. At the moment only "median" is supported.}
}
\value{
a single line dataframe with duplicated entries merged into one
}
\description{
This function BLABLA
}
\details{
XXX
}
\examples{

#

}
\author{
Pierre Nouvellet and Anne Cori
}
