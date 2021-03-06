% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/incidence_data_cleanup.R
\name{update_cases_column}
\alias{update_cases_column}
\title{Update the case count with a column for dates and a column
for the appropriate case definition.}
\usage{
update_cases_column(case_count, case_type = c("scc", "sc", "cc", "scd", "sd",
  "cd", "ALL"))
}
\description{
adds a column "cases" with appropriate case definition and
a column "date" with date extracted from timestamp
}
\author{
Sangeeta Bhatia
}
