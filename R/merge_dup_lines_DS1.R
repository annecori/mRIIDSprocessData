
#' Merge duplicated lines from data stream 1
#'
#' This function BLABLA
#'
#' @export
#'
#' @author Pierre Nouvellet and Anne Cori
#'
#' @param dat_dup dataframe containing duplicated lines to be merged (could contain a single row)
#' @param cols_to_keep vector of column names to appear in merged dataset
#' @param rule string to specify how merging should be made. At the moment only "median" is supported.
#'
#' @return a single line dataframe with duplicated entries merged into one
#'
#' @details XXX
#'
#' @importFrom stats median
#'
#' @examples
#'
#' #
#'

merge_dup_lines_DS1 <- function(dat_dup, cols_to_keep, rule = c("median")) {
  rule <- match.arg(rule)

  if (!all(cols_to_keep %in% names(dat_dup))) {
    stop("dat_dup should have names containing all elements of cols_to_keep.")
  }
  if (!("healthmap_alert_id" %in% names(dat_dup))) {
    stop("dat_dup should have a column 'healthmap_alert_id'.")
  }
  if (!("cases" %in% names(dat_dup))) {
    stop("dat_dup should have a column 'Cases'.")
  }

  ### to start with, create an output based on first entry
  out <- dat_dup[1, cols_to_keep]

  if (nrow(dat_dup) > 1) # only necessary to merge duplicates if there are >1 entry
  {
    ### apply corrections to get correct answer
    # by definition location and country are the same for all entries
    # keep all HealthMap alert IDs
    if ("healthmap_alert_id" %in% cols_to_keep) {
      out$healthmap_alert_id <- paste_single_col(dat_dup$healthmap_alert_id)
    }
    # same for headline
    if ("headline" %in% cols_to_keep) {
      out$headline <- paste_single_col(dat_dup$headline)
    }
    # same for URL
    if ("url" %in% cols_to_keep) {
      out$url <- paste_single_col(dat_dup$url)
    }
    # alert_tag, same as healthmap_alert_id
    if ("alert_tag" %in% cols_to_keep) {
      out$alert_tag <- paste_single_col(dat_dup$alert_tag)
    }
    # Feed.Name, same as healthmap_alert_id
    if ("feed_name" %in% cols_to_keep) {
      out$feed_name <- paste_single_col(dat_dup$feed_name)
    }
    # For Longitude and Latitude, just checking these are the same across duplicates and issuing a warning if not
    # in any case, keeping the first of all duplicated entries
    if ("lon" %in% names(dat_dup)) {
      if (length(unique(dat_dup$lon)) > 1) {
        msg <- paste0(
          "All duplicated entries do not have same longitude. ",
          "Issue is with duplicates including HealthMap alert ID ", dat_dup$healthmap_alert_id[1]
        )
        warning(msg)
      }
    }
    if ("lat" %in% names(dat_dup)) {
      if (length(unique(dat_dup$lat)) > 1) {
        msg <- paste0(
          "All duplicated entries do not have same latitude ",
          "Issue is with duplicates including HealthMap alert ID ", dat_dup$healthmap_alert_id[1]
        )
        warning(msg)
      }
    }


    # by definition Date are the same for all entries
    # Cases: applying rule
    if (rule == "median") {
      out$cases <- median(dat_dup$cases, na.rm = TRUE)
    }
  }

  out
}
