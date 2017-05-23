# Sum with na.rm = TRUE but if all nas, return NA instead of 0
#

sum_only_na_stays_na <- function(x)
{
  if(all(is.na(x))) out <- NA else out <- sum(x, na.rm = TRUE)
  return(out)
}


# Gets appropriate case counts off dataframe
#

get_cases <- function(dat, 
                       case_type = c("SCC", "SC", "CC", "SCD", "SD", "CD"))
{
  
  case_type <- match.arg(case_type)
  
  if(case_type %in% "SCC")
  {
    col <- apply(dat[, c("SC", "CC")], 1, sum_only_na_stays_na)
  }else if(case_type %in% "SC")
  {
    col <- dat$SC
  }else if(case_type %in% "CC")
  {
    col <- dat$CC
  }else if(case_type %in% "SCD")
  {
    col <- apply(dat[, c("SD", "CD")], 1, sum_only_na_stays_na)
  }else if(case_type %in% "SD")
  {
    col <- dat$SD
  }else if(case_type %in% "CD")
  {
    col <- dat$CD
  }
  
  return(col)
}


# selects appropriate species and disease, and adds a column "Cases' with appropriate case definition. 
#

check_dat <- function(dat, 
                       spec, 
                       disease, 
                       case_type = c("SCC", "SC", "CC", "SCD", "SD", "CD"))
{
  
  ####################################
  ### add checks on input arguments ###
  ####################################
  
  case_type <- match.arg(case_type)
  
  if(!("Disease" %in% names(dat)))
  {
    stop("dat should have a column 'Disease'.")
  }
  
  if(!("Species" %in% names(dat)))
  {
    stop("dat should have a column 'Species'.")
  }
  
  if(!("Issue.Date" %in% names(dat)))
  {
    stop("dat should have a column 'Issue.Date'.")
  }
  
  if(!("Location" %in% names(dat)))
  {
    stop("dat should have a column 'Location'.")
  }
  
  if(!("Country" %in% names(dat)))
  {
    stop("dat should have a column 'Country'.")
  }
  
  ####################################
  ### select disease / species ###
  ####################################
  
  ### select only disease of interest ###
  dat <- dat[dat$Disease %in% disease,]
  
  ### select only species of interest ###
  dat <- dat[dat$Species %in% spec,]
  
  ####################################
  ### deal with dates ###
  ####################################
  
  ### create dates without time from Issue.Date
  dat$Date <- as.Date(unlist(strsplit(dat$Issue.Date, " "))[seq(1, 2*nrow(dat), 2)], format="%m/%d/%y")
  
  ####################################
  ### Create column called Cases which comprises all relevant cases to be counted in incidence ###
  ####################################
  
  dat$Cases <- get_cases(dat, case_type)
  
  return(dat)
}

# function to create a merged entry, separated by a slash
# 
paste_single_col <- function(col_dup)
{
  out <- paste(col_dup, collapse=" / ")
  return(out)
}