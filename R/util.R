daily.to.weekly    <- function(daily){
    extra <- nrow(daily) %% 7
    if( extra != 0){
        warning("Number of rows is not a multiple of 7.")
        warning(paste("Ignoring last", extra, "days."))
        daily  <- utils::head(daily, -extra)
    }

    weeks  <- cut(daily$Date, breaks = "7 days")
    weekly <- split(daily, weeks) %>%
                 plyr::ldply(function(d) d %>% select(-Date) %>% colSums ) %>%
                dplyr::rename(Date = .id)
    return(weekly)

}

## For each country, we want to plot the training data, the validation data
## and a polygon spanned by the 2.5% and 97.5% quantiles.
plot.weekly <- function(available, projection){
    available$Date %<>% as.Date
    p     <- ggplot(available, aes_string("Date",
                                          colnames(available)[3],
                                          color = "Category")) + geom_point()
    ci.95 <- projection     %>%
              split(.$Date) %>%
              plyr::ldply(. %>% `[`(, 2)
                            %>% quantile(probs = c(0.5, 0.025, 0.975))) %>%
                                dplyr::rename(Date = .id)
    x <- ci.95$Date %>% c(rev(.)) %>% as.Date
    y <- c(ci.95[, 3], rev(ci.95[ , 4]))

    p   <- p + geom_polygon(data = data.frame(x = x, y = y), aes(x, y, alpha = 0.01),
                            color = "red",
                            size = 0.3,
                            linetype = "blank")
    p   <- p + geom_line(data = data.frame(x = as.Date(ci.95$Date), y = ci.95[, 2]),
                         aes(x, y), size = 0.3, color = "red", inherit.aes = FALSE)
    p   <- p + theme_minimal() + theme(legend.position="none")
    p   <- p + xlab("") + theme(axis.text.x = element_text(angle = 45, vjust = 0.2))
    return(p)

}

add_0incid <- function(df){
    df    %<>% arrange(DateOnsetInferred)
    start <- min(df$DateOnsetInferred)
    end   <- max(df$DateOnsetInferred)

    dates.all <- seq(from = start, to = end, by = "1 day")
    ndays     <- length(dates.all)
    country   <- rep(df$Country[1], ndays)
    district  <- rep(df$CL_DistrictRes[1], ndays)
    dummy     <- data.frame(DateOnsetInferred = dates.all,
                            Country = country,
                            CL_DistrictRes = district)
    df %<>% right_join(dummy)
    df$incid %<>% ifelse(is.na(.), 0, . )
    return(df)
}

rms <- function(error){
    sqrt(mean((error)^2, na.rm = TRUE))
}
