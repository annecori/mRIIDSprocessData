daily.to.weekly    <- function(daily){
    weeks  <- cut(daily$Date, breaks="1 week")
    weekly <- split(daily, weeks) %>%
                 plyr::ldply(function(d) colSums(d[, names(d) != "Date"])) %>%
                dplyr::rename(Date = .id)
    return(weekly)

}

## For each country, we want to plot the training data, the validation data
## and a polygon spanned by the 2.5% and 97.5% quantiles.
plot.weekly <- function(available, projection){
    available$Date %<>% as.Date
    p     <- ggplot(available, aes_string("Date",
                                          colnames(available)[3],
                                          color = "Category")) + geom_point(size = 0.1)
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
    p   <- p + xlab("")
    return(p)

}
