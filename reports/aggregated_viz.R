
library(dplyr)
library(ggplot2)
library(ggthemes)
## WHO daily to weekly
incid_wide <- here::here("data/CaseCounts/processed",
                         "WHO_bycountry_wide.csv") %>%
               readr::read_csv(.)

incid_adm0 <- select(incid_wide, Date, `Sierra Leone`)
incid_weekly <- daily.to.weekly(incid_adm0)
incid_weekly$Date %<>% as.Date
incid_weekly <- filter(incid_weekly, Date > "2014-06-01")

at_300 <- here::here("output",
                     "WHO_bycountry_projections_weekly_0.9_2_300.csv") %>%
          readr::read_csv(.) %>%
          filter(Country == "Sierra.Leone")

at_500 <- here::here("output",
                     "WHO_bycountry_projections_weekly_0.9_2_500.csv") %>%
           readr::read_csv(.) %>%
           filter(Country == "Sierra.Leone")

at_250 <- here::here("output",
                     "WHO_bycountry_projections_weekly_0.9_2_250.csv") %>%
           readr::read_csv(.) %>%
           filter(Country == "Sierra.Leone")


p1 <- ggplot(incid_weekly, aes(Date, `Sierra Leone`)) + geom_point()
p1 <- p1 + geom_line(data = at_300, aes(Date, y), col = "red")
p1 <- p1 + geom_ribbon(data = at_300,
                       aes(ymin = ymin, ymax = ymax,
                           x = Date, group = 1),
                       inherit.aes = FALSE,
                       fill = "red", alpha = 0.1)


p1 <- p1 + geom_line(data = at_500, aes(Date, y), col = "blue")
p1 <- p1 + geom_ribbon(data = at_500,
                       aes(ymin = ymin, ymax = ymax,
                           x = Date, group = 1),
                       inherit.aes = FALSE,
                       fill = "blue", alpha = 0.1)

p1 <- p1 + geom_line(data = at_250, aes(Date, y), col = "green")
p1 <- p1 + geom_ribbon(data = at_250,
                       aes(ymin = ymin, ymax = ymax,
                           x = Date, group = 1),
                       inherit.aes = FALSE,
                       fill = "green", alpha = 0.1)


p1 <- p1 + theme_classic()
