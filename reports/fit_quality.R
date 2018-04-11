library(magrittr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)


fitfile <- "output/healthmap_goodness_fit_150.csv"


fit_300 <- readr::read_csv(fitfile)

start <- fit_300$Date[1]
fit_300$week <- 1 + ((fit_300$Date - start) / 7)


## In week i, find the number of districts  for
## which ymin < incid < ymax

fit_300$week  %<>% factor

fit_300_weekly <- fit_300 %>%
               group_by(week) %>%
               filter(ymin <= Incidence & Incidence <= ymax) %>%
               summarise(in_ci = n()) %>%
               tidyr::complete(week, fill = list(in_ci = 0))

num_districts <- unique(fit_300$Country) %>% length
fit_300_weekly$prop <- fit_300_weekly$in_ci / num_districts


## Do the same for t = 500

fitfile <- "output/healthmap_goodness_fit_400.csv"


fit_500<- readr::read_csv(fitfile)

start <- fit_500$Date[1]
fit_500$week <- 1 + ((fit_500$Date - start) / 7)


## In week i, find the number of districts  for
## which ymin < incid < ymax

fit_500$week  %<>% factor

fit_500_weekly <- fit_500 %>%
               group_by(week) %>%
               filter(ymin <= Incidence & Incidence <= ymax) %>%
               summarise(in_ci = n()) %>%
               tidyr::complete(week, fill = list(in_ci = 0))

num_districts <- unique(fit_500$Country) %>% length
fit_500_weekly$prop <- fit_500_weekly$in_ci / num_districts

fits <- list(at_300 = fit_300_weekly, at_500 = fit_500_weekly)
fits <- bind_rows(fits, .id = "At")
fits$At %<>% factor

p <- ggplot(fits, aes(week, prop, col = At)) + geom_point()
p <- p + ylim(0, 1)
p <- p + theme_classic()
p <- p + theme(legend.title = element_blank())
p <- p + scale_colour_discrete(labels = c("At 300 days", "At 500 days"),
                               breaks = c("at_300", "at_500"))
p <- p + ylab("Proportion of districts with observed incidence in 95% CI")
