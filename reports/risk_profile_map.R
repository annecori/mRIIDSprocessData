## ----setup, eval = TRUE--------------------------------------------------
library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(rgdal)

## ------------------------------------------------------------------------
drc <- here::here("data/Geography/drc_moritz/shp") %>%
    rgdal::readOGR(layer = "congo_angola")
#drc@data$id <- drc@data$NAME_2
drc.points  <- broom::tidy(drc, region = "NAME_2")
#drc.points$id <- factor(drc.points$NAME_2)
drc_df      <- left_join(drc.points, drc@data, by = c("id" = "NAME_2"))
drc_df <- filter(drc_df,
                 NAME_0 %in% "Democratic Republic of the Congo") %>%
    droplevels()
drc_df <- select(drc_df, -(sum:median_2), -X_populatio, -X_aedesmean)


## ------------------------------------------------------------------------
roc <- here::here("data/Geography/gadm36_COG_shp") %>%
    rgdal::readOGR(layer = "gadm36_COG_2")
#roc@data$id <- roc@data$NAME_2
roc.points  <- broom::tidy(roc, region = "NAME_2")
#roc.points$id <- factor(roc.points$id)
roc_df      <- left_join(roc.points, roc@data, by = c("id" = "NAME_2"))


## ------------------------------------------------------------------------
angola <- here::here("data/Geography/gadm36_AGO_shp") %>%
    rgdal::readOGR(layer = "gadm36_AGO_1")
#angola@data$id <- angola@data$NAME_1
angola.points  <- broom::tidy(angola, region = "NAME_1")
#angola.points$id <- factor(angola.points$id)
angola_df  <- left_join(angola.points, angola@data, by = c("id" =
                                                            "NAME_1"))


## ------------------------------------------------------------------------
gabon <- here::here("data/Geography/gadm36_GAB_shp") %>%
    rgdal::readOGR(layer = "gadm36_GAB_2")
#gabon@data$id <- gabon@data$NAME_2
gabon.points  <- broom::tidy(gabon, region = "NAME_2")
#gabon.points$id <- factor(gabon.points$id)
gabon_df <- left_join(gabon.points, gabon@data, by = c("id" = "NAME_2"))

## ------------------------------------------------------------------------
wtd_risk_profile <- here::here("output",
                               "importation_risk.csv") %>%
    readr::read_csv(.)

wtd_risk_profile$ci <- wtd_risk_profile$`75%` - wtd_risk_profile$`25%`
## ------------------------------------------------------------------------

clean_names <- function(n) {
    n <- tolower(n) %>%
        stringr::str_replace_all("\ ", "") %>%
        stringr::str_replace_all("-", "")

    n
}

drc_df$id <- clean_names(drc_df$id)
roc_df$id <- clean_names(roc_df$id)
angola_df$id <- clean_names(angola_df$id)
gabon_df$id <- clean_names(gabon_df$id)

idx <- which(wtd_risk_profile$flow_to == "louvakou.loubomo.")
wtd_risk_profile$flow_to[idx] <- "louvakou(loubomo)"

drc_relrisk <- filter(wtd_risk_profile,
                      adm0 == "Democratic Republic of the Congo") %>%
    left_join(drc_df, ., by = c("id" = "flow_to"))


roc_relrisk <- filter(wtd_risk_profile,
                      adm0 == "Republic of Congo") %>%
    left_join(roc_df, ., by = c("id" = "flow_to"))

angola_relrisk <- filter(wtd_risk_profile,
                      adm0 == "Angola") %>%
    left_join(angola_df, ., by = c("id" = "flow_to"))

gabon_relrisk <- filter(wtd_risk_profile,
                      adm0 == "Gabon") %>%
    left_join(gabon_df, ., by = c("id" = "flow_to"))


## ---- eval = TRUE--------------------------------------------------------
p <- ggplot() +
     geom_polygon(data = drc_relrisk,
                  aes(long, lat, group = group)) +
    geom_polygon(data = roc_relrisk, aes(long, lat, group = group)) +
    geom_polygon(data = angola_relrisk, aes(long, lat, group = group)) +
    geom_polygon(data = gabon_relrisk, aes(long, lat, group = group)) +
    geom_path(data = drc_relrisk, aes(long, lat, group = group), color = "black" ) +
    geom_path(data = roc_relrisk, aes(long, lat, group = group), color = "blue" ) +
    geom_path(data = angola_relrisk, aes(long, lat, group = group), color = "red" ) +
    geom_path(data = gabon_relrisk, aes(long, lat, group = group), color = "gray" ) +
    coord_equal() +
    theme_tufte()

here::here("output",
           stringr::str_replace(params$relrisk, ".csv", ".png")) %>%
ggsave(p)

