library(magrittr)
library(geosphere)
africa.adm0 <- "data/Geography/GravityModel/raw/adm0_centroids.tsv" %>%
               read.csv(stringsAsFactors = FALSE, sep = "\t", header = FALSE)
names(africa.adm0) <- c("country", "id", "lon", "lat", "pop")
