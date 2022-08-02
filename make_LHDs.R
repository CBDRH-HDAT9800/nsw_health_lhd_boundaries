library(tidyverse)
library(sf)
library(rmapshaper)

# these NSW LGA boundaries are from
# https://data.gov.au/dataset/ds-dga-f6a00643-1842-48cd-9c2f-df23a3a1dc1e/details

nswlgas <- read_sf("NOV21_NSW_LGA_POLYGON_shp/nsw_lga.shp")

nswlgas %>%
  glimpse()

# LGA to LHD mapping derived from pages at 
# https://www.healthstats.nsw.gov.au/#/locations
# 
lga2lhd <- read_csv("NSW_LHD_LGA_names.csv")

# join LHD mapping to LGA boundaries by LGA name
nswlgas <- nswlgas %>%
  left_join(lga2lhd, by = c("ABB_NAME" = "LGA_name")) %>%
  filter(!is.na(LHD_name))

# group by LHD and remove interior boundaries of the union
nswlhds <- nswlgas %>% 
  group_by(LHD_name) %>% 
  summarise(geometry = st_union(geometry))

# reduce the detail of the boundaries to a more manageable level
nswlhds_simplified <- ms_simplify(nswlhds, keep = 0.1, keep_shapes = TRUE)

# check all NSW - looks OK
nswlhds_simplified %>%
  ggplot(aes(fill=LHD_name)) +
    geom_sf()

# check metro - Sydney LHD not quite right, need to split 
# Canterbury-Bankstown, won't bother now
nswlhds_simplified %>%
  filter(LHD_name %in% c("Northern Sydney", "South Eastern Sydney",
                         "Sydney", "Western Sydney", "South Western Sydney",
                         "Nepean Blue Mountains", "Central Coast",
                         "Illawarra Shoalhaven")) %>%
  ggplot(aes(fill=LHD_name)) +
    geom_sf()

# see above re Sydney LHD
nswlhds_simplified %>%
  filter(LHD_name %in% c("Northern Sydney", "South Eastern Sydney",
                         "Sydney", "Western Sydney")) %>%
  ggplot(aes(fill=LHD_name)) +
    geom_sf()
