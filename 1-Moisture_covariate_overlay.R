
# Script to overlay the latlong BBS stratification map with
# SPEI moisture covariates
# Generates the local moisture covariates for the models

library(terra)
library(bbsBayes2)
library(sf)
library(tidyverse)
library(exactextractr)
# Download the North Atlantic Oscillation data and save to rds
# This downloaded .rds file is included in the repo
# nao <- read.fwf(file = "https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table",
#                   header = FALSE,
#                 skip = 1,
#                 widths = c(5, rep(7,12)),
#                 strip.white = TRUE,
#                 col.names = c("year",month.name)) # accessed on September 18, 2023
#saveRDS(nao,file = "data/nao.rds")


#load the strata map
strata_map <- bbsBayes2::load_map("latlong")
strata_crs <- st_crs(strata_map)


# Manual download required ------------------------------------------------
# Download the SPEI data for 03 and 15 months -----------------------------
# These files are too large for the code repo
# downloaded from https://doi.org/10.20350/digitalCSIC/15470
# download spei03.nc to directory data/too_large
# download spei15.nc to directory data/too_large

## S.M. Vicente-Serrano, S. Beguería, J.I. López-Moreno. 2010. A Multi-scalar drought index sensitive to global warming: The Standardized Precipitation Evapotranspiration Index – SPEI. Journal of Climate 23: 1696, DOI: 10.1175/2009JCLI2909.1

for(n_months in c("03","15")){
# These are the means for the preceeding n_months months "spei03" = 3 months, spei15 = previous 15 months.
# Load the previously downloaded spei data
moisture_full = terra::rast(paste0("data/too_large/spei",n_months,".nc"))

strata_map = st_transform(strata_map, st_crs(moisture_full))
moisture = terra::crop(moisture_full, strata_map)
#remove large moisture object
rm(moisture_full)

strata_moisture <- exact_extract(moisture,strata_map,"mean")

#spei data are monthly values from Jan 1901 - December 2022
spei_dates <- data.frame(col_names = names(strata_moisture),
                          month = rep(1:12,times = 122),
                          year = rep(1901:2022,each = 12))

# june 3-month spei values (april, May, June)
june_spei <- spei_dates %>%
  filter(month == 6,
         year %in% 1966:2022)


strata_june_moisture <- strata_moisture %>%
  select(., matches(june_spei$col_names))


names(strata_june_moisture) <- paste0(june_spei$year)

strata_names <- strata_map %>%
  sf::st_drop_geometry() %>%
  select(strata_name)


strata_june_moisture <- bind_cols(strata_names,strata_june_moisture)


strata_june_moisture_df <- strata_june_moisture %>%
  pivot_longer(.,all_of(as.character(1966:2022)),
               names_to = "year")

saveRDS(strata_june_moisture, paste0("data/annual_latlong_june_spei",n_months,".rds"))

saveRDS(strata_june_moisture_df, paste0("data/annual_latlong_june_spei",n_months,"_df.rds"))


}



