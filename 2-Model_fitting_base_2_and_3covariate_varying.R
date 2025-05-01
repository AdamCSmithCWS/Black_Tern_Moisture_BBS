### running bbsBayes2 spatial GAMYE model


# relies on v1.1.1 of bbsBayes2
#remotes::install_github("https://github.com/bbsBayes/bbsBayes2/releases/tag/v1.1.1")
library(bbsBayes2)
library(tidyverse)

dir.create("output") #necessary folder to save model output

# Data setup using bbsBayes2 --------------------------------------------------------------


species <- "Black Tern"
stratification <- "latlong"

model = "gamye"
model_variant <- "spatial"


strata_map <- bbsBayes2::load_map(stratification)

#only needs to be run once to download a local copy
#of the BBS database.

ey <-2022
sy <-1970

re_prepare_data <- FALSE
if(re_prepare_data){
bbsBayes2::fetch_bbs_data(release = 2023) # release that includes data up to 2022 field season
  
  s <- bbsBayes2::stratify(by = "latlong",
                           species = species,
                           release = 2023) #
  
p <- prepare_data(s,
                  min_n_routes = 1,
                  min_max_route_years = 6,
                  max_year = ey,
                  min_year = sy)

ps <- prepare_spatial(p,
                      strata_map = strata_map)

view_strata <- ps$spatial_data$map +
  geom_sf(data = bbsBayes2::load_map("bbs_usgs"),
          fill = NA)
print(view_strata)

saveRDS(ps,paste0("data/prepared_data_",sy,"-",ey,".rds"))
}


# load prepared BBS data from above loop ---------------------------------------


  ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))


# SPEI --------------------------------------------------------------------

  n_months <- 15

  cov_all <- readRDS(paste0("data/annual_latlong_june_spei",n_months,".rds"))

  strata_incl <- ps$meta_strata
  years_incl <- min(ps$raw_data$year) : max(ps$raw_data$year)
  

  cov_incl <-  strata_incl %>%
    inner_join(.,cov_all,
               by = "strata_name") %>%
    select(matches(as.character(years_incl)),
           strata) %>%
    arrange(strata) %>%
    select(-strata) %>%
    as.matrix()


  ## mean moisture in strata within core of species' range

  # ID BCR 11 strata
  #

  bcrs <- bbsBayes2::load_map("bcr") %>%
    rename(bcr = strata_name)

  core_strata <- strata_map %>%
    sf::st_join(.,bcrs,
                largest = TRUE,
                join = sf::st_covered_by) %>%
    filter(bcr == "BCR11")



  restructure_strata_w_core <- FALSE
  if(restructure_strata_w_core){
  strata_incl <- strata_incl %>%
    mutate(periphery = ifelse(strata_name %in% core_strata$strata_name,
                            0,
                            1))


  saveRDS(strata_incl,"data/strata_w_core_indicator.rds")

}else{
  strata_incl <- readRDS("data/strata_w_core_indicator.rds")
}
  
  core_strata_incl <- strata_incl %>%
    filter(strata_name %in% core_strata$strata_name)
  
  periphery_incl <- strata_incl %>%
    filter(!strata_name %in% core_strata$strata_name)
  
  
  periphery <- as.integer(strata_incl$periphery)
  core = which(periphery == 0)
  mean_cov_core <- colMeans(cov_incl[core,])
  cov_core <- matrix(as.numeric(mean_cov_core),
                     nrow = 1)



# NAOI --------------------------------------------------------------------

  ## global annual covariate
  lag_nao <- 0 #1-year lag for NAO data
  nao <- readRDS("data/nao.rds")
  nao <- nao %>%
    rowwise() %>%
    mutate(.,winter = mean(c(January:May))) %>%
    filter(year %in% c(years_incl-lag_nao)) %>%
    arrange(year)

  cov_ann <- matrix(as.numeric(nao$winter),
                    nrow = 1)




# alternate spei covariates using 3-month spei ---------------------------------




  n_months <- "03"
  lag_time_spei <- 1 # number of years for moisture covariate lag
  years_incl_lag <- c(min(ps$raw_data$year) : max(ps$raw_data$year))-lag_time_spei
  
  cov_all3 <- readRDS(paste0("data/annual_latlong_june_spei",n_months,".rds"))


  cov_incl3 <-  strata_incl %>%
    inner_join(.,cov_all3,
               by = "strata_name") %>%
    select(matches(as.character(years_incl)),
           strata) %>%
    arrange(strata) %>%
    select(-strata) %>%
    as.matrix()


  cov_lag_incl3 <-  strata_incl %>%
    inner_join(.,cov_all3,
               by = "strata_name") %>%
    select(matches(as.character(years_incl_lag)),
           strata) %>%
    arrange(strata) %>%
    select(-strata) %>%
    as.matrix()


  ## mean moisture in strata within core of species' range

  # ID BCR 11 strata
  #


  mean_cov_core3 <- colMeans(cov_incl3[core,])
  cov_core3 <- matrix(as.numeric(mean_cov_core3),
                     nrow = 1)



# Run Base model ----------------------------------------------------------
run_base <- TRUE
  

if(run_base){
  
  # This code exports the base model from bbsBayes2
  # bbsBayes2::copy_model_file(model,model_variant,
  #                            dir = "models")
  # The name of the exported .stan model file was changed
  #  to "gamye_spatial_bbs_CV_base.stan"
                              
  base_mod <- paste0("models/",model,"_spatial_bbs_CV_base.stan")
  
pm <- prepare_model(ps,
                    model = model,
                    model_variant = model_variant,
                    model_file = base_mod,
                    calculate_log_lik = TRUE) #saves observation-level log-likelihood calculations


fit <- run_model(pm,
                 refresh = 400,
                 iter_warmup = 2000,
                 iter_sampling = 4000,
                 thin = 2,
                 max_treedepth = 11,
                 adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = paste0(model,"_",sy,"_",ey,"_base"))
#parameter summary and convergence stats
summ <- get_summary(fit)
saveRDS(summ, paste0("results/summary_",model,"_",sy,"_",ey,"_base.rds"))


}





  # Covariate version of bbsBayes2 gamye spatial model -------------------------------------------------------

# this was run once to create a base file to modify
# bbsBayes2::copy_model_file(model,model_variant,
#                            dir = "models")
# then modifiations were made to four versions of the
#  .stan file to support the relevant covariates and then saved as
#  climate model
# "models/gamye_spatial_bbs_CV_year_effect_2covariate_varying.stan"
# climate plus core model
# "models/gamye_spatial_bbs_CV_year_effect_2covariate_varying_core.stan"
# climate model with lagged SPEI
# "models/gamye_spatial_bbs_CV_year_effect_3covariate_varying.stan"
# climate model with core and lagged SPEI
# "models/gamye_spatial_bbs_CV_year_effect_3covariate_varying_core.stan"
  



  ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))



# Fit climate model -----------------------------------------------

cov_mod <- paste0("models/",model,"_spatial_bbs_CV_year_effect_2covariate_varying.stan")

pm_cov <- prepare_model(ps,
                        model = model,
                        model_variant = model_variant,
                        model_file = cov_mod,
                        calculate_log_lik = TRUE)

# manually adding covariate data required by model
pm_cov$model_data[["cov"]] <- cov_incl
pm_cov$model_data[["cov_ann"]] <- cov_ann

fit_cov <- run_model(pm_cov,
                 refresh = 200,
                 iter_warmup = 2000,
                 iter_sampling = 4000,
                 thin = 2,
                 max_treedepth = 11,
                 adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = paste0(model,"_",sy,"_",ey,"_2covariate_varying"))

#parameter summary and convergence stats
summ <- get_summary(fit_cov)
saveRDS(summ, paste0("results/summary_",model,"_",sy,"_",ey,"_2covariate_varying.rds"))



# Fit climate-plus-core model ------------------------------------------
cov_mod3 <- paste0("models/",model,"_spatial_bbs_CV_year_effect_2covariate_varying_core.stan")

pm_cov3 <- prepare_model(ps,
                         model = model,
                         model_variant = model_variant,
                         model_file = cov_mod3,
                         calculate_log_lik = TRUE)

pm_cov3$model_data[["cov"]] <- cov_incl
pm_cov3$model_data[["cov_ann"]] <- cov_ann

pm_cov3$model_data[["cov_core"]] <- cov_core
pm_cov3$model_data[["periphery"]] <- periphery



fit_cov3 <- run_model(pm_cov3,
                      refresh = 200,
                      iter_warmup = 2000,
                      iter_sampling = 4000,
                      thin = 2,
                      max_treedepth = 11,
                      adapt_delta = 0.8,
                      output_dir = "output",
                      output_basename = paste0(model,"_",sy,"_",ey,"_2covariate_varying_core"))
#parameter summary and convergence stats
summ <- get_summary(fit_cov3)
saveRDS(summ, paste0("results/summary_",model,"_",sy,"_",ey,"_2covariate_varying_core.rds"))







# Fit alternate SPEI-lag model ---------------------------------------------------

cov_mod2 <- paste0("models/",model,"_spatial_bbs_CV_year_effect_3covariate_varying.stan")


pm_cov2 <- prepare_model(ps,
                         model = model,
                         model_variant = model_variant,
                         model_file = cov_mod2,
                         calculate_log_lik = TRUE)

pm_cov2$model_data[["cov"]] <- cov_incl3
pm_cov2$model_data[["cov_ann"]] <- cov_ann

pm_cov2$model_data[["cov_lag"]] <- cov_lag_incl3



fit_cov2 <- run_model(pm_cov2,
                      refresh = 200,
                      iter_warmup = 2000,
                      iter_sampling = 4000,
                      thin = 2,
                      max_treedepth = 11,
                      adapt_delta = 0.8,
                      output_dir = "output",
                      output_basename = paste0(model,"_",sy,"_",ey,"_3covariate_varying"))

#parameter summary and convergence stats
summ <- get_summary(fit_cov2)
saveRDS(summ, paste0("results/summary_",model,"_",sy,"_",ey,"_3covariate_varying.rds"))



# Fit alternate spei lag and climate-plus-core model ------------------------------------------


cov_mod4 <- paste0("models/",model,"_spatial_bbs_CV_year_effect_3covariate_varying_core.stan")


pm_cov4 <- prepare_model(ps,
                         model = model,
                         model_variant = model_variant,
                         model_file = cov_mod4,
                         calculate_log_lik = TRUE)

pm_cov4$model_data[["cov"]] <- cov_incl3
pm_cov4$model_data[["cov_ann"]] <- cov_ann
pm_cov4$model_data[["cov_lag"]] <- cov_lag_incl3

pm_cov4$model_data[["cov_core"]] <- cov_core3
pm_cov4$model_data[["periphery"]] <- periphery



fit_cov4 <- run_model(pm_cov4,
                      refresh = 200,
                      iter_warmup = 2000,
                      iter_sampling = 4000,
                      thin = 2,
                      max_treedepth = 11,
                      adapt_delta = 0.8,
                      output_dir = "output",
                      output_basename = paste0(model,"_",sy,"_",ey,"_3covariate_varying_core"))

#parameter summary and convergence stats
summ <- get_summary(fit_cov4)
saveRDS(summ, paste0("results/summary_",model,"_",sy,"_",ey,"_3covariate_varying_core.rds"))






