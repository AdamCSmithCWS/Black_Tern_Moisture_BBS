library(bbsBayes2)
library(tidyverse)

dir.create("output") #necessary folder to save model output

cross_validate <- TRUE #set to FALSE to run main models, TRUE to run 1o-fold cross validation for selected models
K <- 10
# Data setup using bbsBayes2 --------------------------------------------------------------

options(cmdstanr_warn_inits = FALSE)

species <- "Black Tern"
stratification <- "latlong"

model = "gamye"
model_variant <- "spatial"


strata_map <- bbsBayes2::load_map(stratification)

#only needs to be run once to download a local copy
#of the BBS database.

ey <-2022
sy <-1970



ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))



load("data/load_covariates.RData")

cov_mod <- paste0("models/",model,"_spatial_bbs_CV_year_effect_2covariate_varying_core.stan")

pm_cov <- prepare_model(ps,
                        model = model,
                        model_variant = model_variant,
                        model_file = cov_mod,
                        calculate_log_lik = FALSE,
                        calculate_cv = TRUE,
                        cv_k = K)
# manually adding covariate data required by model
pm_cov$model_data[["cov"]] <- cov_incl
pm_cov$model_data[["cov_ann"]] <- cov_ann1

pm_cov$model_data[["cov_core"]] <- cov_core
pm_cov$model_data[["periphery"]] <- periphery

for(k in 1:K){
  
  fit_tmp <- run_model(pm_cov,
                       refresh = 500,
                       iter_warmup = 1000,
                       iter_sampling = 1000,
                       thin = 1,
                       k = k,
                       init_alternate = 1,#fit_orig$model_fit,
                       max_treedepth = 11,
                       adapt_delta = 0.8,
                       output_basename = "core_naoi1")
  
  
  sum_cv <- get_summary(fit_tmp,variables = "log_lik_cv")
  
  # identifying which counts are being predicted in each fold using the
  # "test" vector in the original model data
  sum_cv <- sum_cv %>%
    mutate(original_count_index = fit_tmp$model_data$test)
  
  saveRDS(sum_cv,paste0("output/CV_",k,"_",sy,"_",ey,"_2covariate_varying_core_naoi1.rds"))
  
}
