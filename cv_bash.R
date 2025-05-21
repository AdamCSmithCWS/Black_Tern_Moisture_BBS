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





load("data/load_covariates.RData")

cov_mod <- paste0("models/",model,"_spatial_bbs_CV_base.stan")

#ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))
# pm_cov <- prepare_model(ps,
#                         model = model,
#                         model_variant = model_variant,
#                         model_file = cov_mod,
#                         calculate_log_lik = FALSE,
#                         calculate_cv = TRUE,
#                         cv_k = K,
#                         cv_fold_groups = "route",
#                         cv_omit_singles = FALSE)
#
# replacing folds with years ----------------------------------------------
# n_groups <- pm_cov$model_data$n_years
# new_folds <- data.frame( year = c(min(pm_cov$model_data$year):max(pm_cov$model_data$year)),
#                          fold = sample(rep(1:10,length.out = n_groups)))
# 
# pm_cov$raw_data <- pm_cov$raw_data %>% 
#   left_join(new_folds,by = "year")
# pm_cov$folds <- pm_cov$raw_data$fold

# saveRDS(pm_cov,"base_cv_data.rds")

pm_cov <- readRDS("base_cv_data.rds")
pm_cov$meta_data$model_file <- cov_mod


raw <- pm_cov$raw_data %>% 
  mutate(original_count_index = row_number())
pm_cov1 <- pm_cov
pm_cov1$model_data$calc_log_lik <- 1


# full_fit <- run_model(pm_cov1,
#                       refresh = 500,
#                       iter_warmup = 1000,
#                       iter_sampling = 3000,
#                       thin = 3,
#                       init_alternate = 1,#fit_orig$model_fit,
#                       max_treedepth = 11,
#                       adapt_delta = 0.8,
#                       output_basename = "base_full",
#                       save_model = TRUE)
full_fit <- readRDS("base_full.rds")

for(k in 1:K){
  
  fit_tmp <- run_model(pm_cov,
                       refresh = 500,
                       iter_warmup = 1000,
                       iter_sampling = 1000,
                       thin = 1,
                       k = k,
                       init_alternate = full_fit$model_fit,
                       max_treedepth = 11,
                       adapt_delta = 0.8,
                       output_basename = "base",
                       save_model = FALSE)
  
  
  sum_cv <- get_summary(fit_tmp,variables = "log_lik_cv")
  
  # identifying which counts are being predicted in each fold using the
  # "test" vector in the original model data
  sum_cv <- sum_cv %>%
    mutate(original_count_index = fit_tmp$model_data$test)
  
  saveRDS(sum_cv,paste0("output/CV_",k,"_",sy,"_",ey,"_base.rds"))
  
}

do_summary <- TRUE
if(do_summary){
loo_out <- NULL
for(mod_name in c("base",
                  "core",
                  "core_naoi1",
                  "cov")){

for(k in 1:K){
  
sum_cv <- readRDS(paste0("output/CV_",k,"_",sy,"_",ey,"_",mod_name,".rds")) %>% 
    mutate(model = mod_name)

loo_out <- bind_rows(loo_out,sum_cv)
  
}
  
  
}

loo_sum <- loo_out %>% 
  group_by(model) %>% 
  summarise(sum_loo = sum(median),
            mean_loo = mean(median),
            max_rhat = max(rhat),
            mean_rhat = mean(rhat),
            min_ess = min(ess_bulk),
            mean_ess = mean(ess_bulk))

core_strat <- data.frame(strata = c(1:pm_cov$model_data$n_strata),
                         periphery = periphery)

loo_point_wise <- loo_out %>% 
  select(model,original_count_index,median) %>% 
  pivot_wider(id_cols = original_count_index,
              values_from = median,
              names_from = model,
              names_prefix = "M") %>% 
  inner_join(raw, by = "original_count_index") %>% 
  left_join(core_strat, by = "strata") %>% 
  mutate(dif_base = Mcore - Mbase,
         dif_cov = Mcore - Mcov,
         dif_naoi1 = Mcore - Mcore_naoi1,
         dif_cov_base = Mcov - Mbase) 

loo_plot <- ggplot(data = loo_point_wise,
                   aes(x = count,y = dif_base,
                       colour = year))+
  geom_point(alpha = 0.3)+
  facet_wrap(vars(periphery),scales = "free_x")

loo_plot

loo_plot <- ggplot(data = loo_point_wise,
                   aes(x = count,y = dif_base,
                       colour = year))+
  geom_point(alpha = 0.3)+
  facet_wrap(vars(periphery),scales = "free_x")

loo_plot


loo_point_summary_by_periphery <- loo_point_wise %>% 
  #filter(abs(dif_cov) < 5) %>% 
  #group_by(periphery) %>% 
  summarise(mean_dif_base = mean(dif_base),
            mean_dif_cov = mean(dif_cov),
            mean_dif_naoi1 = mean(dif_naoi1),
            mean_dif_cov_base = mean(dif_cov_base),
            
            sd_dif_base = sd(dif_base)/sqrt(nrow(loo_point_wise)),
            sd_dif_cov = sd(dif_cov)/sqrt(nrow(loo_point_wise)),
            sd_dif_naoi1 = sd(dif_naoi1)/sqrt(nrow(loo_point_wise)),
            sd_dif_cov_base = sd(dif_cov_base)/sqrt(nrow(loo_point_wise)),
            
            t_dif_base = mean_dif_base/sd_dif_base,
            t_dif_cov = mean_dif_cov/sd_dif_cov,
            t_dif_naoi1 = mean_dif_naoi1/sd_dif_naoi1,
            t_dif_cov_base = mean_dif_cov_base/sd_dif_cov_base)

write_csv(loo_point_summary_by_periphery,
          "selected_cross_validation_comparison.csv")

}

