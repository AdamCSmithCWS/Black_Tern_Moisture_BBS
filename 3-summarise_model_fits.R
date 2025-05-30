
# summarise base and cov fit ----------------------------------------------
library(bbsBayes2)
library(tidyverse)
library(patchwork)
library(cmdstanr)
library(loo)
BLTE_gen = 5.682 # generation time for Black Tern - Bird et al. 2020
BLTE_3Gen = round(BLTE_gen*3) # Three generations to calculate COSEWIC and IUCN trend thresholds

#strata_sel <- readRDS("output/custom_latlong_bcr_stratification.rds")
inds_save <- NULL
trends_save <- NULL
cov_spei_lag_out <- NULL
cov_spei_out <- NULL
cov_nao_out <- NULL
inds_out <- NULL


  model <- c("gamye")
# load the fitted models --------------------------------------------------

  ey <-2022
  sy <- 1970


fit_base <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_base.rds")) # read in the base model fit
fit_cov <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_2covariate_varying.rds")) # read in the covariate model fit
fit_cov_naoi1 <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_2covariate_varying_naoi1.rds")) # read in the covariate model fit
fit_cov_lag <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_3covariate_varying.rds")) # read in the covariate model fit
fit_cov_core <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_2covariate_varying_core.rds")) # read in the covariate model fit
fit_cov_core_naoi1 <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_2covariate_varying_core_naoi1.rds")) # read in the covariate model fit
fit_cov_core_spei <- readRDS(paste0("output/",model,"_",sy,"_",ey,"_1covariate_varying_core.rds")) # read in the covariate model fit


summ_base <- readRDS(paste0("results/summary_",model,"_",sy,"_",ey,"_base.rds")) %>%
  mutate(model = "base")
summ_cov <- readRDS(paste0("results/summary_",model,"_",sy,"_",ey,"_2covariate_varying.rds")) %>%
  mutate(model = "weather")
summ_cov_naoi1 <- readRDS(paste0("results/summary_",model,"_",sy,"_",ey,"_2covariate_varying_naoi1.rds")) %>%
  mutate(model = "weather")
summ_cov_lag <- readRDS(paste0("results/summary_",model,"_",sy,"_",ey,"_3covariate_varying.rds")) %>%
  mutate(model = "weather-lag")
summ_cov_core <- readRDS(paste0("results/summary_",model,"_",sy,"_",ey,"_2covariate_varying_core.rds")) %>%
  mutate(model = "weather-plus-core")
summ_cov_core_naoi1 <- readRDS(paste0("results/summary_",model,"_",sy,"_",ey,"_2covariate_varying_core_naoi1.rds")) %>%
  mutate(model = "weather-plus-core")

summ_all <- bind_rows(summ_base,
                      summ_cov,
                      summ_cov_naoi1,
                      summ_cov_lag,
                      summ_cov_core,
                      summ_cov_core_naoi1)




# Convergence check -------------------------------------------------------

max(summ_all$rhat, na.rm = TRUE)

check_conv <- summ_all %>%
  filter(rhat > 1.03,
         ess_bulk < 400) 




# model comparison calculate elpd -----------------------------------------

loo_base <- fit_base$model_fit$loo()
loo_cov <- fit_cov$model_fit$loo()
loo_cov_naoi1 <- fit_cov_naoi1$model_fit$loo()
loo_cov_lag <- fit_cov_lag$model_fit$loo()
loo_cov_core <- fit_cov_core$model_fit$loo()
loo_cov_core_naoi1 <- fit_cov_core_naoi1$model_fit$loo()
loo_cov_core_spei <- fit_cov_core_spei$model_fit$loo()


loo_comp_out <- as.data.frame(loo_compare(loo_base,loo_cov,loo_cov_naoi1,loo_cov_lag,loo_cov_core,loo_cov_core_naoi1))
loo_comp_out$model_num <- row.names(loo_comp_out)
loo_comp_out$model_sort <- as.integer(gsub(pattern = "model",replacement = "",row.names(loo_comp_out)))
loo_comp_out$model <- c("loo_base","loo_cov","loo_cov_naoi1","loo_cov_lag","loo_cov_core","loo_cov_core_naoi1")[loo_comp_out$model_sort]

loo_compare(loo_cov,loo_cov_lag,loo_cov_core)

pointwise <- fit_base$raw_data
pointwise[,"looic_base"] <- loo_base$pointwise[,"looic"]
pointwise[,"looic_cov"] <- loo_cov$pointwise[,"looic"]
pointwise[,"looic_cov_naoi1"] <- loo_cov_naoi1$pointwise[,"looic"]
pointwise[,"looic_cov_lag"] <- loo_cov_lag$pointwise[,"looic"]
pointwise[,"looic_cov_core"] <- loo_cov_core$pointwise[,"looic"]
pointwise[,"looic_cov_core_naoi1"] <- loo_cov_core_naoi1$pointwise[,"looic"]

pointwise[,"elpd_loo_base"] <- loo_base$pointwise[,"elpd_loo"]
pointwise[,"elpd_loo_cov"] <- loo_cov$pointwise[,"elpd_loo"]
pointwise[,"elpd_loo_cov_naoi1"] <- loo_cov_naoi1$pointwise[,"elpd_loo"]
pointwise[,"elpd_loo_cov_lag"] <- loo_cov_lag$pointwise[,"elpd_loo"]
pointwise[,"elpd_loo_cov_core"] <- loo_cov_core$pointwise[,"elpd_loo"]
pointwise[,"elpd_loo_cov_core_naoi1"] <- loo_cov_core_naoi1$pointwise[,"elpd_loo"]

pointwise[,"influence_pareto_k_base"] <- loo_base$pointwise[,"influence_pareto_k"]
pointwise[,"influence_pareto_k_cov"] <- loo_cov$pointwise[,"influence_pareto_k"]
pointwise[,"influence_pareto_k_cov_naoi1"] <- loo_cov_naoi1$pointwise[,"influence_pareto_k"]
pointwise[,"influence_pareto_k_cov_lag"] <- loo_cov_lag$pointwise[,"influence_pareto_k"]
pointwise[,"influence_pareto_k_cov_core"] <- loo_cov_core$pointwise[,"influence_pareto_k"]
pointwise[,"influence_pareto_k_cov_core_naoi1"] <- loo_cov_core_naoi1$pointwise[,"influence_pareto_k"]

point_trim <- pointwise %>% 
  filter(influence_pareto_k_base < 0.7,
         influence_pareto_k_cov < 0.7,
         influence_pareto_k_cov_naoi1 < 0.7,
         influence_pareto_k_cov_lag < 0.7,
         influence_pareto_k_cov_core < 0.7,
         influence_pareto_k_cov_core_naoi1 < 0.7) %>%
  mutate(elpd_diff_loo_cov_core = elpd_loo_cov_core-elpd_loo_cov_core,
         elpd_diff_loo_cov = elpd_loo_cov_core-elpd_loo_cov,
         elpd_diff_loo_cov_core_naoi1 = elpd_loo_cov_core-elpd_loo_cov_core_naoi1,
         elpd_diff_loo_cov_naoi1 = elpd_loo_cov_core-elpd_loo_cov_naoi1,
         elpd_diff_loo_cov_lag = elpd_loo_cov_core-elpd_loo_cov_lag,
         elpd_diff_loo_base = elpd_loo_cov_core-elpd_loo_base)

compare_no_k_bad <- point_trim %>% 
  summarise(looic_base = sum(looic_base),
            looic_cov = sum(looic_cov),
            looic_cov_naoi1 = sum(looic_cov_naoi1),
            looic_cov_lag = sum(looic_cov_lag),
            looic_cov_core = sum(looic_cov_core),
            looic_cov_core_naoi1 = sum(looic_cov_core_naoi1)) %>% 
  pivot_longer(cols = starts_with("looic"),
               names_to = "model",
               names_prefix = "looic_",
               values_to = "looic_no_bad_k") %>% 
  mutate(model = paste0("loo_",model)) %>% 
  arrange(looic_no_bad_k)

loo_comp_out <- loo_comp_out %>% 
  left_join(compare_no_k_bad, by = "model")

# 
# compare_elpd_no_k_bad <- point_trim %>% 
#   summarise(elpd_base = mean(elpd_diff_loo_base),
#             elpd_cov = mean(elpd_diff_loo_cov),
#             elpd_cov_naoi1 = mean(elpd_diff_loo_cov_naoi1),
#             elpd_cov_lag = mean(elpd_diff_loo_cov_lag),
#             elpd_cov_core = mean(elpd_diff_loo_cov_core),
#             elpd_cov_core_naoi1 = mean(elpd_diff_loo_cov_core_naoi1)) %>% 
#   pivot_longer(cols = starts_with("elpd"),
#                names_to = "model",
#                names_prefix = "elpd_",
#                values_to = "elpd_no_bad_k") %>% 
#   mutate(model = paste0("loo_",model)) %>% 
#   arrange(elpd_no_bad_k)
# 
# loo_comp_out <- loo_comp_out %>% 
#   left_join(compare_elpd_no_k_bad, by = "model")



write.csv(loo_comp_out,
          "results/loo_comparison.csv")



# summ <- readRDS(paste("results/summary",model,sy,ey,"2covariate_varying_lag.rds",
#                       sep = "_"))
#
# cov_eff <- summ %>%
#   filter(grepl("BETA",variable))
# # summ <- readRDS("output/convergence_parameter_summaries.rds")
# # summ %>% filter(variable == "beta_cov")

# trajectories trends and maps --------------------------------------------


  inds_cov <- generate_indices(fit_cov,alternate_n = "n_smooth")
  inds_cov_out <- inds_cov$indices %>%
    mutate(model = "covariates",
           base_model = model,
           type = "smooth")

  inds_covalt <- generate_indices(fit_cov,alternate_n = "n")
  inds_cov_out2 <- inds_covalt$indices %>%
    mutate(model = "covariates",
           base_model = model,
           type = "full")
  inds_cov_out <- bind_rows(inds_cov_out,
                            inds_cov_out2)
  inds_cov_rand <- generate_indices(fit_cov,alternate_n = "n_random")
  inds_cov_rand_out <- inds_cov_rand$indices %>%
    mutate(model = "covariates",
           base_model = model,
           type = "no_covariates")
  inds_cov_out <- bind_rows(inds_cov_out,
                            inds_cov_rand_out)


  inds_out <- bind_rows(inds_out,inds_cov_out)
  
# 3 covariate -------------------------------------------------------------



  inds_cov_lag <- generate_indices(fit_cov_lag,alternate_n = "n_smooth")
  inds_cov_lag_out <- inds_cov_lag$indices %>%
    mutate(model = "covariates_lag",
           base_model = model,
           type = "smooth")

  inds_covalt_lag <- generate_indices(fit_cov_lag,alternate_n = "n")
  inds_cov_lag_out2 <- inds_covalt_lag$indices %>%
    mutate(model = "covariates_lag",
           base_model = model,
           type = "full")
  inds_cov_lag_out <- bind_rows(inds_cov_lag_out,
                                inds_cov_lag_out2)
  inds_cov_lag_rand <- generate_indices(fit_cov_lag,alternate_n = "n_random")
  inds_cov_lag_rand_out <- inds_cov_lag_rand$indices %>%
    mutate(model = "covariates_lag",
           base_model = model,
           type = "no_covariates")
  inds_cov_lag_out <- bind_rows(inds_cov_lag_out,
                            inds_cov_lag_rand_out)


  inds_out <- bind_rows(inds_out,inds_cov_lag_out)


  # 2 covariate plus core range predictor -------------------------------------------------------------



  inds_cov_core <- generate_indices(fit_cov_core,alternate_n = "n_smooth")
  inds_cov_core_out <- inds_cov_core$indices %>%
    mutate(model = "covariates_core",
           base_model = model,
           type = "smooth")

  inds_covalt_core <- generate_indices(fit_cov_core,alternate_n = "n")
  inds_cov_core_out2 <- inds_covalt_core$indices %>%
    mutate(model = "covariates_core",
           base_model = model,
           type = "full")
  inds_cov_core_out <- bind_rows(inds_cov_core_out,
                                inds_cov_core_out2)
  
  inds_cov_core_rand <- generate_indices(fit_cov_core,alternate_n = "n_random")
  inds_cov_core_rand_out <- inds_cov_core_rand$indices %>%
    mutate(model = "covariates_core",
           base_model = model,
           type = "no_covariates")
  inds_cov_core_out <- bind_rows(inds_cov_core_out,
                                inds_cov_core_rand_out)


  inds_out <- bind_rows(inds_out,inds_cov_core_out)


  
  
  
  
  # 2 covariate incl NAOI-lag1 plus core range predictor -------------------------------------------------------------
  
  
  
  inds_cov_core_naoi1 <- generate_indices(fit_cov_core_naoi1,alternate_n = "n_smooth")
  inds_cov_core_naoi1_out <- inds_cov_core_naoi1$indices %>%
    mutate(model = "covariates_core_naoi1",
           base_model = model,
           type = "smooth")
  
  inds_covalt_core <- generate_indices(fit_cov_core_naoi1,alternate_n = "n")
  inds_cov_core_naoi1_out2 <- inds_covalt_core$indices %>%
    mutate(model = "covariates_core_naoi1",
           base_model = model,
           type = "full")
  inds_cov_core_naoi1_out <- bind_rows(inds_cov_core_naoi1_out,
                                 inds_cov_core_naoi1_out2)
  inds_cov_core_naoi1_rand <- generate_indices(fit_cov_core_naoi1,alternate_n = "n_random")
  inds_cov_core_naoi1_rand_out <- inds_cov_core_naoi1_rand$indices %>%
    mutate(model = "covariates_core_naoi1",
           base_model = model,
           type = "no_covariates")
  inds_cov_core_naoi1_out <- bind_rows(inds_cov_core_naoi1_out,
                                 inds_cov_core_naoi1_rand_out)
  
  
  inds_out <- bind_rows(inds_out,inds_cov_core_naoi1_out)
  
  
  
  
  # 2 covariate incl NAOI-lag1 -------------------------------------------------------------
  
  
  
  inds_cov_naoi1 <- generate_indices(fit_cov_naoi1,alternate_n = "n_smooth")
  inds_cov_naoi1_out <- inds_cov_naoi1$indices %>%
    mutate(model = "covariates_naoi1",
           base_model = model,
           type = "smooth")
  
  inds_cov_naoi1alt <- generate_indices(fit_cov_naoi1,alternate_n = "n")
  inds_cov_naoi1_out2 <- inds_cov_naoi1alt$indices %>%
    mutate(model = "covariates_naoi1",
           base_model = model,
           type = "full")
  inds_cov_naoi1_out <- bind_rows(inds_cov_naoi1_out,
                            inds_cov_naoi1_out2)
  inds_cov_naoi1_rand <- generate_indices(fit_cov_naoi1,alternate_n = "n_random")
  inds_cov_naoi1_rand_out <- inds_cov_naoi1_rand$indices %>%
    mutate(model = "covariates_naoi1",
           base_model = model,
           type = "no_covariates")
  inds_cov_naoi1_out <- bind_rows(inds_cov_naoi1_out,
                            inds_cov_naoi1_rand_out)
  
  
  inds_out <- bind_rows(inds_out,inds_cov_naoi1_out)
  
  
  
# base model --------------------------------------------------------------


   inds <- generate_indices(fit_base,alternate_n = "n_smooth")

  inds_outt <- inds$indices %>%
    mutate(model = "base",
           base_model = model,
           type = "smooth")

  inds_out <- bind_rows(inds_out,
                        inds_outt)

  indsf <- generate_indices(fit_base)

  inds_outf <- indsf$indices %>%
    mutate(model = "base",
           base_model = model,
           type = "full")

  inds_out <- bind_rows(inds_out,
                        inds_outf)


  
  
  
  
  
  
  
  saveRDS(inds_out,"results/all_annual_indices_saved.rds")
  
  
  inds_plot_cont <- inds_out %>%
  filter(region == "continent",
         type != "smooth")

traj_t <- ggplot(data = inds_plot_cont,
                 aes(x = year,y = index))+
  geom_ribbon(aes(x = year,y = index,
                  ymin = index_q_0.05,
                  ymax = index_q_0.95,
                  fill = type),
              alpha = 0.25)+
  geom_line(aes(colour = type))+
  scale_colour_viridis_d(aesthetics = c("fill","colour"))+
  facet_wrap(vars(model))

#if(model == "gamye"){
  inds_plot_cont <- inds_out %>%
    filter(region == "continent",
           type == "smooth")
trajsmooth <- ggplot(data = inds_plot_cont,
                     aes(x = year,y = index))+
  geom_ribbon(aes(x = year,y = index,
                  ymin = index_q_0.05,
                  ymax = index_q_0.95,
                  fill = model),
              alpha = 0.25)+
  geom_line(aes(colour = model))+
  scale_colour_viridis_d(aesthetics = c("fill","colour"))

traj_tfinal <- traj_t / trajsmooth
#}else{
#   traj_tfinal <- traj_t
# }
pdf(paste0("Figures/trajectories_",model,"_",sy,"_",ey,".pdf"),
    width = 11,
    height = 8.5)
print(traj_tfinal)
dev.off()





# Trends estimates --------------------------------------------------------

trends_cov <- generate_trends(inds_cov, min_year = sy,
                              prob_decrease = c(0,30,50))

trends_out <- trends_cov$trends %>%
  mutate(type = "long-term",
         base_model = model,
         model = "covariates")


trends_cov_3gen <- generate_trends(inds_cov, min_year = ey-BLTE_3Gen,
                                   prob_decrease = c(0,30,50))

trendt <- trends_cov_3gen$trends %>%
  mutate(type = "three-generation",
         base_model = model,
         model = "covariates")

trends_out <- bind_rows(trends_out,trendt)


# trends cov-naoi1 --------------------------------------------------------



trends_cov_naoi1 <- generate_trends(inds_cov_naoi1, min_year = sy,
                              prob_decrease = c(0,30,50))

trendt <- trends_cov_naoi1$trends %>%
  mutate(type = "long-term",
         base_model = model,
         model = "covariates_naoi1")

trends_out <- bind_rows(trends_out,trendt)

trends_cov_3gen_naoi1 <- generate_trends(inds_cov_naoi1, min_year = ey-BLTE_3Gen,
                                   prob_decrease = c(0,30,50))

trendt <- trends_cov_3gen_naoi1$trends %>%
  mutate(type = "three-generation",
         base_model = model,
         model = "covariates_naoi1")

trends_out <- bind_rows(trends_out,trendt)


# trends covariate lag ----------------------------------------------------



trends_cov_lag <- generate_trends(inds_cov_lag, min_year = sy,
                              prob_decrease = c(0,30,50))

trendt <- trends_cov_lag$trends %>%
  mutate(type = "long-term",
         base_model = model,
         model = "covariates_lag")

trends_out <- bind_rows(trends_out,trendt)

trends_cov_lag_3gen <- generate_trends(inds_cov_lag, min_year = ey-BLTE_3Gen,
                                   prob_decrease = c(0,30,50))
trendt <- trends_cov_lag_3gen$trends %>%
  mutate(type = "three-generation",
         base_model = model,
         model = "covariates_lag")

trends_out <- bind_rows(trends_out,trendt)




# trends base -------------------------------------------------------------

trends <- generate_trends(inds, min_year = sy,
                          prob_decrease = c(0,30,50))
trendst <- trends$trends %>%
  mutate(type = "long-term",
         base_model = model,
         model = "base")
trends_out <- bind_rows(trends_out,trendst)

trends_3gen <- generate_trends(inds, min_year = ey-BLTE_3Gen,
                               prob_decrease = c(0,30,50))

trendst <- trends_3gen$trends %>%
  mutate(type = "three-generation",
         base_model = model,
         model = "base")
trends_out <- bind_rows(trends_out,trendst)







# trends covariate w core -------------------------------------------------


trends_cov_core <- generate_trends(inds_cov_core, min_year = sy,
                                  prob_decrease = c(0,30,50))

trendt <- trends_cov_core$trends %>%
  mutate(type = "long-term",
         base_model = model,
         model = "covariates_core")

trends_out <- bind_rows(trends_out,trendt)

trends_cov_core_3gen <- generate_trends(inds_cov_core, min_year = ey-BLTE_3Gen,
                                       prob_decrease = c(0,30,50))
trendt <- trends_cov_core_3gen$trends %>%
  mutate(type = "three-generation",
         base_model = model,
         model = "covariates_core")

trends_out <- bind_rows(trends_out,trendt)






# trends covariate w core naoi1 -------------------------------------------------


trends_cov_core_naoi1 <- generate_trends(inds_cov_core_naoi1, min_year = sy,
                                   prob_decrease = c(0,30,50))

trendt <- trends_cov_core_naoi1$trends %>%
  mutate(type = "long-term",
         base_model = model,
         model = "covariates_core_naoi1")

trends_out <- bind_rows(trends_out,trendt)

trends_cov_core_3gen_naoi1 <- generate_trends(inds_cov_core_naoi1, min_year = ey-BLTE_3Gen,
                                        prob_decrease = c(0,30,50))
trendt <- trends_cov_core_3gen_naoi1$trends %>%
  mutate(type = "three-generation",
         base_model = model,
         model = "covariates_core_naoi1")

trends_out <- bind_rows(trends_out,trendt)




strata_incl <- readRDS("data/strata_w_core_indicator.rds") %>%
  select(strata_name,periphery)

trends_out <- trends_out %>%
  full_join(.,strata_incl,
            by = c("region" = "strata_name"))

saveRDS(trends_out,"all_trend_estimates.rds")
## plot continental trends for long-term and three generation
##
model_names <- data.frame(model = c("covariates_core",
                                    "covariates",
                                    "base"),
                          Model = c("SPEI - NAOI - Core SPEI",
                                    "SPEI - NAOI",
                                    "Base"))
trends_cont <- trends_out %>%
  filter(region == "continent",
         model != "covariates_lag",
         !model %in% c("covariates_naoi1","covariates_core_naoi1")) %>%
  left_join(.,model_names,
            by = "model") %>%
  mutate(Time = type)

trend_plot <- ggplot(data = trends_cont,
                     aes(x = Model,y = trend,
                         colour = Time,
                         group = Time))+
  geom_errorbar(aes(ymin = trend_q_0.05,
                    ymax = trend_q_0.95),
                alpha = 0.3,
                width = 0,
                position = position_dodge(width = 0.2))+
  geom_point(position = position_dodge(width = 0.2))+
  geom_hline(yintercept = 0)+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "bottom")

pdf("figures/trends_by_model.pdf")
print(trend_plot)
dev.off()
## consider calculating a core and periphery trend using alternate regions in strata_incl
##
##
# Trend plot estimates ----------------------------------------------------






# Trend maps --------------------------------------------------------------
#
# map_cov_long <- plot_map(trends,
#                          strata_custom = strata_sel)
# map_cov_3gen <- plot_map(trends_cov_3gen,
#                          strata_custom = strata_sel)
# map_long <- plot_map(trends,
#                      strata_custom = strata_sel)
# map_3gen <- plot_map(trends_3gen,
#                      strata_custom = strata_sel)

# load original data
ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))

base_map <- load_map(ps$meta_data$stratify_by) %>%
  select(-area_sq_km) %>%
  inner_join(ps$meta_strata,
             by = "strata_name")

bbox <- sf::st_bbox(base_map)


map_cov_long <- plot_map(trends_cov) +
  labs(subtitle = "Covariate long-term trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])
map_cov_3gen <- plot_map(trends_cov_3gen)+
  labs(subtitle = "Covariate three-generation trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])


map_cov_lag_long <- plot_map(trends_cov_lag) +
  labs(subtitle = "Covariate long-term trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])
map_cov_lag_3gen <- plot_map(trends_cov_lag_3gen)+
  labs(subtitle = "Covariate three-generation trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])




map_cov_core_long <- plot_map(trends_cov_core) +
  labs(subtitle = "Covariate w core long-term trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])
map_cov_core_3gen <- plot_map(trends_cov_core_3gen)+
  labs(subtitle = "Covariate w core three-generation trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])


map_long <- plot_map(trends)+
  labs(subtitle = "Base long-term trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])
map_3gen <- plot_map(trends_3gen)+
  labs(subtitle = "Base three-generation trends")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])



all_maps <- map_long + map_cov_long + map_cov_core_long + map_3gen + map_cov_3gen + map_cov_core_3gen + plot_layout(ncol = 3,
                                                                            nrow = 2,
                                                                            byrow = TRUE,
                                                                            guides = "collect")

pdf(paste0("figures/trend_maps_varying_covariate",model,"_",sy,"_",ey,".pdf"),
    width = 11,height = 8.5)
print(all_maps)
dev.off()






# mapping abundance -------------------------------------------------------


abund_overall <- plot_map(trends_cov_core_3gen,
                          alternate_column = "rel_abundance")

abund_overall
# mapping covariates ------------------------------------------------------

cov_spei <- get_summary(fit_cov,variables = "beta_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         base_model = model,
         predictor = "SPEI")

cov_spei_out <- bind_rows(cov_spei_out,
                          cov_spei)

strata_map <- load_map("bbs_usgs") %>% 
  select(-bcr)

map_spei <- base_map %>%
  inner_join(.,cov_spei)


spei_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_spei,
          aes(fill = mean))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of spring SPEI on annual abundance",model,"_",sy,"-",ey))+
  theme_bw()

print(spei_map)


cov_nao <- get_summary(fit_cov,variables = "beta_ann_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         base_model = model,
         predictor = "NAOI")

cov_nao_out <- bind_rows(cov_nao_out,
                         cov_nao)

map_nao <- base_map %>%
  inner_join(.,cov_nao,
             by = "strata")


nao_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_nao,
          aes(fill = mean))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of NAO on annual abundance",model,"_",sy,"-",ey))+
  theme_bw()

print(nao_map)

pdf(paste0("Figures/Spatial variation in covariate effects ",model,"_",sy,"-",ey,".pdf"),
    width = 11,
    height = 8.5)
print(spei_map + nao_map)
dev.off()


# #SPEI after core maps ----------------------------------------------------------


cov_spei_after_core <- get_summary(fit_cov_core,variables = "beta_cov") %>%
  mutate(strata = row_number(),
         model = "covariate",
         base_model = model,
         predictor = "SPEI_after_core")

cov_spei_out <- bind_rows(cov_spei_out,
                          cov_spei_after_core)


map_spei_after_core <- base_map %>%
  inner_join(.,cov_spei_after_core)


spei_after_core_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_spei_after_core,
          aes(fill = mean))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of spring SPEI on annual abundance after SPEI lag",model,"_",sy,"-",ey))+
  theme_bw()

print(spei_after_core_map)




cov_nao_core <- get_summary(fit_cov_core,variables = "beta_ann_cov") %>%
  mutate(strata = row_number(),
         model = "covariate after core",
         base_model = model,
         predictor = "NAOI after core")


cov_BETA_nao <- get_summary(fit_cov_core,variables = "BETA_ann_cov") %>%
  mutate(model = "covariate after core",
         base_model = model,
         predictor = "NAOI after core")


cov_BETA_core <- get_summary(fit_cov_core,variables = "BETA_cor_cov") %>%
  mutate(model = "covariate after core",
         base_model = model,
         predictor = "core moisture")


paste("difference between wet year and dry year in core creates a",
      round(100*(exp(cov_BETA_core$mean*2)-1)),"% difference in abundance in the periphery")



cov_nao_out <- bind_rows(cov_nao_out,
                         cov_nao_core)

map_nao_core <- base_map %>%
  inner_join(.,cov_nao_core,
             by = "strata")


nao_map_core <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white")+
  geom_sf(data = map_nao_core,
          aes(fill = mean))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of NAO after core",model,"_",sy,"-",ey))+
  theme_bw()

print(nao_map_core)


#
# cov_spei_lag <- get_summary(fit_cov_lag,variables = "beta_lag_cov") %>%
#   mutate(strata = row_number(),
#          model = "covariate",
#          base_model = model,
#          predictor = "SPEI_lag")
#
# cov_spei_out <- bind_rows(cov_spei_out,
#                           cov_spei_lag)
#
#
# map_spei_lag <- base_map %>%
#   inner_join(.,cov_spei_lag)
#
#
# spei_lag_map <- ggplot()+
#   geom_sf(data = strata_map,
#           fill = "white")+
#   geom_sf(data = map_spei_lag,
#           aes(fill = mean))+
#   colorspace::scale_fill_continuous_diverging(rev = TRUE,
#                                               palette = "Blue-Red 3")+
#   coord_sf(xlim = bbox[c("xmin","xmax")],
#            ylim = bbox[c("ymin","ymax")])+
#   labs(title = paste0("Effect of lagged spring SPEI on annual abundance ",model,"_",sy,"-",ey))+
#   theme_bw()
#
# print(spei_lag_map)
#

bcrs <- bbsBayes2::load_map("bcr") %>%
  rename(bcr = strata_name)

core_strata <- strata_map %>%
  sf::st_join(.,bcrs,
              largest = TRUE,
              join = sf::st_covered_by) %>%
  filter(bcr == "BCR11")

core <- bcrs %>% filter(bcr == "BCR11")

# overall spei map --------------------------------------------------------

spei_names <- data.frame(predictor = c("SPEI","SPEI_after_core"),
                         Predictor = c("SPEI 15 Months",
                                       "SPEI 15 Months after core"))

map_spei_all <- base_map %>%
  inner_join(.,cov_spei_out,
             by = "strata") %>%
  inner_join(.,spei_names,
             by = "predictor")


spei_all_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white",
          colour = grey(0.7))+
  geom_sf(data = map_spei_all,
          aes(fill = mean))+
  geom_sf(data = core,
          fill = NA,
          colour = "black")+
  # geom_sf(data = map_spei_all,
  #         aes(fill = q5))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of SPEI on annual abundance ",model,"_",sy,"-",ey))+
  theme_bw()+
  facet_grid(rows = vars(Predictor))



spei_all_map_q5 <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white",
          colour = grey(0.7))+
  geom_sf(data = map_spei_all,
          aes(fill = q5))+
  geom_sf(data = core,
          fill = NA,
          colour = "black")+
  # geom_sf(data = map_spei_all,
  #         aes(fill = q5))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Lower CL Effect of SPEI on annual abundance ",model,"_",sy,"-",ey))+
  theme_bw()+
  facet_grid(rows = vars(Predictor))



spei_all_map_q95 <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white",
          colour = grey(0.7))+
  geom_sf(data = map_spei_all,
          aes(fill = q95))+
  geom_sf(data = core,
          fill = NA,
          colour = "black")+
  # geom_sf(data = map_spei_all,
  #         aes(fill = q5))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Upper CL Effect of SPEI on annual abundance ",model,"_",sy,"-",ey))+
  theme_bw()+
  facet_grid(rows = vars(Predictor))


pdf(paste0("Figures/Spatial variation in SPEI effects by two models ",model,"_",sy,"-",ey,".pdf"),
height = 11,
width = 8.5)

print(spei_all_map)
print(spei_all_map_q5/ spei_all_map_q95)
dev.off()




# overall NAO map ---------------------------------------------------------


nao_names <- data.frame(predictor = c("NAOI","NAOI after core"),
                         Predictor = c("NAOI preceding winter",
                                       "NAOI preceding winter after core"))

map_nao_all <- base_map %>%
  inner_join(.,cov_nao_out,
             by = "strata") %>%
  inner_join(.,nao_names,
             by = "predictor")


nao_all_map <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white",
          colour = grey(0.7))+
  geom_sf(data = map_nao_all,
          aes(fill = mean))+
  geom_sf(data = core,
          fill = NA,
          colour = "black")+
  # geom_sf(data = map_nao_all,
  #         aes(fill = q5))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Effect of NAOI on annual abundance ",model,"_",sy,"-",ey))+
  theme_bw()+
  facet_grid(rows = vars(Predictor))


nao_all_map_q5 <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white",
          colour = grey(0.7))+
  geom_sf(data = map_nao_all,
          aes(fill = q5))+
  geom_sf(data = core,
          fill = NA,
          colour = "black")+
  # geom_sf(data = map_nao_all,
  #         aes(fill = q5))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Lower CL of NAOI on annual abundance ",model,"_",sy,"-",ey))+
  theme_bw()+
  facet_grid(rows = vars(Predictor))



nao_all_map_q95 <- ggplot()+
  geom_sf(data = strata_map,
          fill = "white",
          colour = grey(0.7))+
  geom_sf(data = map_nao_all,
          aes(fill = q95))+
  geom_sf(data = core,
          fill = NA,
          colour = "black")+
  # geom_sf(data = map_nao_all,
  #         aes(fill = q5))+
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red 3")+
  coord_sf(xlim = bbox[c("xmin","xmax")],
           ylim = bbox[c("ymin","ymax")])+
  labs(title = paste0("Upper CL of NAOI on annual abundance ",model,"_",sy,"-",ey))+
  theme_bw()+
  facet_grid(rows = vars(Predictor))


pdf(paste0("Figures/Spatial variation in NAOI effects by two models ",model,"_",sy,"-",ey,".pdf"),
    height = 11,
    width = 8.5)


print(nao_all_map)
print(nao_all_map_q5 / nao_all_map_q95)
dev.off()








# cov_nao <- get_summary(fit_cov_core,variables = "beta_ann_cov") %>%
#   mutate(strata = row_number(),
#          model = "covariate",
#          base_model = model,
#          predictor = "NAOI after core")
#
# cov_nao_out <- bind_rows(cov_nao_out,
#                          cov_nao)
#
# map_nao <- base_map %>%
#   inner_join(.,cov_nao,
#              by = "strata")
#
#
# nao_map <- ggplot()+
#   geom_sf(data = strata_map,
#           fill = "white")+
#   geom_sf(data = map_nao,
#           aes(fill = mean))+
#   colorspace::scale_fill_continuous_diverging(rev = TRUE,
#                                               palette = "Blue-Red 3")+
#   coord_sf(xlim = bbox[c("xmin","xmax")],
#            ylim = bbox[c("ymin","ymax")])+
#   labs(title = paste0("Effect of NAO on annual abundance",model,"_",sy,"-",ey))+
#   theme_bw()
#
# print(nao_map)

print_map <- spei_all_map + nao_all_map
pdf(paste0("Figures/Spatial variation in covariate effects w core",model,"_",sy,"-",ey,".pdf"),
    height = 11,
    width = 8.5)
print(print_map)
dev.off()





# Centroid of distribution ------------------------------------------------

# load strata-map with strata areas and centroids

#centroids
cent_base_map <- base_map %>%
  sf::st_centroid() %>%
  sf::st_set_crs(sf::st_crs(base_map))

centroids <- cent_base_map%>%
  sf::st_coordinates()

cent_base_map <- cent_base_map %>%
  mutate(x_coord = centroids[,1],
         y_coord = centroids[,2]) %>%
  sf::st_drop_geometry()
# generate annual mean x-coordinate and annual mean y-coordinate
#

inds_full <- inds_out %>%
  filter(.,type == "full") %>%
  inner_join(.,cent_base_map,
            by = c("region" = "strata_name"))


weighted_center_coord <- function(coord,area,index){

  w <- area*index
  coord1 <- sum((coord*w)/sum(w))
  return(coord1)
}

population_centers <- inds_full %>%
  group_by(year, model) %>%
  summarise(mean_x = weighted_center_coord(x_coord,
                                           area_sq_km,
                                           index),
            mean_y = weighted_center_coord(y_coord,
                                           area_sq_km,
                                           index))


# reload covariates -------------------------------------------------------
lag_time_spei <- 0
cov_all <- readRDS("data/annual_latlong_june_spei15.rds")

strata_incl <- ps$meta_strata
years_incl <- min(ps$raw_data$year) : max(ps$raw_data$year)
years_incl_lag <- c(min(ps$raw_data$year) : max(ps$raw_data$year))-lag_time_spei

#mean overall spei
cov_incl <-  strata_incl %>%
  inner_join(.,cov_all,
             by = "strata_name") %>%
  select(strata_name,
         matches(as.character(years_incl))) %>%
  pivot_longer(.,cols = matches(as.character(years_incl)),
               names_to = "year") %>%
  rename(spei = value) %>%
  group_by(year) %>%
  summarise(spei = mean(spei)) %>%
  mutate(year = as.integer(year))


cov_lag_incl <-  strata_incl %>%
  inner_join(.,cov_all,
             by = "strata_name") %>%
  select(strata_name,
         matches(as.character(years_incl_lag))) %>%
  pivot_longer(.,cols = matches(as.character(years_incl_lag)),
               names_to = "year") %>%
  mutate(year = as.integer(year),
         year = year+lag_time_spei)%>%
  rename(spei_lag = value)%>%
  group_by(year) %>%
  summarise(spei_lag = mean(spei_lag))

## global annual covariate
lag_nao <- 0 #1-year lag for NAO data
nao <- readRDS("data/nao.rds")
nao <- nao %>%
  rowwise() %>%
  mutate(.,winter = mean(c(January:May))) %>%
  filter(year %in% c(years_incl-lag_nao)) %>%
  arrange(year) %>%
  mutate(year = year+lag_nao) %>%
  rename(nao = winter)

covariates_all <- cov_incl %>%
  inner_join(.,cov_lag_incl,
             by = c("year")) %>%
  inner_join(.,nao,
             by = c("year"))

# pairs(covariates_all[,c("spei","spei_lag","nao")])

population_centers <- population_centers %>%
  left_join(.,covariates_all,
            by = "year")


northing_plot <- ggplot(data = population_centers) +
  geom_point(aes(x = spei_lag,y = mean_y, colour = model))
northing_plot

easting_plot <- ggplot(data = population_centers) +
  geom_point(aes(x = spei,y = mean_x, colour = model))
easting_plot


centre_map <- population_centers %>%
  #filter(year > 1985) %>%
  sf::st_as_sf(.,coords = c("mean_x","mean_y"),
               crs = sf::st_crs(base_map))

centre_map_spei <- ggplot()+
  #geom_sf(data = base_map)+
  geom_sf(data = centre_map,
          aes(colour = spei))+
  labs(title = "SPEI")
centre_map_scope <- ggplot()+
  geom_sf(data = base_map)+
  geom_sf(data = centre_map)+
  labs(title = "With range for scale")
centre_map_spei_lag <- ggplot()+
  #geom_sf(data = base_map)+
  geom_sf(data = centre_map,
          aes(colour = spei_lag))+
  labs(title = "SPEI_lag")
centre_map_nao <- ggplot()+
  #geom_sf(data = base_map)+
  geom_sf(data = centre_map,
          aes(colour = nao))+
  labs(title = "NAO")

centre_map_time <- ggplot()+
  #geom_sf(data = base_map)+
  geom_sf(data = centre_map,
          aes(colour = year))+
  labs(title = "Year")


map_view <- centre_map_spei + centre_map_nao + centre_map_spei_lag + centre_map_time + centre_map_scope +
  plot_layout(ncol = 2, nrow = 3, byrow = TRUE)
map_view

pdf("Figures/centroid_movements_w_lag_allyears.pdf",
    height = 11,
    width = 8.5)

print(map_view)
dev.off()












