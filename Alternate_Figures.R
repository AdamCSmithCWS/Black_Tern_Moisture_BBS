

# some alternate figures for supplement etc. ------------------------------


library(bbsBayes2)
library(tidyverse)
library(patchwork)
library(scico) # good cbf palettes
library(sf)

ey <-2022
sy <- 1970

BLTE_gen = 5.682 # generation time for Black Tern - Bird et al. 2020
BLTE_3Gen = round(BLTE_gen*3) # Three generations to calculate COSEWIC and IUCN trend thresholds

model_palette <- viridis::viridis(6,end = 0.95)
names(model_palette) <- c("Base","Climate","Climate NAOI-lag-1-year","Climate with lag","Climate plus core",
                          "Climate plus core NAOI-lag-1-year")


# base maps of core BCR and political jurisdictions
bcr11_core <- bbsBayes2::load_map("bcr") %>%
  filter(strata_name == "BCR11")
political <- bbsBayes2::load_map("prov_state")
strata_map <- bbsBayes2::load_map("latlong")

map_theme <- ggplot2::theme(line = ggplot2::element_line(linewidth = 0.4),
                            rect = ggplot2::element_rect(linewidth = 0.1),
                            text = element_text(family = "serif",
                                                size = 7))
# trend map breaks
breaks <- c(-7, -4, -2, -1, -0.5, 0.5, 1, 2, 4, 7)
labls <- c(paste0("< ", breaks[1]),
           paste0(breaks[-c(length(breaks))],":", breaks[-c(1)]),
           paste0("> ",breaks[length(breaks)]))
labls <- paste0(labls, " %")

## colours below come from scico::scico(n = 11,
#              palette = "roma")
trend_map_colours <- c("#7E1700","#974D13", "#AC7726", "#C1A443", "#D2D384", "#C0E9C2", "#88D9D7",
                       "#4BB2CE","#2C87BE", "#1E5FAC", "#023198")


strata_core <- readRDS("data/strata_w_core_indicator.rds") %>%
  mutate(core = ifelse(periphery == 1, "periphery","core"))


model <- "base"


fit <- readRDS(paste0("output/gamye_",sy,"_",ey,"_",model,".rds")) # read in the base model fit





fit_cov <- readRDS(paste0("output/gamye_",sy,"_",ey,"_2covariate_varying.rds")) # read in the covariate model fit

fit_cov_lag <- readRDS(paste0("output/gamye_",sy,"_",ey,"_3covariate_varying.rds")) # read in the covariate model fit

fit_cov_naoi1 <- readRDS(paste0("output/gamye_",sy,"_",ey,"_2covariate_varying_naoi1.rds")) # read in the covariate model fit

fit_cov_core <- readRDS(paste0("output/gamye_",sy,"_",ey,"_2covariate_varying_core.rds")) # read in the covariate model fit

fit_cov_core_naoi1 <- readRDS(paste0("output/gamye_",sy,"_",ey,"_2covariate_varying_core_naoi1.rds")) # read in the covariate model fit

cov_hypers <- get_summary(fit_cov,variables = c("BETA_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","SPEI-local","NAOI")) %>%
  mutate(model = "Climate")

cov_hypers_naoi1 <- get_summary(fit_cov_naoi1,variables = c("BETA_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","SPEI-local","NAOI")) %>%
  mutate(model = "Climate NAOI-lag-1-year")

cov_hypers_core <- get_summary(fit_cov_core,variables = c("BETA_cov","BETA_cor_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","SPEI-local","NAOI"),
         effect = ifelse(variable == "BETA_cor_cov",
                         "SPEI-core",
                         effect)) %>%
  mutate(model = "Climate plus core")

cov_hypers_core_naoi1 <- get_summary(fit_cov_core_naoi1,variables = c("BETA_cov","BETA_cor_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","SPEI-local","NAOI"),
         effect = ifelse(variable == "BETA_cor_cov",
                         "SPEI-core",
                         effect)) %>%
  mutate(model = "Climate plus core NAOI-lag-1-year")

cov_hypers2 <- get_summary(fit_cov_lag,variables = c("BETA_cov","BETA_lag_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","SPEI-local","NAOI"),
         effect = ifelse(variable == "BETA_lag_cov",
                         "SPEI-local-lagged",
                         effect)) %>%
  mutate(model = "Climate with lag")



cov_hypers_all <- bind_rows(cov_hypers,cov_hypers2,cov_hypers_naoi1,
                        cov_hypers_core,cov_hypers_core_naoi1) %>% 
  mutate(model = factor(model,
                        levels = c("Base","Climate","Climate NAOI-lag-1-year","Climate with lag","Climate plus core",
                                   "Climate plus core NAOI-lag-1-year"),
                        ordered = TRUE))


cov_effect_plot <- ggplot(data = cov_hypers_all,
                          aes(x = effect, y = mean,
                              colour = model))+
  geom_hline(yintercept = 0, colour = "grey60")+
  geom_errorbar(aes(ymin = q5, ymax = q95),
                width = 0, alpha = 0.6,
                position = position_dodge(width = 0.3))+
  geom_point(position = position_dodge(width = 0.3))+
  theme_bw()+
  map_theme +
  xlab("")+
  ylab("Estimated effect")+
  ggplot2::scale_colour_manual(values = model_palette,
                               aesthetics= c("colour","fill"),
                               na.value = "white",
                               name = "Model")
# scale_colour_viridis_d(aesthetics = c("colour","fill"),
#                        end = 0.8, name = "Model")

pdf("final_figures/Figure2_alt.pdf",
    width = 3.5,height = 2.5)
print(cov_effect_plot)
dev.off()



# 
# inds_plotting <- readRDS("results/all_annual_indices_saved.rds") %>% 
#   filter(region == "continent",
#          type != "smooth")

# Figure 5 Trend comparison with covariates -------------------------------
inds_plotting <- NULL
trends_save <- NULL
for(model in c("base",
               "2covariate_varying",
               "3covariate_varying",
               "2covariate_varying_core",
               "2covariate_varying_core_naoi1",
               "2covariate_varying_naoi1")){

    fit_temp <- readRDS(paste0("output/gamye_",sy,"_",ey,"_",model,".rds")) # read in the base model fit

  
  
  inds <- generate_indices(fit_temp,alternate_n = "n_smooth",
                           regions = "continent",
                           hpdi = TRUE)
  inds_temp <- inds$indices %>%
    mutate(model_type = model,
           index_type = "smooth")
  inds_plotting <- bind_rows(inds_plotting,
                             inds_temp)
  
  
  
  #core and periphery trajectories
  inds_core <- generate_indices(fit_temp,
                                alternate_n = "n_smooth",
                                regions = "core",
                                regions_index = strata_core,
                                hpdi = TRUE)
  inds_temp <- inds_core$indices %>%
    mutate(model_type = model,
           index_type = "smooth")
  inds_plotting <- bind_rows(inds_plotting,
                             inds_temp)
  
  
  for(sy1 in c(sy,(ey-BLTE_3Gen))){
    t_long <- generate_trends(inds,
                              min_year = sy1,
                              prob_decrease = c(0,30,50),
                              hpdi = TRUE)
    
    trends_out_t <- t_long$trends %>%
      mutate(model_type = model)
    trends_save <- bind_rows(trends_save,trends_out_t)
    
    t_long_core <- generate_trends(inds_core,
                                   min_year = sy1,
                                   prob_decrease = c(0,30,50),
                                   hpdi = TRUE)
    
    trends_out_t <- t_long_core$trends %>%
      mutate(model_type = model)
    trends_save <- bind_rows(trends_save,trends_out_t)
    
  }
  
  
  
} #end of trend and index calculations

# saveRDS(inds_plotting,
#         paste0("output/indices_df_compare_",sy,"_",ey,".rds"))
# 
# saveRDS(trends_save,
#         paste0("output/trends_df_compare_",sy,"_",ey,".rds"))
# 
# 
# 
# inds_plotting <- readRDS(paste0("output/indices_df_compare_",sy,"_",ey,".rds"))
# trends_save <- readRDS(paste0("output/trends_df_compare_",sy,"_",ey,".rds"))

model_names <- data.frame(model_type = c("base",
                                         "2covariate_varying",
                                         "2covariate_varying_naoi1",
                                         "3covariate_varying",
                                         "2covariate_varying_core",
                                         "2covariate_varying_core_naoi1"),
                          model_name = c("Base",
                                    "Climate",
                                    "Climate NAOI-lag-1-year",
                                    "Climate with lag",
                                    "Climate plus core",
                                    "Climate plus core NAOI-lag-1-year"))


trends_highlevel <- trends_save %>%
  filter(region_type %in% c("continent","core"))%>%
  mutate(span = paste0(start_year,"-",end_year),
         Region = str_to_title(region)) %>% 
  left_join(model_names)

trends_plot <- ggplot(data = trends_highlevel,
                      aes(x = span,y = trend,
                          colour = model_name))+
  geom_hline(yintercept = 0, colour = "grey60")+
  geom_errorbar(aes(ymin = trend_q_0.05, ymax = trend_q_0.95),
                width = 0, alpha = 0.6,
                position = position_dodge(width = 0.3))+
  geom_point(position = position_dodge(width = 0.3))+
  theme_bw()+
  #map_theme +
  theme(legend.position = "bottom",
        line = ggplot2::element_line(linewidth = 0.4),
        rect = ggplot2::element_rect(linewidth = 0.1),
        text = element_text(family = "serif",
                            size = 7))+
  xlab("")+
  ylab("Trend (%/year)")+
  ggplot2::scale_colour_manual(values = model_palette,
                               aesthetics= c("colour","fill"),
                               na.value = "white",
                               name = "Model")+
  facet_grid(rows = vars(Region))


trajs_highlevel1 <- inds_plotting %>%
  filter(region_type %in% c("continent"),
         year > 2004)%>%
  mutate(Region = str_to_title(region),
         time_period = "Short-term") %>% 
  left_join(model_names)

trajs_highlevel <- inds_plotting %>%
  filter(region_type %in% c("continent"))%>%
  mutate(Region = str_to_title(region),
         time_period = "Long-term") %>% 
  left_join(model_names)


traj_comp <- ggplot(data = trajs_highlevel,
                    aes(x = year,y = index))+
  geom_ribbon(aes(ymin = index_q_0.05,ymax = index_q_0.95,
                  fill = model_name),
              alpha = 0.3)+
  geom_line(aes(colour = model_name))+
  ggplot2::scale_colour_manual(values = model_palette,
                               aesthetics= c("colour","fill"),
                               na.value = "white",
                               name = "Model")+
  scale_y_continuous(transform = "log10")+
  ylab("Index of abundance")+
  theme_bw()+
  theme(legend.position = "none",
        line = ggplot2::element_line(linewidth = 0.4),
        rect = ggplot2::element_rect(linewidth = 0.1),
        text = element_text(family = "serif",
                            size = 7))+
  facet_grid(rows = vars(Region))

stack_plot <- traj_comp / trends_plot + plot_layout(heights = c(1,3))

pdf("final_figures/Figure5_alternate.pdf",
    width = 3.5,height = 5.5)
print(stack_plot)
dev.off()


#













