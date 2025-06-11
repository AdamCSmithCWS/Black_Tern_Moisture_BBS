

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
names(model_palette) <- c("Base",
                          "Climate",
                          "Climate [NAOI 1-year lagged]",
                          "Climate [spring + spring 1-year lagged]",
                          "Climate-plus-core",
                          "Climate-plus-core [NAOI 1-year lagged]")


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
  mutate(model = "Climate [NAOI 1-year lagged]")

cov_hypers_core <- get_summary(fit_cov_core,variables = c("BETA_cov","BETA_cor_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","SPEI-local","NAOI"),
         effect = ifelse(variable == "BETA_cor_cov",
                         "SPEI-core",
                         effect)) %>%
  mutate(model = "Climate-plus-core")

cov_hypers_core_naoi1 <- get_summary(fit_cov_core_naoi1,variables = c("BETA_cov","BETA_cor_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","SPEI-local","NAOI"),
         effect = ifelse(variable == "BETA_cor_cov",
                         "SPEI-core",
                         effect)) %>%
  mutate(model = "Climate-plus-core [NAOI 1-year lagged]")

cov_hypers2 <- get_summary(fit_cov_lag,variables = c("BETA_cov","BETA_lag_cov","BETA_ann_cov")) %>%
  mutate(effect = ifelse(variable == "BETA_cov","SPEI-local","NAOI"),
         effect = ifelse(variable == "BETA_lag_cov",
                         "SPEI-local-lagged",
                         effect)) %>%
  mutate(model = "Climate [spring + spring 1-year lagged]")



cov_hypers_all <- bind_rows(cov_hypers,cov_hypers2,cov_hypers_naoi1,
                        cov_hypers_core,cov_hypers_core_naoi1) %>% 
  mutate(model = factor(model,
                        levels = c("Base",
                                   "Climate",
                                   "Climate [NAOI 1-year lagged]",
                                   "Climate [spring + spring 1-year lagged]",
                                   "Climate-plus-core",
                                   "Climate-plus-core [NAOI 1-year lagged]"),
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

saveRDS(cov_effect_plot,"figures/Figure_s1.rds")
pdf("final_figures/Figure2_alt.pdf",
    width = 6,height = 4.5)
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
                                    "Climate [NAOI 1-year lagged]",
                                    "Climate [spring + spring 1-year lagged]",
                                    "Climate-plus-core",
                                    "Climate-plus-core [NAOI 1-year lagged]"))


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


saveRDS(stack_plot,"figures/figure_s4.rds")

pdf("final_figures/Figure5_alternate.pdf",
    width = 6,height = 5.5)
print(stack_plot)
dev.off()


#






# Figure S2 ----------------------------------------------------------------

# spatial effects of covariates from alternate models

# error manual breaks
breaks_cov <- c(-0.5, -0.25, -0.1, 0, 0.1, 0.25, 0.5)
labls_cov <- c(paste0("< ", breaks_cov[1]),
               paste0(breaks_cov[-c(length(breaks_cov))],":", breaks_cov[-c(1)]),
               paste0("> ",breaks_cov[length(breaks_cov)]))
cov_effect_colours_manual <- rev(colorspace::diverge_hcl(11,
                                                         "Blue-Red")[-c(5,6,7)])
#"#023FA5" "#5D6CAE" "#8C94BF" "#B3B7CF" "#D2B0B6" "#C18692" "#AB5468" "#8E063B"



cov_spei <- get_summary(fit_cov_lag,variables = "beta_cov") %>%
  mutate(strata = row_number(),
         model = "Climate [spring + spring 1-year lagged]",
         predictor = "SPEI")

cov_spei1 <- get_summary(fit_cov_lag,variables = "beta_lag_cov") %>%
  mutate(strata = row_number(),
         model = "Climate [spring + spring 1-year lagged]",
         predictor = "SPEI-lag1")


cov_spei_se <- cov_spei %>%
  pivot_longer(cols = c(q5,q95),
               names_to = "parameter",
               values_to = "estimate") %>%
  mutate(param = ifelse((parameter == "q5"),
                        "Lower 90% CI",
                        "Upper 90% CI"),
         qual_estimate = cut(estimate,
                             breaks = c(-Inf, breaks_cov, Inf),
                             labels = labls_cov))


cov_spei_se1 <- cov_spei1 %>%
  pivot_longer(cols = c(q5,q95),
               names_to = "parameter",
               values_to = "estimate") %>%
  mutate(param = ifelse((parameter == "q5"),
                        "Lower 90% CI",
                        "Upper 90% CI"),
         qual_estimate = cut(estimate,
                             breaks = c(-Inf, breaks_cov, Inf),
                             labels = labls_cov))



pal_cov_se <- stats::setNames(
  cov_effect_colours_manual,
  levels(cov_spei_se$qual_estimate))

# load original data
ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))

base_map <- load_map(ps$meta_data$stratify_by) %>%
  inner_join(ps$meta_strata,
             by = "strata_name")

bbox <- sf::st_bbox(base_map)


map_spei_se <- base_map %>%
  inner_join(.,cov_spei_se)

map_spei <- base_map %>%
  inner_join(.,cov_spei)



spei_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_spei,
                   ggplot2::aes(fill = .data$mean),
                   colour = "grey50", size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("Effect of SPEI")) +
  map_theme +
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red")+
  ggplot2::guides(fill = ggplot2::guide_colorbar())+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.75)+
  ggplot2::coord_sf(xlim = bbox[c("xmin","xmax")],
                    ylim = bbox[c("ymin","ymax")])


spei_map_se <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_spei_se,
                   ggplot2::aes(fill = .data$qual_estimate),
                   colour = "grey50", size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("CI limits")) +
  map_theme +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))+
  ggplot2::scale_fill_manual(values = pal_cov_se,
                             na.value = "white")+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.75)+
  ggplot2::coord_sf(xlim = bbox[c("xmin","xmax")],
                    ylim = bbox[c("ymin","ymax")])+
  facet_wrap(vars(param),
             nrow = 2)


both <- spei_map / spei_map_se






map_spei_se1 <- base_map %>%
  inner_join(.,cov_spei_se1)

map_spei1 <- base_map %>%
  inner_join(.,cov_spei1)



spei_map1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_spei1,
                   ggplot2::aes(fill = .data$mean),
                   colour = "grey50", size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("Effect of SPEI 1-year lag")) +
  map_theme +
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red")+
  ggplot2::guides(fill = ggplot2::guide_colorbar())+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.75)+
  ggplot2::coord_sf(xlim = bbox[c("xmin","xmax")],
                    ylim = bbox[c("ymin","ymax")])


spei_map_se1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_spei_se1,
                   ggplot2::aes(fill = .data$qual_estimate),
                   colour = "grey50", size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("CI limits")) +
  map_theme +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))+
  ggplot2::scale_fill_manual(values = pal_cov_se,
                             na.value = "white")+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.75)+
  ggplot2::coord_sf(xlim = bbox[c("xmin","xmax")],
                    ylim = bbox[c("ymin","ymax")])+
  facet_wrap(vars(param),
             nrow = 2)


both1 <- spei_map1 / spei_map_se1


both <- spei_map + spei_map1 + plot_layout(nrow = 2)


pdf("final_figures/Figure3_alternate.pdf",
    width = 3.5,height = 5.5)
print(both)
dev.off()

saveRDS(both,"Figures/Figure_s2.rds")


# fit_cov_core <- readRDS(paste0("output/gamye_",sy,"_",ey,"_2covariate_varying_core.rds")) # read in the covariate model fit

cov_naoi <- get_summary(fit_cov_core_naoi1,variables = "beta_ann_cov") %>%
  mutate(strata = row_number(),
         model = "Climate-plus-core [NAOI 1-year lagged]",
         predictor = "naoi")


cov_naoi_se <- cov_naoi %>%
  pivot_longer(cols = c(q5,q95),
               names_to = "parameter",
               values_to = "estimate") %>%
  mutate(param = ifelse((parameter == "q5"),
                        "Lower 90% CI",
                        "Upper 90% CI"),
         qual_estimate = cut(estimate,
                             breaks = c(-Inf, breaks_cov, Inf),
                             labels = labls_cov))




pal_cov_se <- stats::setNames(
  cov_effect_colours_manual,
  levels(cov_naoi_se$qual_estimate))

# load original data
ps <- readRDS(paste0("data/prepared_data_",sy,"-",ey,".rds"))

base_map <- load_map(ps$meta_data$stratify_by) %>%
  inner_join(ps$meta_strata,
             by = "strata_name")

bb <- sf::st_bbox(base_map)


map_naoi_se <- base_map %>%
  inner_join(.,cov_naoi_se)

map_naoi <- base_map %>%
  inner_join(.,cov_naoi)



naoi_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_naoi,
                   ggplot2::aes(fill = .data$mean),
                   colour = "grey50", size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("Effect of NAOI")) +
  map_theme +
  colorspace::scale_fill_continuous_diverging(rev = TRUE,
                                              palette = "Blue-Red")+
  ggplot2::guides(fill = ggplot2::guide_colourbar(nbin = 7))+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.75)+
  ggplot2::coord_sf(xlim = bb[c("xmin","xmax")],
                    ylim = bb[c("ymin","ymax")])


naoi_map_se <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA)  +
  ggplot2::geom_sf(data = map_naoi_se,
                   ggplot2::aes(fill = .data$qual_estimate),
                   colour = "grey50", size = 0.1) +
  ggplot2::theme_bw() +
  ggplot2::labs(fill = paste0("CI limits")) +
  map_theme +
  ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))+
  ggplot2::scale_fill_manual(values = pal_cov_se,
                             na.value = "white")+
  ggplot2::geom_sf(data = political,
                   colour = "grey70", size = 0.1,
                   fill = NA) +
  ggplot2::geom_sf(data = bcr11_core, fill = NA, linewidth = 0.75)+
  ggplot2::coord_sf(xlim = bb[c("xmin","xmax")],
                    ylim = bb[c("ymin","ymax")])+
  facet_wrap(vars(param),
             nrow = 2)


both <- naoi_map / naoi_map_se

both


pdf("final_figures/Figure4_alt.pdf",
    width = 3.5,height = 5.5)
print(both)
dev.off()

saveRDS(both,"figures/Figure_s3.rds")







