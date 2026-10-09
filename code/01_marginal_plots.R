#### MARGINAL EFFECTS PLOTS #### 
#Lake Ontario LWF and CIS 
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

#### read in data ##### 
#read in marginal data from Maxent model 
cis_marginal_hist<-read.csv("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Manuscripts/Ont_CIS_LWF/ont_cis_marginal_hist.csv")
lwf_marginal_hist<-read.csv("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Manuscripts/Ont_CIS_LWF/ont_lwf_marginal_hist.csv")
cis_marginal_contemp<-read.csv("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Manuscripts/Ont_CIS_LWF/ont_cis_marginal_contemp.csv")
lwf_marginal_contemp<-read.csv("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Manuscripts/Ont_CIS_LWF/ont_lwf_marginal_contemp.csv")

#look at variables 
summary(as.factor(cis_marginal_hist$variable))
summary(as.factor(lwf_marginal_hist$variable))
summary(as.factor(cis_marginal_contemp$variable))
summary(as.factor(lwf_marginal_contemp$variable))

#### CISCO #### 
#remove the average data and do own averages and SD 
#only fetch, bathy, and substrate
cis_hist<-cis_marginal_hist %>% 
  filter(variable != "species_bathy_extrap" & variable != "species_fetch" & variable != "species_substrate_hist") %>% 
  filter(variable != "species_bathy_extrap_only" & variable != "species_fetch_only" &  variable != "species_substrate_hist_only") 

cis_hist$enviro <- sub("^species_[0-9]+_", "", cis_hist$variable) #remove fold number

cis_contemp<-cis_marginal_contemp %>% 
  filter(variable != "species_bathy_extrap" & variable != "species_contemp_ice_extrap") %>% 
  filter(variable != "species_bathy_extrap_only" & variable != "species_contemp_ice_extrap_only") 

cis_contemp$enviro <- sub("^species_[0-9]+_", "", cis_contemp$variable) #remove fold number


#Summarise mean & sd 
cis_hist_summary <- cis_hist %>%
  group_by(enviro, x) %>%
  summarise(
    mean_value = mean(y, na.rm = TRUE),
    sd_value   = sd(y, na.rm = TRUE), 
    .groups = "drop"
  )

summary(as.factor(cis_hist_summary$enviro))

#get values at threshold 0.13 and highest suitability
cis_hist_summary %>%
  drop_na(sd_value) %>%
  filter(x > 0.00) %>% #can't have neg fetch or bathy
  group_by(enviro) %>%
  summarise(
    x_at_0.25 = x[which.min(abs(mean_value - 0.13))],
    x_at_1    = x[which.max(abs(mean_value))]
  )


cis_contemp_summary <- cis_contemp %>%
  group_by(enviro, x) %>%
  summarise(
    mean_value = mean(y, na.rm = TRUE),
    sd_value   = sd(y, na.rm = TRUE), 
    .groups = "drop"
  )

summary(as.factor(cis_contemp_summary$enviro))

cis_contemp_summary %>%
  drop_na(sd_value) %>%
  filter(x > 0.00) %>% #can't have neg fetch or bathy
  group_by(enviro) %>%
  summarise(
    x_at_0.25 = x[which.min(abs(mean_value - 0.05))],
    x_at_1    = x[which.max(abs(mean_value))], 
    max = max(x)
  )

#* Historical ####
#marginal #
bathy_marg<-filter(cis_hist_summary, enviro == "bathy_extrap")
fetch_marg<-filter(cis_hist_summary, enviro == "fetch")%>% 
  drop_na(sd_value)

substrate_marg<-filter(cis_hist_summary, enviro == "substrate_hist")%>% 
  mutate(x =case_when(
    x == 1 ~ "boulders",
    x == 2 ~ "clay",
    x == 3 ~ "clay_gravel",
    x == 4 ~  "clay_mud", 
    x == 5 ~ "clay_rocks",
    x == 6 ~  "gravel",
    x == 7 ~ "hard",
    x == 8 ~ "mud", 
    x == 9 ~  "rocky",
    x == 10 ~  "sand",
    x == 11 ~  "sand_clay",
    x == 12 ~ "sand_gravel",
    x == 13 ~  "sand_mud",
    x == 14 ~ "sand_rock")
  )



#plot each separately 
bathy<-ggplot(bathy_marg, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(linewidth = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "bathymetry (meters)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))

fetch<-ggplot(fetch_marg, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(linewidth = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "fetch (meters)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))


#plot substrate
substrate<-ggplot(substrate_marg, aes(x = x, y = mean_value)) +
  geom_bar(stat="identity") +
  ylim(0, 1) +
  geom_errorbar(aes(ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value),
                width = 0.2, size = 0.9) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "substrate",
       y = "suitability"
  )

# variable only ##
#same plot for only variables for supplement 
fetch_only_df<-filter(cis_hist_summary, enviro == "fetch_only")%>% 
  drop_na(sd_value)
bathy_only_df<-filter(cis_hist_summary, enviro == "bathy_extrap_only")

substrate_only_df<-filter(cis_hist_summary, enviro == "substrate_hist_only")%>% 
  mutate(x =case_when(
    x == 1 ~ "boulders",
    x == 2 ~ "clay",
    x == 3 ~ "clay_gravel",
    x == 4 ~  "clay_mud", 
    x == 5 ~ "clay_rocks",
    x == 6 ~  "gravel",
    x == 7 ~ "hard",
    x == 8 ~ "mud", 
    x == 9 ~  "rocky",
    x == 10 ~  "sand",
    x == 11 ~  "sand_clay",
    x == 12 ~ "sand_gravel",
    x == 13 ~  "sand_mud",
    x == 14 ~ "sand_rock")
  )



#plot each separately 

fetch_only<-ggplot(fetch_only_df, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(size = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "fetch (meters)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))


bathy_only<-ggplot(bathy_only_df, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(size = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "bathymetry (meters)",
    y = "suitability",
  )   +
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))



#plot substrate
substrate_only<-ggplot(substrate_only_df, aes(x = x, y = mean_value)) +
  geom_bar(stat="identity") +
  ylim(0, 1) +
  geom_errorbar(aes(ymin = mean_value - sd_value,
                    ymax = mean_value + sd_value),
                width = 0.2, size = 0.9) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(x = "substrate",
       y = "suitability"
  )


#* Contemporary ####
bathy_contemp<-filter(cis_contemp_summary, enviro == "bathy_extrap")%>% 
  drop_na(sd_value)
bathy_contemp_only<-filter(cis_contemp_summary, enviro == "bathy_extrap_only")
ice_contemp<-filter(cis_contemp_summary, enviro == "contemp_ice_extrap")%>% 
  drop_na(sd_value)
ice_contemp_only<-filter(cis_contemp_summary, enviro == "contemp_ice_extrap_only")%>% 
  drop_na(sd_value)


bathy_contemp_plot<-ggplot(bathy_contemp, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(size = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "bathymetry (meters)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))


bathy_contemp_only_plot<-ggplot(bathy_contemp_only, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(size = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "bathymetry (meters)",
    y = "suitability",
  )   +
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))

ice_contemp_plot<-ggplot(ice_contemp, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(size = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "ice duration (days)",
    y = "suitability",
  )   +
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))

ice_contemp_only_plot<-ggplot(ice_contemp_only, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(size = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "ice duration (days)",
    y = "suitability",
  )   +
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))


cisco_marg_plot<-cowplot::plot_grid(bathy, fetch, substrate, 
                                    bathy_only, fetch_only, substrate_only, 
                                    bathy_contemp_plot, ice_contemp_plot, NULL,
                                    bathy_contemp_only_plot, ice_contemp_only_plot,
                                    nrow=4, labels = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', '', 'i', 'j') 
)

cisco_marg_plot

ggsave("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Manuscripts/Ont_CIS_LWF/figures/cisco_marg_plot.png",
       cisco_marg_plot,
       width = 8,
       height = 8,
       device = "png",         # File format
       dpi = 300,        
       units = c("in"))


#### LAKE WHITEFISH #### 

#remove the average data and do own averages and SD 
lwf_hist<-lwf_marginal_hist %>% 
  filter(variable != "species_bathy_extrap" & variable != "species_hist_ice_extrap" ) %>% 
  filter(variable != "species_bathy_extrap_only" & variable != "species_hist_ice_extrap_only") 

lwf_hist$enviro <- sub("^species_[0-9]+_", "", lwf_hist$variable) #remove fold number

lwf_contemp<-lwf_marginal_contemp %>% 
  filter(variable != "species_fetch" & variable != "species_contemp_ice_extrap") %>% 
  filter(variable != "species_fetch_only" & variable != "species_contemp_ice_extrap_only") 

lwf_contemp$enviro <- sub("^species_[0-9]+_", "", lwf_contemp$variable) #remove fold number


#Summarise mean & sd 
lwf_hist_summary <- lwf_hist %>%
  group_by(enviro, x) %>%
  summarise(
    mean_value = mean(y, na.rm = TRUE),
    sd_value   = sd(y, na.rm = TRUE), 
    .groups = "drop"
  )

summary(as.factor(lwf_hist_summary$enviro))

#look at ranges using the maxSSS threshold
lwf_hist_summary %>%
  drop_na(sd_value) %>%
  filter(x >0) %>% 
  group_by(enviro) %>%
  summarise(
    x_at_0.25 = x[which.min(abs(mean_value - 0.11))],
    x_at_1    = x[which.max(abs(mean_value))], 
    max = max(x)
  )


lwf_contemp_summary <- lwf_contemp %>%
  group_by(enviro, x) %>%
  summarise(
    mean_value = mean(y, na.rm = TRUE),
    sd_value   = sd(y, na.rm = TRUE), 
    .groups = "drop"
  )

summary(as.factor(lwf_contemp_summary$enviro))

lwf_contemp_summary %>%
  drop_na(sd_value) %>%
  filter(x >0) %>% 
  group_by(enviro) %>%
  summarise(
    x_at_0.25 = x[which.min(abs(mean_value - 0.066))],
    x_at_1    = x[which.min(abs(mean_value - 1))], 
    max = max(x)
  )

#* Historical  #### 
lwf_bathy_hist<-filter(lwf_hist_summary, enviro == "bathy_extrap")
lwf_ice_hist<-filter(lwf_hist_summary, enviro == "hist_ice_extrap")%>% 
  drop_na(sd_value)


#plot each separately 
bathy_lwf_hist<-ggplot(lwf_bathy_hist, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(linewidth = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "bathymetry (meters)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))

ice_lwf_hist_plot<-ggplot(lwf_ice_hist, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(linewidth = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "ice duration (days)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))

lwf_bathy_hist_only<-filter(lwf_hist_summary, enviro == "bathy_extrap_only")
lwf_ice_hist_only<-filter(lwf_hist_summary, enviro == "hist_ice_extrap_only")%>% 
  drop_na(sd_value)

only_bathy_lwf_hist_plot<-ggplot(lwf_bathy_hist_only, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(linewidth = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "bathymetry (meters)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))

ice_only_lwf_hist_plot<-ggplot(lwf_ice_hist_only, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(linewidth = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "ice duration (days)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))


#* Contemporary #### 
lwf_fetch_contemp<-filter(lwf_contemp_summary, enviro == "fetch")
lwf_ice_contemp<-filter(lwf_contemp_summary, enviro == "contemp_ice_extrap")%>% 
  drop_na(sd_value)


#plot each separately 
lwf_fetch_contemp_plot<-ggplot(lwf_fetch_contemp, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(linewidth = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "fetch (meters)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))

ice_lwf_contemp_plot<-ggplot(lwf_ice_contemp, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(linewidth = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "ice duration (days)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))

lwf_fetch_contemp_only<-filter(lwf_contemp_summary, enviro == "fetch_only")
lwf_ice_contemp_only<-filter(lwf_contemp_summary, enviro == "contemp_ice_extrap_only")%>% 
  drop_na(sd_value)

only_fetch_lwf_contemp_plot<-ggplot(lwf_fetch_contemp_only, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(linewidth = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "fetch (meters)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))

ice_only_lwf_hist_plot<-ggplot(lwf_ice_contemp_only, aes(x = x, y = mean_value)) +
  geom_ribbon(aes(ymin = mean_value - sd_value,
                  ymax = mean_value + sd_value),
              alpha = 0.2,
              color = NA) +          # shaded SD band
  geom_line(linewidth = 1.2) +            # mean line
  theme_bw() +
  labs(
    x = "ice duration (days)",
    y = "suitability",
  )  + 
  scale_x_continuous(limits = c(0.00000, NA)) +
  scale_y_continuous(limits = c(0, 1))

lwf_marg_plot<-cowplot::plot_grid(bathy_lwf_hist, ice_lwf_hist_plot, 
                                  only_bathy_lwf_hist_plot,ice_only_lwf_hist_plot,  
                                  ice_lwf_contemp_plot,  lwf_fetch_contemp_plot, 
                                  ice_only_lwf_hist_plot, only_fetch_lwf_contemp_plot,
                                    nrow=4, labels = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h') 
)

lwf_marg_plot

ggsave("C:/Users/kingk42/OneDrive - State of Michigan DTMB/Desktop/Manuscripts/Ont_CIS_LWF/figures/lwf_marg_plot.png",
       lwf_marg_plot,
       width = 8,
       height = 8,
       device = "png",         # File format
       dpi = 300,        
       units = c("in"))

