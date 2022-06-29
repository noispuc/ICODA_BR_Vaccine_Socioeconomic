################################################################################
####### Data Analysis - Vaccine Coverage per City - Vaccine Descriptive
################################################################################
Sys.setlocale(category = "LC_ALL", locale = "english")

# Libraries ---------------------------------------------------------------
library(tidyverse)


# Data Input --------------------------------------------------------------
df_city_vacc_sivep_info_month <-  read_csv("input/df_city_vacc_adm_sivep_month_D1_single.csv")

df_city_vacc_stats_filter_m <- 
    df_city_vacc_sivep_info_month %>% 
    mutate(
        hdi_group = cut_number(idhm, 3, labels = FALSE),
        hdi = factor(hdi_group,
                     levels = rev(c(1, 2, 3)),
                     labels = rev(c("Low HDI",
                                "Medium HDI",
                                "High HDI"))
        )
    ) %>%
    filter(!is.na(hdi)) 


# Model: SVI ~ characteristics and interaction ----------------------------
df_city_vacc_stats_filter_month <-
    df_city_vacc_stats_filter_m %>% 
    mutate(
        period_date = as.Date(paste0(period, "-01")),
        period_num = lubridate::month(period_date)
        ) %>% 
    mutate(
        y = round(doses_pop_100_D1_age_sex * population_2020/100), ## Obtaining age-and-sex adjusted counts
        ) %>% 
    mutate(
        hosp_adm_doses    = ((total_hosp_adm_100k_age_sex / 100000) / (doses_pop_100_D1_age_sex_agg / 100)) * 1000,
        hosp_deaths_doses = ((total_hosp_death_100k_age_sex / 100000) / (doses_pop_100_D1_age_sex_agg / 100)) * 1000
    )



df_city_stats_month <-
    df_city_vacc_stats_filter_month %>%
    group_by(period_num, hdi) %>%
    summarise(
        total_city = n(),
        doses_D1_agg_avg      = mean(doses_pop_100_D1_age_sex_agg),
        doses_D1_agg_sd       = sd(doses_pop_100_D1_age_sex_agg),
        hosp_adm_doses_avg    = mean(hosp_adm_doses),
        hosp_adm_doses_sd     = sd(hosp_adm_doses),
        hosp_deaths_doses_avg = mean(hosp_deaths_doses),
        hosp_deaths_doses_sd  = sd(hosp_deaths_doses)
    ) %>%
    ungroup() %>% 
    mutate(
        doses_D1_agg_avg_low  = doses_D1_agg_avg - 1.96 * doses_D1_agg_sd / sqrt(total_city),
        doses_D1_agg_avg_high = doses_D1_agg_avg + 1.96 * doses_D1_agg_sd / sqrt(total_city),
        
        hosp_adm_doses_avg_low  = hosp_adm_doses_avg - 1.96 * hosp_adm_doses_sd / sqrt(total_city),
        hosp_adm_doses_avg_high = hosp_adm_doses_avg + 1.96 * hosp_adm_doses_sd / sqrt(total_city),
        
        hosp_death_dose_avg_low  = hosp_deaths_doses_avg - 1.96 * hosp_deaths_doses_sd / sqrt(total_city),
        hosp_death_dose_avg_high = hosp_deaths_doses_avg + 1.96 * hosp_deaths_doses_sd / sqrt(total_city)
    ) 


writexl::write_xlsx(
    df_city_stats_month %>% 
        mutate(
            doses_D1_agg_avg_CI = paste0(round(doses_D1_agg_avg, 2), " (",
                                         round(doses_D1_agg_avg_low, 2), "-",
                                         round(doses_D1_agg_avg_high, 2), ")"),
            hosp_adm_doses_avg_CI = paste0(round(hosp_adm_doses_avg, 2), " (",
                                           round(hosp_adm_doses_avg_low, 2), "-",
                                           round(hosp_adm_doses_avg_high, 2), ")"),
            hosp_deaths_doses_avg_CI = paste0(round(hosp_deaths_doses_avg, 2), " (",
                                              round(hosp_death_dose_avg_low, 2), "-",
                                              round(hosp_death_dose_avg_high, 2), ")")
        ) %>% 
        select(period_num, hdi, total_city, doses_D1_agg_avg_CI, hosp_adm_doses_avg_CI, hosp_deaths_doses_avg_CI) %>% 
        arrange(hdi, period_num)
    , "output/Statistics/vaccine_average_coverage_hosp_death_month.xlsx"
    )




################################################################################
#### Vaccination plots
dodge_width <-  0.5

### Cumulative doses
plot_vacc_agg_month <- 
    df_city_stats_month %>% 
    ggplot() +
    geom_line(
        aes(x = period_num,
            y = doses_D1_agg_avg,
            color = hdi), position = position_dodge(width = dodge_width)
    ) +
    geom_point(
        aes(x = period_num,
            y = doses_D1_agg_avg,
            color = hdi), position = position_dodge(width = dodge_width)
    ) +
    geom_errorbar(
        aes(x = period_num,
            ymin = doses_D1_agg_avg_low,
            ymax = doses_D1_agg_avg_high,
            color = hdi), position = position_dodge(width = dodge_width), width = 0.3
    ) +
    scale_color_discrete(name = "") +
    scale_x_continuous(breaks = 1:8,
                       labels = month.abb[1:8]) +
    labs(
        y = "Standardised first doses/100 people (95% CI)",
        x = ""
    ) +
    theme_classic()


################################################################################
#### Hospital admissions

### Average hospital admission rates
plot_hosp_adm_month <-
    df_city_stats_month %>%
    ggplot() +
    geom_line(
        aes(x = period_num,
            y = hosp_deaths_doses_avg,
            color = svi)
    ) +
    scale_color_discrete(name = "") +
    scale_x_continuous(breaks = 1:8,
                       labels = month.abb[1:8]
    ) +
    labs(
        y = "Hospital admissions/100,000 people",
        x = ""
    ) +
    theme_classic()

plot_hosp_adm_month_dose <- 
    df_city_stats_month %>% 
    ggplot() +
    geom_line(
        aes(x = period_num,
            y = hosp_adm_doses_avg,
            color = hdi), position = position_dodge(width = dodge_width)
    ) +
    geom_point(
        aes(x = period_num,
            y = hosp_adm_doses_avg,
            color = hdi), position = position_dodge(width = dodge_width)
    ) +
    geom_errorbar(
        aes(x = period_num,
            ymin = hosp_adm_doses_avg_low,
            ymax = hosp_adm_doses_avg_high,
            color = hdi), position = position_dodge(width = dodge_width), width = 0.3
    ) +
    scale_color_discrete(name = "") +
    scale_x_continuous(breaks = 1:8,
                       labels = month.abb[1:8]) +
    labs(
        y = "Hospital admissions/1,000 doses (95% CI)",
        x = ""
    ) +
    theme_classic()



################################################################################
### Average in-hospital death rates
plot_hosp_death_month_dose <- 
    df_city_stats_month %>%
    ggplot() +
    geom_line(
        aes(x = period_num,
            y = hosp_deaths_doses_avg,
            color = hdi), position = position_dodge(width = dodge_width)
    ) +
    geom_point(
        aes(x = period_num,
            y = hosp_deaths_doses_avg,
            color = hdi), position = position_dodge(width = dodge_width)
    ) +
    geom_errorbar(
        aes(x = period_num,
            ymin = hosp_death_dose_avg_low,
            ymax = hosp_death_dose_avg_high,
            color = hdi), position = position_dodge(width = dodge_width), width = 0.3
    ) +
    scale_color_discrete(name = "") +
    labs(
        y = "In-hospital deaths/1,000 doses (95% CI)",
        x = ""
    ) +
    theme_classic()



library(patchwork)

plot_month_combined <-
    (plot_vacc_agg_month + plot_hosp_adm_month_dose + plot_hosp_death_month_dose) +
    plot_annotation(tag_levels = "A") +
    plot_layout(ncol = 3, guide = 'collect') &
    theme(legend.position = "top")


ggsave("output/Figure4_MonthRates_Vaccine_Doses.pdf",
       plot = plot_month_combined, width = 12, height = 4,
       units = "in", dpi = 800)



# Estimating Average Growth Rates -----------------------------------------
df_city_hosp_doses <-
    df_city_vacc_stats_filter_month %>%
    mutate(
        hosp_adm_doses = ((total_hosp_adm_100k_age_sex / 100000) / (doses_pop_100_D1_age_sex_agg / 100)) * 1000,
        hosp_deaths_doses = ((total_hosp_death_100k_age_sex / 100000) / (doses_pop_100_D1_age_sex_agg / 100)) * 1000
        )

library(lme4)
model_avg_rate_adm <- lmer(log(hosp_adm_doses + 0.0001) ~  period_num * hdi + (1|cod_ibge),
                           data = df_city_hosp_doses %>%
                               mutate(hdi = factor(hdi, levels = c("High HDI",
                                                                   "Medium HDI",
                                                                   "Low HDI"),
                                                   labels = c("High_HDI",
                                                              "Medium_HDI",
                                                              "Low_HDI")
                                                   )
                                      )
                           )

# model_avg_rate_adm_2 <- lmer(log(hosp_adm_doses + 0.0001) ~  period_num + hdi + (1|cod_ibge),
#                            data = df_city_hosp_doses %>%
#                                mutate(hdi = factor(hdi, levels = c("High HDI",
#                                                                    "Medium HDI",
#                                                                    "Low HDI"),
#                                                    labels = c("High_HDI",
#                                                               "Medium_HDI",
#                                                               "Low_HDI")
#                                )
#                                )
# )
# anova(model_avg_rate_adm, model_avg_rate_adm_2)
# p-value: 0.6047


model_avg_rate_death <- lmer(log(hosp_deaths_doses + 0.0001) ~  period_num * hdi + (1|cod_ibge),
                             data = df_city_hosp_doses %>%
                                 mutate(hdi = factor(hdi, levels = c("High HDI",
                                                                     "Medium HDI",
                                                                     "Low HDI"),
                                                     labels = c("High_HDI",
                                                                "Medium_HDI",
                                                                "Low_HDI")
                                                     )
                                        )
                             )

# model_avg_rate_death_2 <- lmer(log(hosp_deaths_doses + 0.0001) ~  period_num + hdi + (1|cod_ibge),
#                              data = df_city_hosp_doses %>%
#                                  mutate(hdi = factor(hdi, levels = c("High HDI",
#                                                                      "Medium HDI",
#                                                                      "Low HDI"),
#                                                      labels = c("High_HDI",
#                                                                 "Medium_HDI",
#                                                                 "Low_HDI")
#                                  )
#                                  )
# )
# anova(model_avg_rate_death, model_avg_rate_death_2)
# p-value: 1.702e-06



# Estimating average growth rates -----------------------------------------


library(multcomp)
library(broom.mixed)

df_avg_interaction <- 
    bind_rows(
        tidy(confint(glht(model_avg_rate_adm, linfct = c("period_num = 0")))) %>% 
            mutate(type = "hosp_adm"),
        tidy(confint(glht(model_avg_rate_adm, linfct = c("period_num + period_num:hdiMedium_HDI = 0"))))%>% 
            mutate(type = "hosp_adm"),
        tidy(confint(glht(model_avg_rate_adm, linfct = c("period_num + period_num:hdiLow_HDI = 0"))))%>% 
            mutate(type = "hosp_adm"),
        tidy(confint(glht(model_avg_rate_death, linfct = c("period_num = 0")))) %>% 
            mutate(type = "death"),
        tidy(confint(glht(model_avg_rate_death, linfct = c("period_num + period_num:hdiMedium_HDI = 0"))))%>% 
            mutate(type = "death"),
        tidy(confint(glht(model_avg_rate_death, linfct = c("period_num + period_num:hdiLow_HDI = 0"))))%>% 
            mutate(type = "death")
        ) %>% 
    mutate(
        estimate_rate  = exp(estimate)  - 1,
        conf_low_rate  = exp(conf.low)  - 1,
        conf_high_rate = exp(conf.high) - 1
    ) %>% 
    mutate(
        estimate_CI = paste0(
            round(100 * estimate_rate, 1), " (",
            round(100 * conf_low_rate, 1), ",",
            round(100 * conf_high_rate, 1), ")"
        )
    )

writexl::write_xlsx(df_avg_interaction, "output/df_avg_interaction.xlsx")


