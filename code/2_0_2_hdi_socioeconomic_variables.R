################################################################################
####### Data Analysis - Socioeconomic variables and HDI
################################################################################

# Libraries ---------------------------------------------------------------
library(tidyverse)

# Data Input --------------------------------------------------------------
df_city_vacc_sivep_info <-  read_csv("input/df_city_vacc_adm_sivep_D1_single.csv")

################################################################################
#### Plot - Age-and-sex-adj doses per 100 pop. and HDI
df_city_vacc_stats_filter <- 
    df_city_vacc_sivep_info %>% 
    mutate(
        doses_pop_100_D1_age_sex = if_else(doses_pop_100_D1_age_sex > 100, 100, doses_pop_100_D1_age_sex),
        svi_group = cut_number(idhm, 3, labels = FALSE),
        svi = factor(svi_group, 
                     levels = rev(c(1, 2, 3)),
                     labels = rev(c("Low HDI",
                                "Medium HDI",
                                "High HDI"))
        )
    ) %>% 
    filter(!is.na(svi))


### Socioeconomic Vulnerability indicator (average[HDI income, HDI education])
## HDI and componetnts vs  
df_city_hdi_dose <- 
    df_city_vacc_stats_filter %>% 
    select(cod_ibge, svi, idhm, doses_pop_100_D1_age_sex, idhm_educ, idhm_long, idhm_renda) %>% 
    pivot_longer(-c(cod_ibge, svi, doses_pop_100_D1_age_sex), names_to = "hdi", values_to = "hdi_val") %>% 
    mutate(
        
        hdi = factor(hdi, 
                     levels = c("idhm", "idhm_educ", "idhm_long", "idhm_renda"),
                     labels = c("HDI", 
                                "HDI subcomponent - Education", 
                                "HDI subcomponent - Income", 
                                "HDI subcomponent - Longevity") )
        )



plot_city_hdi_dose <- 
    df_city_hdi_dose %>% 
    ggplot() +
    geom_point(aes(x = hdi_val, y = doses_pop_100_D1_age_sex, color = svi), size = 0.5) +
    geom_smooth(aes(x = hdi_val, y = doses_pop_100_D1_age_sex)) +
    scale_color_discrete(name = "") +
    facet_wrap(. ~ hdi) +
    labs(
        x = "HDI",
        y = "Standardised first dose/100 people"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )


ggsave("output/plot_city_hdi_dose.png",
       plot = plot_city_hdi_dose, width = 10, height = 7,
       units = "in", dpi = 800)





# Socioeconomic variables -------------------------------------------------
## Primary Health Care coverage
plot_city_vacc_pricare_cov <-
    df_city_vacc_stats_filter %>%
    ggplot() +
    geom_point(aes(x = pricare_cov/100, y = doses_pop_100_D1_age_sex, color = svi), size = 0.5) +
    geom_smooth(aes(x = pricare_cov/100, y = doses_pop_100_D1_age_sex)) +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_color_discrete(name = "") +
    labs(
        x = "Primary healthcare coverage (%)",
        y = "Standardised first dose/100 people"
    ) +
    theme_classic()


## GINI index
plot_city_vacc_gini <-
    df_city_vacc_stats_filter %>%
    ggplot() +
    geom_point(aes(x = gini, y = doses_pop_100_D1_age_sex, color = svi),
                   size = 0.5) +
    geom_smooth(aes(x = gini, y = doses_pop_100_D1_age_sex)) +
    scale_color_discrete(name = "") +
    labs(
        x = "GINI Index",
        y = "Standardised first dose/100 people"
        ) +
    theme_classic()


## GINI Distance to capital
plot_city_vacc_dist <-
    df_city_vacc_stats_filter %>%
    ggplot() +
    geom_point(aes(x = dist_to_capital, y = doses_pop_100_D1_age_sex, color = svi), size = 0.5) +
    geom_smooth(aes(x = dist_to_capital, y = doses_pop_100_D1_age_sex)) +
    scale_color_discrete(name = "") +
    labs(
        x = "Distance to State Capital (Km)",
        y = "Standardised first dose/100 people"
    ) +
    theme_classic()




library(patchwork)
# #### Combined plot - socioeconomic variables
plot_comb_socioeconomic <-
    (plot_city_vacc_pricare_cov + plot_city_vacc_gini + plot_city_vacc_dist) +
    plot_layout(ncol = 3, guides = "collect") &
    # plot_annotation(tag_levels = "A") &
    theme(
        legend.position = "top"
    )

ggsave("output/plot_comb_socioeconomic.png", plot_comb_socioeconomic, units = "in",
       dpi = 800, width = 12, height = 4)





# COVID-19 admissions and in-hospital deaths ------------------------------
plot_city_vacc_covid_hosp <-
    df_city_vacc_stats_filter %>%
    select(cod_ibge, svi, doses_pop_100_D1_age_sex, total_hosp_adm_100k_age_sex, total_hosp_death_100k_age_sex) %>% 
    pivot_longer(-c(cod_ibge, svi, doses_pop_100_D1_age_sex), names_to = "covid", values_to = "admissions") %>% 
    mutate(
        covid = factor(covid,
                       levels = c("total_hosp_adm_100k_age_sex", "total_hosp_death_100k_age_sex"),
                       labels = c("COVID-19 Hospital admissions", "COVID-19 In-hospital Deaths"))
    ) %>% 
    ggplot() +
    geom_point(aes(x = admissions, y = doses_pop_100_D1_age_sex, color = svi), size = 0.5) +
    geom_smooth(aes(x = admissions, y = doses_pop_100_D1_age_sex)) +
    scale_x_continuous(labels = scales::comma_format()) +
    scale_color_discrete(name = "") +
    scale_x_log10() +
    facet_wrap(. ~ covid, scales = "free_x") +
    labs(
        x = "Standardised rate per 100,000 population (log scale)",
        y = "Standardised first dose/100 people"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )


ggsave("output/plot_city_vacc_covid_hosp.png", plot_city_vacc_covid_hosp, units = "in",
       dpi = 800, width = 8, height = 4)

