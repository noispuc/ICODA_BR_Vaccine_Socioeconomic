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
                                "High HDI")),
        ),
        rm = factor(rm, levels = c(0, 1), labels = c("No", "Yes"))
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


## Distance to capital
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

## Population density
plot_city_vacc_density <-
    df_city_vacc_stats_filter %>%
    ggplot() +
    geom_point(aes(x = pop_area, y = doses_pop_100_D1_age_sex, color = svi), size = 0.5) +
    geom_smooth(aes(x = pop_area, y = doses_pop_100_D1_age_sex)) +
    scale_color_discrete(name = "") +
    scale_x_continuous(labels = scales::comma_format(), trans = "log10") +
    labs(
        x = "Population density (Population/Km² - Log scale)",
        y = "Standardised first dose/100 people"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )

# ggsave("output/plot_city_vacc_density.png", plot_city_vacc_density, units = "in",
#        dpi = 800, width = 5, height = 4)

plot_city_vacc_density_box <-
    df_city_vacc_stats_filter %>%
    ggplot() +
    geom_violin(aes(x = svi, y = pop_area, color = svi), size = 0.5, draw_quantiles = 0.5) +
    # geom_smooth(aes(x = pop_area, y = doses_pop_100_D1_age_sex)) +
    scale_color_discrete(name = "") +
    scale_y_log10() +
    labs(
        y = "Population density (Population/Km²)",
        yx = "HDI"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )

ggsave("output/plot_city_vacc_density_box.png", plot_city_vacc_density_box, units = "in",
       dpi = 800, width = 5, height = 4)



## Metropolitan region
plot_city_vacc_mr <-
    df_city_vacc_stats_filter %>%
    ggplot() +
    geom_violin(aes(x = factor(rm), y = doses_pop_100_D1_age_sex, color = svi), size = 0.5, draw_quantiles = 0.5) +
    # geom_smooth(aes(x = pop_area, y = doses_pop_100_D1_age_sex)) +
    scale_color_discrete(name = "") +
    # scale_x_log10() +
    labs(
        x = "Metropolitan Region",
        y = "Standardised first dose/100 people"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )

ggsave("output/plot_city_vacc_mr.png", plot_city_vacc_mr, units = "in",
       dpi = 800, width = 6, height = 4)



## Election votes
plot_city_vacc_votos <-
    df_city_vacc_stats_filter %>%
    ggplot() +
    geom_point(aes(x = perc_votos_bolso/100, 
                    y = doses_pop_100_D1_age_sex, color = svi), size = 0.5) +
    geom_smooth(aes(x = perc_votos_bolso/100, y = doses_pop_100_D1_age_sex)) +
    scale_color_discrete(name = "") +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
        x = "Election votes for Bolsonardo (%)",
        y = "Standardised first dose/100 people"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )

ggsave("output/plot_city_vacc_votos.png", plot_city_vacc_votos, units = "in",
       dpi = 800, width = 5, height = 4)




## Type Urban
plot_city_vacc_urban <-
    df_city_vacc_stats_filter %>%
    ggplot() +
    geom_violin(aes(x = type_urban, 
                    y = doses_pop_100_D1_age_sex, color = svi), size = 0.5, draw_quantiles = 0.5) +
    
    # geom_point(aes(x = perc_votos_bolso/100, 
    #                y = doses_pop_100_D1_age_sex, color = svi), size = 0.5) +
    # geom_smooth(aes(x = perc_votos_bolso/100, y = doses_pop_100_D1_age_sex)) +
    scale_color_discrete(name = "") +
    # scale_x_continuous(labels = scales::percent_format()) +
    labs(
        x = "Urban type",
        y = "Standardised first dose/100 people"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )

# ggsave("output/plot_city_vacc_votos.png", plot_city_vacc_votos, units = "in",
#        dpi = 800, width = 5, height = 4)
# 


library(patchwork)
# #### Combined plot - socioeconomic variables
plot_comb_socioeconomic <-
    (plot_city_vacc_pricare_cov + plot_city_vacc_gini + plot_city_vacc_dist + plot_city_vacc_density) +
    plot_layout(ncol = 2, guides = "collect") &
    # plot_annotation(tag_levels = "A") &
    theme(
        legend.position = "top"
    )

ggsave("output/plot_comb_socioeconomic.png", plot_comb_socioeconomic, units = "in",
       dpi = 800, width = 11, height = 7)





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
    geom_smooth(aes(x = admissions, y = doses_pop_100_D1_age_sex, color = svi), method = "gam") +
    scale_x_continuous(labels = scales::comma_format(), trans = "log10") +
    scale_color_discrete(name = "") +
    # scale_x_log10() +
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


# COVID-19 admissions and in-hospital deaths - 2020 ----------------------------
# plot_city_vacc_covid_hosp <-
#     df_city_vacc_stats_filter %>%
#     select(cod_ibge, svi, doses_pop_100_D1_age_sex, 
#            confirmed_cases_100k_2020, confirmed_deaths_100k_2020) %>% 
#     pivot_longer(-c(cod_ibge, svi, doses_pop_100_D1_age_sex), names_to = "covid", values_to = "admissions") %>% 
#     mutate(
#         covid = factor(covid,
#                        levels = c("confirmed_cases_100k_2020", "confirmed_deaths_100k_2020"),
#                        labels = c("COVID-19 Cases", "COVID-19 Deaths"))
#     ) %>% 
#     ggplot() +
#     geom_point(aes(x = admissions, y = doses_pop_100_D1_age_sex, color = svi), size = 0.5) +
#     geom_smooth(aes(x = admissions, y = doses_pop_100_D1_age_sex)) +
#     scale_x_continuous(labels = scales::comma_format()) +
#     scale_color_discrete(name = "") +
#     scale_x_log10() +
#     facet_wrap(. ~ covid, scales = "free_x") +
#     labs(
#         x = "Standardised rate per 100,000 population (log scale)",
#         y = "Standardised first dose/100 people"
#     ) +
#     theme_classic() +
#     theme(
#         legend.position = "top"
#     )
plot_city_vacc_covid_hosp_pre_vacc <-
    df_city_vacc_stats_filter %>%
    select(cod_ibge, svi, doses_pop_100_D1_age_sex, total_hosp_adm_100k_age_sex_2020, total_hosp_death_100k_age_sex_2020) %>% 
    pivot_longer(-c(cod_ibge, svi, doses_pop_100_D1_age_sex), names_to = "covid", values_to = "admissions") %>% 
    mutate(
        covid = factor(covid,
                       levels = c("total_hosp_adm_100k_age_sex_2020", "total_hosp_death_100k_age_sex_2020"),
                       labels = c("COVID-19 Hospital admissions (2020)", "COVID-19 In-hospital Deaths (2020)"))
    ) %>% 
    ggplot() +
    geom_point(aes(x = admissions, y = doses_pop_100_D1_age_sex, color = svi), size = 0.5) +
    geom_smooth(aes(x = admissions, y = doses_pop_100_D1_age_sex, group = svi)) +
    scale_x_continuous(labels = scales::comma_format(), trans = "log10") +
    scale_color_discrete(name = "") +
    # scale_x_log10() +
    facet_wrap(. ~ covid + svi, scales = "free_x") +
    labs(
        x = "Standardised rate per 100,000 population (log scale)",
        y = "Standardised first dose/100 people"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )


ggsave("output/plot_city_vacc_covid_hosp_pre_vacc_group.png", plot_city_vacc_covid_hosp_pre_vacc, units = "in",
       dpi = 800, width = 10, height = 8)



# Correlation Matrix ------------------------------------------------------

df_corr <-
    df_city_vacc_stats_filter %>% 
    select(
        doses_pop_100_D1_age_sex,
        idhm, 
        pricare_cov,
        dist_to_capital,
        gini, 
        pop_area,
        total_pop,
        total_hosp_adm_100k_age_sex_2020, 
        total_hosp_death_100k_age_sex_2020
    ) %>% cor(method = "spearman") %>% round(., 2)

rownames(df_corr) <- colnames(df_corr) <- c("Standardised Vaccination Rates", 
                       "HDI", 
                       "Primary Healthcare Coverage", 
                       "Distance to Capital", 
                       "GINI", 
                       "Population Density", 
                       "Total Population", 
                       "COVID-19 Hospital Admissions (2020)",
                       "COVID-19 In-hospital deaths (2020)")


plot_correlation_matrix <-
    ggcorrplot::ggcorrplot(df_corr,
                           ggtheme = theme_bw(),
                           type = "lower", lab = TRUE, outline.color = "white") 

ggsave("output/plot_correlation_matrix.png", plot_correlation_matrix, units = "in",
       dpi = 800, width = 10, height = 11)

