################################################################################
####### Data Analysis - Vaccine Coverage per City - Vaccine Descriptive
################################################################################

# Libraries ---------------------------------------------------------------
library(tidyverse)

# Data Input --------------------------------------------------------------
df_city_vacc_sivep_info <-  read_csv("input/df_city_vacc_adm_sivep_D1_single_2021-08-31.csv") %>%
    mutate(
        hdi = factor(hdi_group, 
                     levels = c(1, 2, 3),
                     labels = c("Low HDI",
                                "Medium HDI",
                                "High HDI")
                     )
        )


################################################################################
### Violin plots and Beeswarm plots
library(patchwork)
library(ggbeeswarm)

plot_city_vacc_svi <- 
    df_city_vacc_sivep_info %>% 
    drop_na() %>% 
    ggplot() +
    geom_violin(aes(x = hdi, y = doses_pop_100_D1_age_sex), 
                draw_quantiles = 0.5, fill = NA) +
    labs(
        x = "",
        y = "Age-and-sex-adjusted\ndoses/100 people"
    ) +
    theme_classic()

df_median_vals <- 
    df_city_vacc_sivep_info %>% 
    filter(!is.na(hdi)) %>%
    mutate(
        region = factor(region, 
                        levels = c("North", "Northeast", "Central-West",
                                   "Southeast", "South"))
    ) %>% 
    group_by(hdi) %>% 
    summarise(
        dose_median = median(doses_pop_100_D1_age_sex)
    )

plot_params <- 
    list(shape = 21, 
         alpha = 0.6,
         cex = 0.8,                    # "distance" of the points
         priority = "none",    # you can play with the sorting as well
         groupOnX = FALSE,
         dodge.width = 0.8,
         
         line_width = 0.8
    )


plot_bee_vacc_svi <-
    df_city_vacc_sivep_info %>% 
    filter(!is.na(hdi)) %>%
    mutate(
        region = factor(region, 
                        levels = c("North", "Northeast", 
                                   "Central-West",
                                   "Southeast", "South"))
    ) %>% 
    ggplot() +
    ggbeeswarm::geom_beeswarm(
        aes(y = hdi, x = doses_pop_100_D1_age_sex, 
            fill = region, size = population_2020),
        shape = plot_params[[1]], 
        alpha = plot_params[[2]],
        cex = plot_params[[3]],                    # "distance" of the points
        priority = plot_params[[4]],    # you can play with the sorting as well
        groupOnX = plot_params[[5]],
        dodge.width = plot_params[[6]]
    ) +
    scale_fill_discrete(name = "") +
    scale_size(range = c(1, 20), guide = "none") +
    geom_boxplot(data = df_median_vals,
                 aes(y = hdi, x = dose_median), size = plot_params[[7]], width = 3) +
    labs(
        y = "",
        x = "Age-and-sex-adjusted first doses/100 people"
    ) +
    theme_minimal() +
    theme(legend.position = "top")


plot_bee_svi_comb_box_bee <-
    (
        plot_city_vacc_svi + plot_bee_vacc_svi
        ) +
    plot_annotation(tag_levels = "A") + 
    plot_layout(widths = c(1, 1)) 

ggsave("output/Figure2_HDI_FirstDoses_BeeSwarm_R2.pdf",
       plot_bee_svi_comb_box_bee,
       width = 12, height = 5, units = "in", dpi = 800)



################################################################################
#### Descriptive Table of doses/100 hab
library(gtsummary)

tbl_svi_descriptive <-
    df_city_vacc_sivep_info %>% 
    mutate(
        dist_capital_cat = cut_number(dist_to_capital, n = 4),
        region = factor(region, 
                        levels = c("North", "Northeast", 
                                   "Central-West",
                                   "Southeast", "South")),
        metro_region = factor(rm, levels = c(0, 1), labels = c("No", "Yes")),
        pop_size = cut_number(population_2020, n = 4)
        ) %>% 
    select(
        population_2020,
        pop_size,
        pop_area,
        white,
        black_brown,
        asian,
        indigenous,
        region,
        capital,
        dist_to_capital,
        type_urban,
        gini, 
        idhm,
        pricare_cov,
        doses_pop_100_D1_age_sex,
        total_hosp_adm_100k_age_sex_2020,
        total_hosp_death_100k_age_sex_2020,
        total_hosp_adm_100k_age_sex,
        total_hosp_death_100k_age_sex,
        hdi
    ) %>%
    tbl_summary(
        by = "hdi"
        ) %>% 
    add_overall()

flextable::save_as_docx(
    as_flex_table(tbl_svi_descriptive), 
    path = "output/Table1_Descriptive_Vaccine_City.docx"
    )







################################################################################
#### Plots - vaccine doses and socioeconomic variables -----------
### HDI
plot_city_vacc_hdi <- 
    df_city_vacc_sivep_info %>% 
    drop_na() %>% 
    mutate(
        hdi = factor(hdi_group, 
                     levels = c(3, 2, 1),
                     labels = c("High HDI",
                                "Medium HDI",
                                "Low HDI")
                     )
        ) %>% 
    ggplot() +
    geom_point(aes(x = idhm, y = doses_pop_100_D1_age_sex, color = factor(hdi)), size = 0.5) +
    geom_smooth(aes(x = idhm, y = doses_pop_100_D1_age_sex, group = 1)) +
    scale_color_discrete(name = "") +
    labs(
        x = "Human Development Index (HDI)",
        y = "Standardised first doses/100 people"
        ) +
    theme_classic() +
    theme(
        legend.position = "top"
        )


ggsave("output/plot_city_vacc_hdi.png",
       plot_city_vacc_hdi,
       width = 5, height = 4, units = "in", dpi = 800)


