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






# Modelling association ---------------------------------------------------

### Negative Binomial model
library(rms)
library(splines)

df_model_vacc_vars <- 
    df_city_vacc_sivep_info %>%
    mutate(
        y = round(doses_pop_100_D1_age_sex * population_2020 / 100), ## Obtaining age-and-sex adjusted counts
        
        idhm = idhm * 10,
        pricare_cov_10 = pricare_cov / 10,

        quartil_idhm = factor(hdi, levels = c("High HDI", "Medium HDI", "Low HDI")),
        region = factor(region,
                        levels = c("Northeast", "North", "Central-West",
                                   "Southeast", "South")),
        codigo_ibge_6 = as.character(codigo_ibge_6),
        ln_pop_area = log(pop_area),
        ln_total_hosp_adm_100k_age_sex_2020 = ifelse(is.infinite(log(total_hosp_adm_100k_age_sex_2020)), 0, log(total_hosp_adm_100k_age_sex_2020)),
        metro_region = factor(rm, levels = c(0, 1), labels = c("No", "Yes")),
        pop_size = cut_number(population_2020, n = 4)
        ) %>% 
    filter(!is.na(hdi)) 


#### NB model
## Estimating RCS knots for modelling
knot_dist <- rcspline.eval(df_model_vacc_vars$dist_to_capital, knots.only = TRUE)
knot_gini <- rcspline.eval(df_model_vacc_vars$gini, knots.only = TRUE)
# knot_hcw <- rcspline.eval(df_model_vacc_vars$hcw_100k, knots.only = TRUE)

## Estimating - Original Model
# model_vacc_idhm_quartil <-
#     MASS::glm.nb(
#         y ~ quartil_idhm * pricare_cov_10_var +
#             rcs(gini, parms = knot_gini) +
#             rcs(total_pop, parms = knot_pop) +
#             rcs(dist_to_capital, parms = knot_dist) +
#             region +
#             offset(log(total_pop)),
#         data = df_model_vacc_vars %>%
#             mutate(
#                 pricare_cov_10_var = pricare_cov_10 - mean(pricare_cov_10)
#             )
#         )
## Reduced model (R1)
# model_vacc_idhm_quartil_red <-
#     MASS::glm.nb(
#         y ~ quartil_idhm * pricare_cov_10_var +
#             # metro_region +
#             ln_pop_area +
#             ln_total_hosp_adm_100k_age_sex_2020 +
#             # rcs(total_pop, parms = knot_pop) +
#             # rcs(gini, parms = knot_gini) +
#             # rcs(dist_to_capital, parms = knot_dist) +
#             # region +
#             offset(log(total_pop)),
#         data = df_model_vacc_vars %>% 
#             mutate(
#                 pricare_cov_10_var = pricare_cov_10 - mean(pricare_cov_10)
#             )
#     )


## Estimating -  Model after R1 (including new variables)
model_vacc_idhm_quartil <-
    MASS::glm.nb(
        y ~ quartil_idhm * pricare_cov_10_var +
            type_urban +
            ln_pop_area +
            ln_total_hosp_adm_100k_age_sex_2020 +
            pop_size +
            rcs(gini, parms = knot_gini) +
            rcs(dist_to_capital, parms = knot_dist) +
            region +
            offset(log(population_2020))
        , data = df_model_vacc_vars %>%
            mutate(
                pricare_cov_10_var = pricare_cov_10 - mean(pricare_cov_10)
            ) %>%
            select(quartil_idhm, pricare_cov_10_var, metro_region, 
                   ln_pop_area, ln_total_hosp_adm_100k_age_sex_2020,
                   gini, dist_to_capital, region, population_2020, y, 
                   type_urban, pop_size, pop_area, ln_hcw_100k, 
                   total_hosp_adm_100k_age_sex_2020) %>%
            drop_na()
        )

## Obtaining estimates as RR
df_model_vacc_idhm_quartil_estimates <- 
    broom.mixed::tidy(
        model_vacc_idhm_quartil,
        exponentiate = TRUE,
        conf.int = TRUE
    ) %>% 
    filter(term != "(Intercept)") %>%
    select(term, estimate, conf.low, conf.high) 


writexl::write_xlsx(df_model_vacc_idhm_quartil_estimates, "output/df_model_vacc_idhm_quartil_estimates.xlsx")





#### Calculating Marginal Means for interaction FHS coverage vs HDI
library(ggeffects)
library(splines)


pricare_terms <- c(0, 2.5, 5, 7.5, 10) - mean(df_model_vacc_vars$pricare_cov_10)

model_predict <- ggemmeans(model_vacc_idhm_quartil,
                           terms = c("pricare_cov_10_var[pricare_terms]", "quartil_idhm"), 
                           params = c("knot_dist", "knot_gini"))

avg_city_pop <- mean(df_model_vacc_vars$population_2020[!is.na(df_model_vacc_vars$hdi)])


model_predict_rates <- 
    model_predict %>% 
    as_tibble() %>% 
    mutate(
        x = x + mean(df_model_vacc_vars$pricare_cov_10),
        predicted = predicted / avg_city_pop * 100,
        conf.low  = conf.low  / avg_city_pop * 100,
        conf.high = conf.high / avg_city_pop * 100
    )

plot_adj_pred_interaction <-
    model_predict_rates %>%
    ggplot() +
    geom_line(aes(x = x, y = predicted, color = group)) +
    geom_ribbon(aes(x = x, y = predicted, 
                    ymin = conf.low, 
                    ymax = conf.high, fill = group), alpha = 0.3) +
    scale_x_continuous(
        breaks = seq(0, 10, 1),
        labels = paste0(seq(0, 10, 1) * 10, "%", recycle0 = TRUE)
        ) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(
        color = "",
        fill  = "",
        x     = "Primary healthcare coverage (%)",
        y     = "First doses per 100 people"
        ) + 
    theme_classic() +
    theme(
        legend.position = "top"
        )

ggsave("output/Figure3_MarginalEffects_FirstDose.pdf", 
       plot_adj_pred_interaction, 
       units = "in", dpi = 800, width = 7, height = 5)


writexl::write_xlsx(
    model_predict_rates %>% 
        mutate(
            predicted_ci = paste0(round(predicted, 1), " (",
                                  round(conf.low, 1), "-",
                                  round(conf.high, 1), ")"
                                  )
        ) %>% 
        as_tibble() %>% 
        select(phc_coverage_10 = x, group, predicted_ci) %>% 
        pivot_wider(names_from = "group", values_from = "predicted_ci"),
    
    "output/model_predict_quart_idhm.xlsx")






### Marginal means with population, distance and GINI
model_predict_gini <- ggemmeans(model_vacc_idhm_quartil,
                           terms = c("gini [quart]", "quartil_idhm"), 
                           params = c("knot_pop", "knot_dist", "knot_gini"))

plot_adj_emmeans_gini <-
    model_predict_gini %>% 
    mutate(
        predicted = predicted / avg_city_pop * 100,
        conf.low  = conf.low  / avg_city_pop * 100,
        conf.high = conf.high / avg_city_pop * 100
    ) %>% 
    ggplot() +
    geom_line(aes(x = x, y = predicted, color = group)) +
    geom_ribbon(aes(x = x, y = predicted, 
                    ymin = conf.low, 
                    ymax = conf.high, fill = group), alpha = 0.3) +
    labs(
        color = "",
        fill  = "",
        x     = "GINI Index",
        # y     = "Predicted count of doses"
        y     = "First doses per 100 people"
    ) + 
    theme_classic() +
    theme(
        legend.position = "top"
    )


### Marginal means with distance
model_predict_dist <- ggemmeans(model_vacc_idhm_quartil,
                                terms = c("dist_to_capital [quart]", "quartil_idhm"), 
                                params = c("knot_pop", "knot_dist", "knot_gini"))

plot_adj_emmeans_dist <-
    model_predict_dist %>% 
    mutate(
        predicted = predicted / avg_city_pop * 100,
        conf.low  = conf.low  / avg_city_pop * 100,
        conf.high = conf.high / avg_city_pop * 100
    ) %>% 
    ggplot() +
    geom_line(aes(x = x, y = predicted, color = group)) +
    geom_ribbon(aes(x = x, y = predicted, 
                    ymin = conf.low, 
                    ymax = conf.high, fill = group), alpha = 0.3) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(
        color = "",
        fill  = "",
        x     = "Distance to State Capital (Km)",
        # y     = "Predicted count of doses"
        y     = "First doses per 100 people"
    ) + 
    theme_classic() +
    theme(
        legend.position = "top"
    )



### Marginal means with Population
model_predict_pop <- ggemmeans(model_vacc_idhm_quartil,
                                terms = c("population_2020 [quart2]", "quartil_idhm"), 
                                params = c("knot_pop", "knot_dist", "knot_gini"))

plot_adj_emmeans_pop <-
    model_predict_pop %>% 
    mutate(
        # x = x + mean(df_model_vacc_vars$pricare_cov_10),
        predicted = predicted / avg_city_pop * 100,
        conf.low  = conf.low  / avg_city_pop * 100,
        conf.high = conf.high / avg_city_pop * 100
    ) %>% 
    ggplot() +
    geom_line(aes(x = x, y = predicted, color = group)) +
    geom_ribbon(aes(x = x, y = predicted, 
                    ymin = conf.low, 
                    ymax = conf.high, fill = group), alpha = 0.3) +
    # scale_x_continuous(
    #     breaks = seq(0, 10, 1),
    #     labels = paste0(seq(0, 10, 1) * 10, "%", recycle0 = TRUE)
    # ) +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_x_continuous(labels = scales::comma_format()) +
    labs(
        color = "",
        fill  = "",
        x     = "Population",
        # y     = "Predicted count of doses"
        y     = "First doses per 100 people"
    ) + 
    theme_classic() +
    theme(
        legend.position = "top"
    )




#### Combined plot - Marginal Means of other continous variables
library(patchwork)
plot_comb_emmeans_continuous <-
    (plot_adj_emmeans_pop + plot_adj_emmeans_gini + plot_adj_emmeans_dist) +
    plot_layout(ncol = 3, guides = "collect") &
    # plot_annotation(tag_levels = "A") &
    theme(
        legend.position = "top"
    )

ggsave("output/plot_comb_emmeans_continuous.png", plot_comb_emmeans_continuous, units = "in",
       dpi = 800, width = 12, height = 4)




#### Estimating RR based on different PHC references
model_vacc_idhm_quartil_cob_range <- 
    sort(c(seq(0, 10, 1), mean(df_model_vacc_vars$pricare_cov_10))) %>% 
    map_dfr(function(x) {
        model_est <- 
            MASS::glm.nb(
                y ~ quartil_idhm * pricare_cov_10_var + 
                    type_urban +
                    ln_pop_area +
                    ln_total_hosp_adm_100k_age_sex_2020 +
                    pop_size +
                    rcs(gini, parms = knot_gini) +
                    rcs(dist_to_capital, parms = knot_dist) +
                    region +
                    offset(log(population_2020)),
                data = df_model_vacc_vars %>%
                    mutate(
                        pricare_cov_10_var = pricare_cov_10 - x
                        )
                )
        
        
        model_est_tidy <- 
            broom.mixed::tidy(
                model_est,
                exponentiate = TRUE,
                conf.int = TRUE
            ) %>% 
            filter(term != "(Intercept)") %>%
            select(term, estimate, conf.low, conf.high) %>% 
            filter(str_detect(term, "(quartil)")) %>% 
            filter(!str_detect(term, "(pricare)")) %>%
            mutate(
                term = str_remove(term, "quartil_idhm")
            ) %>% 
            bind_rows(
                tibble(
                    term = c("High HDI (Ref.)"),
                    estimate = c(1)
                    )
            )  %>% 
            mutate(
                order = c(2, 3, 1)
            ) %>% 
            arrange(order)

        model_est_tidy_header <-
            bind_rows(
                tibble(
                    term = if_else(x == mean(df_model_vacc_vars$pricare_cov_10), 
                                   paste0("Average: ", round(x*10, 1), "%"),
                                   paste0("PHC coverage: ", round(x*10, 1), "%")
                                   )
                ),
                model_est_tidy
            ) %>%
            mutate(
                order = x * 10
            )


        
        return(model_est_tidy_header)
        }
        ) 

writexl::write_xlsx(model_vacc_idhm_quartil_cob_range, "output/Statistics/model_vacc_idhm_quartil_cob_range.xlsx")


plot_margins_RR_groupHDI <-
    model_vacc_idhm_quartil_cob_range %>%
    filter(str_detect(term, "(Low|Medium)")) %>%
    mutate(
        is_phc_cov = if_else(order == mean(df_model_vacc_vars$pricare_cov), "yes_avg", "not_avg")
    ) %>% 
    ggplot() +
    geom_point(aes(y = estimate, x = order, color = term)) +
    geom_errorbar(aes(y = estimate, ymin = conf.low,
                      ymax = conf.high, x = order, color = term, 
                      linetype = is_phc_cov),
                  width = 0.7) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_color_discrete(name = "") +
    geom_label(aes(y = 1.02, x = 50, label = "Reference: High HDI"),
               size = 4) +
    scale_linetype_discrete(guide = "none") +
    labs(
        x = "Primary healthcare coverage (%)",
        y = "Rate Ratio (95% Confidence Interval)"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )


ggsave("output/plot_margins_RR_groupHDI_D1_single.png", plot_margins_RR_groupHDI,
       units = "in", dpi = 800, width = 7, height = 5)


















