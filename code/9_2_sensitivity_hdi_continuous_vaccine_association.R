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




# Sensitivity analysis: IDH continuous ------------------------------------


#### NB model
## Estimating RCS knots for modelling
# knot_pop  <- rcspline.eval(df_city_vacc_stats_filter$total_pop, knots.only = TRUE)
knot_dist <- rcspline.eval(df_city_vacc_stats_filter$dist_to_capital, knots.only = TRUE)
knot_gini <- rcspline.eval(df_city_vacc_stats_filter$gini, knots.only = TRUE)


## Estimating 
model_vacc_idhm <-
    MASS::glm.nb(
        y ~ idhm_10 * pricare_cov_10_var + 
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
                idhm_10 = idhm - mean(idhm),
                pricare_cov_10_var = pricare_cov_10 - mean(pricare_cov_10)
            )
    )



## Obtaining estimates as RR
df_model_vacc_idhm_estimates <- 
    broom.mixed::tidy(
        model_vacc_idhm,
        exponentiate = TRUE,
        conf.int = TRUE
    ) %>% 
    filter(term != "(Intercept)") %>%
    select(term, estimate, conf.low, conf.high) 


writexl::write_xlsx(df_model_vacc_idhm_estimates, "output/df_sensitivity_model_vacc_idhm_estimates.xlsx")




#### Calculating Marginal Means for interaction PHC coverage vs HDI
library(ggeffects)
library(splines)


pricare_terms <- c(0, 2.5, 5, 7.5, 10) - mean(df_model_vacc_vars$pricare_cov_10)
idhm_terms <- c(4, 5, 6, 7, 8) - mean(df_model_vacc_vars$idhm)


avg_city_pop <- mean(df_model_vacc_vars$population_2020[!is.na(df_model_vacc_vars$hdi)])

model_predict_idhm <- ggemmeans(model_vacc_idhm,
                                terms = c("pricare_cov_10_var [pricare_terms]", "idhm_10 [idhm_terms]"),
                                params = c("knot_pop", "knot_dist", "knot_gini"))

model_predict_rates_idhm <- 
    model_predict_idhm %>% 
    as_tibble() %>% 
    mutate(
        x = x + mean(df_model_vacc_vars$pricare_cov_10),
        group = as.numeric(as.character(group)) + mean(df_model_vacc_vars$idhm),
        predicted = predicted / avg_city_pop * 100,
        conf.low  = conf.low  / avg_city_pop * 100,
        conf.high = conf.high / avg_city_pop * 100
    ) %>% 
    mutate(
        group = group / 10
    )



plot_adj_pred_interaction_idhm <-
    model_predict_rates_idhm %>% 
    ggplot() +
    geom_line(aes(x = x, y = predicted, color = factor(group))) +
    geom_ribbon(aes(x = x, y = predicted, ymin = conf.low, 
                    ymax = conf.high, fill = factor(group)), alpha = 0.3) +
    scale_x_continuous(
        breaks = seq(0, 10, 1),
        labels = paste0(seq(0, 10, 1) * 10, "%", recycle0 = TRUE)
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(
        color = "HDI",
        fill  = "HDI",
        x     = "Primary healthcare coverage (%)",
        y     = "First doses per 100 people"
    ) + 
    theme_classic() +
    theme(
        legend.position = "top"
    )

ggsave("output/plot_sensitivity_adj_pred_interaction_idhm.png", 
       plot_adj_pred_interaction_idhm, 
       units = "in", dpi = 800, width = 7, height = 5)





#### Estimating RR based on different cob-esf references
model_vacc_idhm_cob_range <- 
    sort(c(seq(0, 10, 1), mean(df_model_vacc_vars$pricare_cov_10))) %>% 
    map_dfr(function(x) {
        model_est <- 
            MASS::glm.nb(
                y ~ idhm * pricare_cov_10_var + 
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
                        idhm_10 = idhm - mean(idhm),
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
            filter(str_detect(term, "(idhm)")) %>% 
            filter(!str_detect(term, "(pricare)"))

        model_est_tidy_header <-
            bind_rows(
                tibble(
                    term = if_else(x == mean(df_model_vacc_vars$pricare_cov_10[!is.na(df_model_vacc_vars$svi)]), 
                                   paste0("Average: ", round(x*10, 1), "%"),
                                   paste0("PHC coverage: ", round(x*10, 1), "%")
                    ),
                ),
                model_est_tidy
            ) %>%
            mutate(
                order = x * 10
            )
        
        
        
        return(model_est_tidy_header)
    }
    ) 


writexl::write_xlsx(model_vacc_idhm_cob_range, 
                    "output/Statistics/df_sensitivity_model_vacc_idhm_cob_range.xlsx")





plot_margins_RR_HDI <-
    model_vacc_idhm_cob_range %>%
    mutate(
        is_phc_cov = if_else(order == mean(df_model_vacc_vars$pricare_cov), "yes_avg", "not_avg")
    ) %>% 
    ggplot() +
    geom_point(aes(y = estimate, x = order)) +
    geom_errorbar(aes(y = estimate, ymin = conf.low,
                      ymax = conf.high, x = order, 
                      linetype = is_phc_cov),
                  width = 0.7) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_color_discrete(name = "") +
    scale_linetype_discrete(guide = "none") +
    labs(
        x = "Primary healthcare coverage (%)",
        y = "Rate Ratio (95% Confidence Interval)"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )


ggsave("output/plot_sensitivity_margins_RR_HDI_D1_single.png", plot_margins_RR_HDI,
       units = "in", dpi = 800, width = 7, height = 5)



