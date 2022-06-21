################################################################################
####### Data Analysis - Vaccine Coverage per City - Vaccine Descriptive
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
                     levels = c(1, 2, 3),
                     labels = c("Low HDI",
                                "Medium HDI",
                                "High HDI")
                     )
        ) 


write_csv(df_city_vacc_stats_filter,
          "input/df_city_vacc_adm_sivep_hdi_D1_single.csv")


### Violin plots and Beeswarm plots
library(patchwork)
library(ggbeeswarm)

plot_city_vacc_svi <- 
    df_city_vacc_stats_filter %>% 
    drop_na() %>% 
    ggplot() +
    geom_violin(aes(x = svi, y = doses_pop_100_D1_age_sex), 
                draw_quantiles = 0.5, fill = NA) +
    labs(
        x = "",
        y = "Age-and-sex-adjusted\ndoses/100 people"
    ) +
    theme_classic()

df_median_vals <- 
    df_city_vacc_stats_filter %>% 
    filter(!is.na(svi)) %>%
    mutate(
        region = factor(region, 
                        levels = c("North", "Northeast", "Central-West",
                                   "Southeast", "South"))
    ) %>% 
    group_by(svi) %>% 
    summarise(
        dose_median    = median(doses_pop_100_D1_age_sex)
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
    df_city_vacc_stats_filter %>% 
    filter(!is.na(svi)) %>%
    mutate(
        region = factor(region, 
                        levels = c("North", "Northeast", 
                                   "Central-West",
                                   "Southeast", "South"))
    ) %>% 
    ggplot() +
    ggbeeswarm::geom_beeswarm(
        aes(y = svi, x = doses_pop_100_D1_age_sex, 
            fill = region, size = total_pop),
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
                 aes(y = svi, x = dose_median), size = plot_params[[7]], width = 3) +
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

ggsave("output/plot_bee_svi_comb_box_bee_svi_idhm_quartil_D1_single.png",
       plot_bee_svi_comb_box_bee,
       width = 12, height = 5, units = "in", dpi = 800)







################################################################################
#### Descriptive Table of doses/100 hab
library(gtsummary)

tbl_svi_descriptive <-
    df_city_vacc_stats_filter %>% 
    mutate(
        pop_cat = cut_number(total_pop, n = 4),
        dist_capital_cat = cut_number(dist_to_capital, n = 4),
        region = factor(region, 
                        levels = c("North", "Northeast", 
                                   "Central-West",
                                   "Southeast", "South")),
        # ln_pop_area = log(pop_area),
        # ln_total_hosp_adm_100k_age_sex_2020 = ifelse(is.infinite(log(total_hosp_adm_100k_age_sex_2020)), 0, log(total_hosp_adm_100k_age_sex_2020)),
        metro_region = factor(rm, levels = c(0, 1), labels = c("No", "Yes")),
        pop_size = cut_number(total_pop, n = 4)
        ) %>% 
    select(
        total_pop,
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
        svi
    ) %>%
    tbl_summary(
        by = "svi"
        ) %>% 
    add_overall()

flextable::save_as_docx(
    as_flex_table(tbl_svi_descriptive), 
    path = "output/desc_table_city_SVI_group_D1_single_idh_quartil.docx"
    )








################################################################################
#### Plots - vaccine doses and socioeconomic variables -----------
### HDI
plot_city_vacc_hdi <- 
    df_city_vacc_stats_filter %>% 
    drop_na() %>% 
    mutate(
        svi = factor(svi_group, 
                     levels = c(3, 2, 1),
                     labels = c("High HDI",
                                "Medium HDI",
                                "Low HDI")
                     )
        ) %>% 
    ggplot() +
    geom_point(aes(x = idhm, y = doses_pop_100_D1_age_sex, color = factor(svi)), size = 0.5) +
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




# Modelling association ---------------------------------------------------

### Negative Binomial model
library(rms)
library(splines)

df_model_vacc_vars <- 
    df_city_vacc_stats_filter %>%
    mutate(
        y = round(doses_pop_100_D1_age_sex * total_pop/100), ## Obtaining age-and-sex adjusted counts
        
        idhm = idhm * 10,
        pricare_cov_10 = pricare_cov / 10,

        pricare_cov_hab_mean = pricare_cov_hab - mean(pricare_cov_hab),
        quartil_idhm = factor(svi, levels = c("High HDI", "Medium HDI", "Low HDI")),
        region = factor(region,
                        levels = c("Northeast", "North", "Central-West",
                                   "Southeast", "South")),
        codigo_ibge_6 = as.character(codigo_ibge_6),
        ln_pop_area = log(pop_area),
        ln_total_hosp_adm_100k_age_sex_2020 = ifelse(is.infinite(log(total_hosp_adm_100k_age_sex_2020)), 0, log(total_hosp_adm_100k_age_sex_2020)),
        metro_region = factor(rm, levels = c(0, 1), labels = c("No", "Yes")),
        pop_size = cut_number(total_pop, n = 4)
        ) %>% 
    filter(!is.na(svi)) 


#### NB model
## Estimating RCS knots for modelling
# knot_pop  <- rcspline.eval(df_city_vacc_stats_filter$total_pop, knots.only = TRUE)
knot_dist <- rcspline.eval(df_city_vacc_stats_filter$dist_to_capital, knots.only = TRUE)
knot_gini <- rcspline.eval(df_city_vacc_stats_filter$gini, knots.only = TRUE)
# knot_pop_area <- rcspline.eval(df_city_vacc_stats_filter$pop_area, knots.only = TRUE)
# knot_cases <- rcspline.eval(df_city_vacc_stats_filter$confirmed_cases_100k_2020, knots.only = TRUE)

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
            # rcs(pop_area, parms = knot_pop_area) +
            # rcs(total_hosp_adm_100k_age_sex_2020, parms = knot_cases) +
            pop_size +
            # ns(gini) +
            # ns(dist_to_capital) +
            rcs(gini, parms = knot_gini) +
            rcs(dist_to_capital, parms = knot_dist) +
            region +
            offset(log(total_pop))
        , data = df_model_vacc_vars %>%
            mutate(
                pricare_cov_10_var = pricare_cov_10 - mean(pricare_cov_10)
            ) %>%
            select(quartil_idhm, pricare_cov_10_var, metro_region, ln_pop_area, ln_total_hosp_adm_100k_age_sex_2020,
                   gini, dist_to_capital, region, total_pop, y, type_urban, pop_size, pop_area,
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
                           params = c("knot_pop", "knot_dist", "knot_gini"))

avg_city_pop <- mean(df_model_vacc_vars$total_pop[!is.na(df_model_vacc_vars$svi)])


model_predict_rates <- 
    model_predict %>% 
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
        x     = "Primary healthcare care coverage (%)",
        y     = "First doses per 100 people"
        ) + 
    theme_classic() +
    theme(
        legend.position = "top"
        )

ggsave("output/plot_adj_pred_interaction_D1_single_new.pdf", 
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
                                terms = c("total_pop [quart2]", "quartil_idhm"), 
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
                    # rcs(total_pop, parms = knot_pop) +
                    pop_size +
                    rcs(gini, parms = knot_gini) +
                    rcs(dist_to_capital, parms = knot_dist) +
                    region +
                    offset(log(total_pop)),
                
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
        x = "Primary healthcare care coverage (%)",
        y = "Rate Ratio (95% Confidence Interval)"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )


ggsave("output/plot_margins_RR_groupHDI_D1_single.png", plot_margins_RR_groupHDI,
       units = "in", dpi = 800, width = 7, height = 5)





















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
            offset(log(total_pop)),
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


writexl::write_xlsx(df_model_vacc_idhm_estimates, "output/df_model_vacc_idhm_estimates.xlsx")




#### Calculating Marginal Means for interaction PHC coverage vs HDI
library(ggeffects)
library(splines)


pricare_terms <- c(0, 2.5, 5, 7.5, 10) - mean(df_model_vacc_vars$pricare_cov_10)
idhm_terms <- c(4, 5, 6, 7, 8) - mean(df_model_vacc_vars$idhm)


avg_city_pop <- mean(df_model_vacc_vars$total_pop[!is.na(df_model_vacc_vars$svi)])

model_predict_idhm <- ggemmeans(model_vacc_idhm,
                                terms = c("pricare_cov_10_var [pricare_terms]", "idhm_10 [idhm_terms]"),
                                params = c("knot_pop", "knot_dist", "knot_gini"))

model_predict_rates_idhm <- 
    model_predict_idhm %>% 
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
        x     = "Primary healthcare care coverage (%)",
        y     = "First doses per 100 people"
    ) + 
    theme_classic() +
    theme(
        legend.position = "top"
    )

ggsave("output/plot_adj_pred_interaction_idhm.png", 
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
                    offset(log(total_pop)),    
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


writexl::write_xlsx(model_vacc_idhm_cob_range, "output/Statistics/model_vacc_idhm_cob_range.xlsx")





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
        x = "Primary healthcare care coverage (%)",
        y = "Rate Ratio (95% Confidence Interval)"
    ) +
    theme_classic() +
    theme(
        legend.position = "top"
    )


ggsave("output/plot_margins_RR_HDI_D1_single.png", plot_margins_RR_HDI,
       units = "in", dpi = 800, width = 7, height = 5)



