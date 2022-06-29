################################################################################
####### Data Preparation - Variables per Cities - HCW and Comorbidities
################################################################################

# Libraries ---------------------------------------------------------------
library(tidyverse)

# Data Input --------------------------------------------------------------
### BR cities - general info + socioeconomic info
df_br_cities_info <- 
    read_csv("input/df_br_cities_info.csv")


### Population per city and Sex 
df_population_city_sex <- 
    data.table::fread("input/df_population_city_sex.csv", na.strings = c("NA", "")) %>% 
    as_tibble()


### Population per city
df_population_city <-
    df_population_city_sex %>%
    group_by(uf, cod_ibge, nome, idade_grupo) %>%
    summarise(
        population = sum(population)
    ) %>%
    ungroup()


### COVID-19 confirmed cases (Brasil.io)
# df_covid_cases_city <-
#     data.table::fread("input/df_covid_cases_city_2021-08-31.csv.gz",
#                       na.strings = c("NA", ""), encoding = "UTF-8") %>%
#     as_tibble()


### Vaccine Data

# All vaccine doses - healthcare workers
df_vaccine_br_city_age_group <- 
    data.table::fread("input/df_vaccine_br_city_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
    as_tibble() %>% 
    filter(categoria_grupo == "age_group") %>%
    filter(idade_grupo %in% c("60-69", "70-79", "80+")) %>%
    mutate(vacina_dose_agg = vacina_dose1_agg) %>% ## Adding D1 + Single
    group_by(uf, cidade_resid_ibge, sexo, idade_grupo, vacina, vacina_dose_agg) %>% 
    summarise(
        total = sum(total)
    ) %>% 
    ungroup() %>% 
    filter(!is.na(cidade_resid_ibge))


## SIVEP Data - All period
df_covid_sivep <-
    left_join(
        data.table::fread("input/df_sivep_covid_2021_08_31.csv.gz", na.strings = c("NA", "")) %>%
            as_tibble() %>%
            group_by(CO_MUN_RES, idade_grupo, CS_SEXO) %>%
            summarise(
                hosp_adm = n()
            ),
        data.table::fread("input/df_sivep_covid_2021_08_31.csv.gz", na.strings = c("NA", "")) %>%
            as_tibble() %>%
            filter(EVOLUCAO == "Death") %>%
            group_by(CO_MUN_RES, idade_grupo, CS_SEXO) %>%
            summarise(
                hosp_death = n()
            )
    )    %>%
    ungroup() %>%
    mutate(
        hosp_adm = if_else(is.na(hosp_adm), 0L, hosp_adm),
        hosp_death = if_else(is.na(hosp_death), 0L, hosp_death),
        CS_SEXO = stringr::str_to_lower(CS_SEXO)
    )


## SIVEP Data - Pre-vaccination 
df_covid_sivep_pre_vacc <-
    left_join(
        data.table::fread("input/df_sivep_covid_2021_08_31.csv.gz", na.strings = c("NA", "")) %>%
            filter(date_sint < "2021-01-01") %>%
            as_tibble() %>%
            group_by(CO_MUN_RES, idade_grupo, CS_SEXO) %>%
            summarise(
                hosp_adm = n()
            ),
        data.table::fread("input/df_sivep_covid_2021_08_31.csv.gz", na.strings = c("NA", "")) %>%
            filter(date_sint < "2021-01-01") %>%
            as_tibble() %>%
            filter(EVOLUCAO == "Death") %>%
            group_by(CO_MUN_RES, idade_grupo, CS_SEXO) %>%
            summarise(
                hosp_death = n()
            )
    )    %>%
    ungroup() %>%
    mutate(
        hosp_adm = if_else(is.na(hosp_adm), 0L, hosp_adm),
        hosp_death = if_else(is.na(hosp_death), 0L, hosp_death),
        CS_SEXO = stringr::str_to_lower(CS_SEXO)
    )



# Data Preparation -----------------------------------------------------------

################################################################################
#### Total population per city (using city codes as reference for further DFs)
df_br_cities_pop <-
    df_population_city_sex %>% 
    group_by(cod_ibge) %>% 
    summarise(
        total_pop = sum(population)
    )

#### Adult population per City 
# df_br_cities_pop_adults <-
#     df_population_city_sex %>% 
#     filter(!(idade_grupo %in% c("0-4", "5-9", "10-14", "15-19"))) %>% 
#     group_by(cod_ibge) %>% 
#     summarise(
#         population_adult_2020 = sum(population)
#     )


################################################################################
#### Total doses per city
df_dose_city <-
    df_vaccine_br_city_age_group %>%
    group_by(uf, cidade_resid_ibge, vacina_dose_agg) %>%
    summarise(
        total_doses = sum(total)
    ) %>%
    pivot_wider(names_from = "vacina_dose_agg", values_from = "total_doses") %>%
    ungroup() %>%
    mutate(
        D1_single_new = if_else(is.na(D1_single) & !is.na(D2), D2, D1_single),
        D2 = if_else(is.na(D1_single) & !is.na(D2), D1_single, D2)
    ) %>%
    mutate(
        D1_single = D1_single_new,
        D1_single = if_else(is.na(D1_single), 0L, D1_single),
        D2 = if_else(is.na(D2), 0L, D2)
    ) %>%
    select(-D1_single_new)



################################################################################
#### Total doses per city - age and sex adjusted - comorbidities 

## Total doses per city age and sex
df_dose_city_age_sex_group <- 
    df_vaccine_br_city_age_group %>% 
    mutate(
        total_D1 = if_else(vacina_dose_agg == "D1_single", total, 0L),
        total_D2 = if_else(vacina_dose_agg == "D2", total, 0L),
        total_D1_D2 = total
    ) %>% 
    group_by(cidade_resid_ibge, sexo, idade_grupo) %>%
    summarise(
        total_doses_D1    = sum(total_D1),
        total_doses_D2    = sum(total_D2), # D2 + Single
        total_doses_D1_D2 = sum(total_D1_D2)
    ) %>%
    ungroup()



### Adjusting by age and sex from the BR population distribution
df_dose_city_age_sex_adj_group <- 
    df_population_city_sex %>%
    filter(idade_grupo %in% c("60-69", "70-79", "80+")) %>%
    group_by(cod_ibge, sexo, idade_grupo) %>%
    summarise(
        city_total_population = sum(population)
    ) %>% 
    ungroup() %>% 
    left_join(
        df_population_city_sex %>% 
            group_by(sexo, idade_grupo) %>% 
            summarise(
                br_total_population = sum(population)
            )
        , by = c("sexo" = "sexo",
                 "idade_grupo" = "idade_grupo")
    ) %>% 
    left_join(
        df_dose_city_age_sex_group
        , by = c("cod_ibge" = "cidade_resid_ibge", 
                 "sexo" = "sexo",
                 "idade_grupo" = "idade_grupo")
    ) %>% 
    mutate(
        total_doses_D1    = if_else(is.na(total_doses_D1), 0L, total_doses_D1),
        total_doses_D2    = if_else(is.na(total_doses_D2), 0L, total_doses_D2),
        total_doses_D1_D2 = if_else(is.na(total_doses_D1_D2), 0L, total_doses_D1_D2)
    ) %>% 
    filter(!is.na(sexo)) %>% 
    mutate(
        doses_pop_D1    = total_doses_D1    / city_total_population,
        doses_pop_D2    = total_doses_D2    / city_total_population,
        doses_pop_D1_D2 = total_doses_D1_D2 / city_total_population
    ) %>%
    mutate(
        doses_pop_exp_D1    = doses_pop_D1    * br_total_population,
        doses_pop_exp_D2    = doses_pop_D2    * br_total_population,
        doses_pop_exp_D1_d2 = doses_pop_D1_D2 * br_total_population
    ) %>%
    group_by(cod_ibge) %>%
    summarise(
        city_total_pop = sum(city_total_population),
        br_total_pop   = sum(br_total_population),
        
        doses_D1_total    = sum(total_doses_D1),
        doses_D2_total    = sum(total_doses_D2),
        doses_D1_D2_total = sum(total_doses_D1_D2),
        
        doses_exp_D1_total    = sum(doses_pop_exp_D1),
        doses_exp_D2_total    = sum(doses_pop_exp_D2),
        doses_exp_D1_d2_total = sum(doses_pop_exp_D1_d2)
    ) %>%
    ungroup() %>% 
    mutate(
        doses_pop_100_D1_crude   = (doses_D1_total     / city_total_pop) * 100,
        doses_pop_100_D1_age_sex = (doses_exp_D1_total / br_total_pop) * 100,
        
        doses_pop_100_D2_crude   = (doses_D2_total     / city_total_pop) * 100,
        doses_pop_100_D2_age_sex = (doses_exp_D2_total / br_total_pop) * 100,
        
        doses_pop_100_D1_D2_crude   = (doses_D1_D2_total     / city_total_pop) * 100,
        doses_pop_100_D1_D2_age_sex = (doses_exp_D1_d2_total / br_total_pop) * 100
    ) %>% 
    select(-c(city_total_pop, br_total_pop))



################################################################################
#### Total number of COVID_19 confirmed cases and deaths per population for each city
# df_covid_notif_br <-
#     df_covid_cases_city %>%
#     group_by(city_ibge_code, city) %>%
#     summarise(
#         confirmed_cases  = sum(new_confirmed),
#         confirmed_deaths = sum(new_deaths)
#     ) %>%
#     mutate(
#         city_ibge_code_6 = as.numeric(stringr::str_sub(city_ibge_code, 1, 6))
#     ) %>%
#     ungroup() %>%
#     left_join(
#         df_population_city %>%
#             group_by(city = cod_ibge) %>%
#             summarise(
#                 total_pop = sum(population)
#             )
#         , by = c("city_ibge_code_6" = "city")
#     ) %>%
#     mutate(
#         confirmed_cases_100k  = (confirmed_cases  / total_pop) * 100000,
#         confirmed_deaths_100k = (confirmed_deaths / total_pop) * 100000
#     ) %>%
#     select(city_ibge_code_6, confirmed_cases, confirmed_deaths, 
#            confirmed_cases_100k, confirmed_deaths_100k)
# 
# 
# df_covid_notif_br_pre_vacc <-
#     df_covid_cases_city %>%
#     filter(date < as.Date("2021-01-01")) %>% 
#     group_by(city_ibge_code, city) %>%
#     summarise(
#         confirmed_cases_2020  = sum(new_confirmed),
#         confirmed_deaths_2020 = sum(new_deaths)
#     ) %>%
#     mutate(
#         city_ibge_code_6 = as.numeric(stringr::str_sub(city_ibge_code, 1, 6))
#     ) %>%
#     ungroup() %>%
#     left_join(
#         df_population_city %>%
#             group_by(city = cod_ibge) %>%
#             summarise(
#                 total_pop = sum(population)
#             )
#         , by = c("city_ibge_code_6" = "city")
#     ) %>%
#     mutate(
#         confirmed_cases_100k_2020  = (confirmed_cases_2020  / total_pop) * 100000,
#         confirmed_deaths_100k_2020 = (confirmed_deaths_2020 / total_pop) * 100000
#     ) %>%
#     select(city_ibge_code_6, confirmed_cases_2020, confirmed_deaths_2020, 
#            confirmed_cases_100k_2020, confirmed_deaths_100k_2020)
# 




################################################################################
#### Total number of COVID_19 confirmed cases and deaths per population for each city

df_covid_adm_br <-
    df_population_city_sex %>%
    filter(!(idade_grupo %in% c("0-4", "5-9",
                                "10-14", "15-19"))) %>%
    group_by(cod_ibge, sexo, idade_grupo) %>%
    summarise(
        city_total_population = sum(population)
    ) %>%
    ungroup() %>%
    left_join(
        df_population_city_sex %>%
            filter(!(idade_grupo %in% c("0-4", "5-9",
                                        "10-14", "15-19"))) %>%
            group_by(sexo, idade_grupo) %>%
            summarise(
                br_total_population = sum(population)
            )
        , by = c("sexo" = "sexo",
                 "idade_grupo" = "idade_grupo")
    ) %>%
    left_join(
        df_covid_sivep
        , by = c("cod_ibge" = "CO_MUN_RES",
                 "sexo" = "CS_SEXO",
                 "idade_grupo" = "idade_grupo")
    ) %>%
    mutate(
        hosp_adm = if_else(is.na(hosp_adm), 0L, hosp_adm),
        hosp_death = if_else(is.na(hosp_death), 0L, hosp_death)
    ) %>%
    filter(!is.na(sexo)) %>%
    mutate(
        hosp_adm_pop   = hosp_adm / city_total_population,
        hosp_death_pop = hosp_death / city_total_population
    ) %>%
    mutate(
        hosp_adm_pop_exp   = hosp_adm_pop * br_total_population,
        hosp_death_pop_exp = hosp_death_pop * br_total_population
    ) %>%
    group_by(cod_ibge) %>%
    summarise(
        city_total_pop   = sum(city_total_population),

        total_hosp_adm   = sum(hosp_adm),
        total_hosp_death = sum(hosp_death),

        total_hosp_adm_100k_crude     = (sum(hosp_adm)           / sum(city_total_population)) * 100000,
        total_hosp_adm_100k_age_sex   = (sum(hosp_adm_pop_exp)   / sum(br_total_population)) * 100000,

        total_hosp_death_100k_crude   = (sum(hosp_death)         / sum(city_total_population)) * 100000,
        total_hosp_death_100k_age_sex = (sum(hosp_death_pop_exp) / sum(br_total_population)) * 100000
    ) %>%
    ungroup() %>%
    select(-c(city_total_pop)) %>%
    select(cod_ibge, total_hosp_adm_100k_age_sex, total_hosp_death_100k_age_sex)


df_covid_adm_br_pre_vacc <-
    df_population_city_sex %>%
    filter(!(idade_grupo %in% c("0-4", "5-9",
                                "10-14", "15-19"))) %>%
    group_by(cod_ibge, sexo, idade_grupo) %>%
    summarise(
        city_total_population = sum(population)
    ) %>%
    ungroup() %>%
    left_join(
        df_population_city_sex %>%
            filter(!(idade_grupo %in% c("0-4", "5-9",
                                        "10-14", "15-19"))) %>%
            group_by(sexo, idade_grupo) %>%
            summarise(
                br_total_population = sum(population)
            )
        , by = c("sexo" = "sexo",
                 "idade_grupo" = "idade_grupo")
    ) %>%
    left_join(
        df_covid_sivep_pre_vacc
        , by = c("cod_ibge" = "CO_MUN_RES",
                 "sexo" = "CS_SEXO",
                 "idade_grupo" = "idade_grupo")
    ) %>%
    mutate(
        hosp_adm = if_else(is.na(hosp_adm), 0L, hosp_adm),
        hosp_death = if_else(is.na(hosp_death), 0L, hosp_death)
    ) %>%
    filter(!is.na(sexo)) %>%
    mutate(
        hosp_adm_pop   = hosp_adm / city_total_population,
        hosp_death_pop = hosp_death / city_total_population
    ) %>%
    mutate(
        hosp_adm_pop_exp   = hosp_adm_pop * br_total_population,
        hosp_death_pop_exp = hosp_death_pop * br_total_population
    ) %>%
    group_by(cod_ibge) %>%
    summarise(
        city_total_pop   = sum(city_total_population),

        total_hosp_adm   = sum(hosp_adm),
        total_hosp_death = sum(hosp_death),

        total_hosp_adm_100k_crude     = (sum(hosp_adm)           / sum(city_total_population)) * 100000,
        total_hosp_adm_100k_age_sex_2020   = (sum(hosp_adm_pop_exp)   / sum(br_total_population)) * 100000,

        total_hosp_death_100k_crude   = (sum(hosp_death)         / sum(city_total_population)) * 100000,
        total_hosp_death_100k_age_sex_2020 = (sum(hosp_death_pop_exp) / sum(br_total_population)) * 100000
    ) %>%
    ungroup() %>%
    select(-c(city_total_pop)) %>%
    select(cod_ibge, total_hosp_adm_100k_age_sex_2020, total_hosp_death_100k_age_sex_2020)




###############################################################################
#### General dataset per city

## Merging tables
df_city_vacc_sivep_info <- 
    df_br_cities_info %>% 
    # left_join(
    #     df_br_cities_pop
    #     , by = c("codigo_ibge_6" = "cod_ibge")
    # ) %>%
    # left_join(
    #     df_br_cities_pop_adults
    #     , by = c("codigo_ibge_6" = "cod_ibge")
    # ) %>%
    # left_join(
    #     df_dose_city %>%
    #         select(-uf)
    #     , by = c("codigo_ibge_6" = "cidade_resid_ibge")
    # ) %>%
    # left_join(
    #     df_dose_city_hcw %>% 
    #         select(-uf)
    #     , by = c("codigo_ibge_6" = "cidade_resid_ibge")
    # ) %>% 
    left_join(
        df_dose_city_age_sex_adj_group
        , by = c("codigo_ibge_6" = "cod_ibge")
    ) %>% 
    # mutate(
    #     D1_cov_adult        = D1 / total_pop_adult,
    #     D2_single_cov_adult = D2_single / total_pop_adult,
    #     
    #     D1_cov        = D1 / total_pop,
    #     D2_single_cov = D2_single / total_pop
    # ) %>% 
    left_join(
        df_covid_adm_br
        , by = c("codigo_ibge_6" = "cod_ibge")
    ) %>%
    left_join(
        df_covid_adm_br_pre_vacc
        , by = c("codigo_ibge_6" = "cod_ibge")
    ) %>%
    # left_join(
    #     df_covid_notif_br
    #     , by = c("codigo_ibge_6" = "city_ibge_code_6")
    # ) %>%
    # left_join(
    #     df_covid_notif_br_pre_vacc
    #     , by = c("codigo_ibge_6" = "city_ibge_code_6")
    # ) %>%
    mutate(
        region = case_when(
            uf %in% c("SP", "RJ", "ES", "MG") ~ "Southeast",
            uf %in% c("SC", "RS", "PR") ~ "South",
            uf %in% c("MT", "MS", "GO", "DF") ~ "Central-West",
            uf %in% c("AM", "AP", "TO", "PA", "RO", "RR", "AC") ~ "North",
            uf %in% c("BA", "AL", "SE", "PE", "MA", "RN", "PB", "CE", "PI") ~ "Northeast"
        )
    ) %>% 
    mutate(
        doses_pop_100_D1_age_sex = if_else(doses_pop_100_D1_age_sex > 100, 100, doses_pop_100_D1_age_sex),
        hdi_group = cut_number(idhm, 3, labels = FALSE),
        hdi = factor(hdi_group, 
                     levels = c(1, 2, 3),
                     labels = c("Low HDI",
                                "Medium HDI",
                                "High HDI")
        )
    ) 






# Comparing doses per 100 people and HDI ----------------------------------

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
        ln_hcw_100k = ifelse(is.infinite(log(hcw_100k)), 0, log(hcw_100k)),
        metro_region = factor(rm, levels = c(0, 1), labels = c("No", "Yes")),
        pop_size = cut_number(population_2020, n = 4)
    ) %>% 
    filter(!is.na(hdi)) 


#### NB model
## Estimating RCS knots for modelling
knot_dist <- rcspline.eval(df_model_vacc_vars$dist_to_capital, knots.only = TRUE)
knot_gini <- rcspline.eval(df_model_vacc_vars$gini, knots.only = TRUE)


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


writexl::write_xlsx(df_model_vacc_idhm_quartil_estimates, "output/df_sensitivity_model_vacc_idhm_quartil_60y.xlsx")





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

ggsave("output/plot_sensitivity_adj_pred_interaction_60y.png", 
       plot_adj_pred_interaction, 
       units = "in", dpi = 800, width = 7, height = 5)


# writexl::write_xlsx(
#     model_predict_rates %>% 
#         mutate(
#             predicted_ci = paste0(round(predicted, 1), " (",
#                                   round(conf.low, 1), "-",
#                                   round(conf.high, 1), ")"
#             )
#         ) %>% 
#         as_tibble() %>% 
#         select(phc_coverage_10 = x, group, predicted_ci) %>% 
#         pivot_wider(names_from = "group", values_from = "predicted_ci"),
#     
#     "output/model_predict_quart_idhm.xlsx")







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

writexl::write_xlsx(model_vacc_idhm_quartil_cob_range, 
                    "output/Statistics/df_sensitivity_model_vacc_idhm_quartil_cob_range_age_group_60y.xlsx")


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


ggsave("output/plot_sensitivity_margins_RR_groupHDI_D1_single_60y.png", plot_margins_RR_groupHDI,
       units = "in", dpi = 800, width = 7, height = 5)

