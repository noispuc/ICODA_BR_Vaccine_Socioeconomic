################################################################################
####### Data Analysis - Pandemic Curves and vaccines - Vaccine Descriptive
################################################################################
Sys.setlocale(category = "LC_ALL", locale = "english")

# Libraries ---------------------------------------------------------------
library(tidyverse)

# date_limit <- "2021-08-31"

# Data Input --------------------------------------------------------------
### Population per city - per Sex
df_population_city_sex <- 
    data.table::fread("input/df_population_city_sex.csv", na.strings = c("NA", "")) %>% 
    as_tibble()

### Population per city (PNAD) - All sexes
df_population_city <- 
    df_population_city_sex %>% 
    group_by(uf, cod_ibge, nome, idade_grupo) %>% 
    summarise(
        population = sum(population)
        ) %>% 
    ungroup()


br_population_total <- 
    df_population_city_sex %>% 
    summarise(
        population = sum(population)
        ) %>% 
    pull()

br_population_adults <- 
    df_population_city_sex %>% 
    filter(
        !(idade_grupo %in% c("0-4", "5-19", "10-14", "15-19"))
        ) %>%
    summarise(
        population = sum(population)
        ) %>% 
    pull()

### COVID-19 confirmed cases (Brasil.io)
# df_covid_cases_city <- 
#     data.table::fread("input/df_covid_cases_city_2021-08-31.csv", 
#                       na.strings = c("NA", ""), encoding = "UTF-8") %>% 
#     as_tibble() 
    # %>% 
    # filter(!is.na(city_ibge_code))

    
### Vaccine Data
df_vaccine_br_uf <- 
    data.table::fread("input/df_vaccine_br_city_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
    as_tibble() %>% 
    group_by(uf, data_aplicacao, sexo, idade_grupo, vacina, vacina_dose_agg) %>% ## Keeps D1 and D2 + single separated
    summarise(
        total = sum(total)
    ) %>% 
    ungroup()


df_sivep_covid_daily <- 
    read_csv("input/df_sivep_covid_daily_2021_08_31.csv.gz")




# df_sivep_covid_daily %>% 
#     filter(date >= '2021-01-01') %>% 
#     summarise(
#         total_adm = sum(hosp_adm),
#         total_deaths = sum(hosp_deaths)
#     )



# Data Analysis -----------------------------------------------------------

################################################################################
#### (A) Cases per 100,000 population vs Vaccine Coverage per day ####
df_covid_hosp_adm_vacc_br <-
    df_sivep_covid_daily %>%
    mutate(
        hosp_adm_100k = (hosp_adm / sum(br_population_adults)) * 100000
    ) %>% 
    mutate(
        hosp_adm_100k_MM7 = zoo::rollmean(hosp_adm_100k, k = 7,
                                          fill = NA, align = "right")
    ) %>% 
    mutate(date = as.Date(date)) %>%
    left_join(
        df_vaccine_br_uf %>%
            mutate(data_aplicacao = as.Date(data_aplicacao)) %>%
            group_by(data_aplicacao, vacina_dose_agg) %>%
            summarise(
                total_doses = sum(total)
            ) %>%
            ungroup() %>%
            pivot_wider(names_from = "vacina_dose_agg", values_from = "total_doses")
        , by = c("date" = "data_aplicacao")
    ) %>%
    arrange(date) %>%
    mutate(
        D1_acum = cumsum(if_else(is.na(D1), 0L, D1)),
        D2_single_acum = cumsum(if_else(is.na(D2_single), 0L, D2_single)),

        D1_cov = D1_acum / br_population_total,
        D2_single_cov = D2_single_acum / br_population_total
    ) %>%
    mutate(
        D1_cov = if_else(date >= as.Date("2021-01-17"), D1_cov, NA_real_),
        D2_single_cov = if_else(date >= as.Date("2021-01-17"), D2_single_cov, NA_real_)
    )

# df_events_text <- 
#     tibble(
#         date  = as.Date(c("2021-02-15", "2021-08-11")),
#         event = c("Gamma variant dominance", 
#                   "Delta variant dominance"),
#         y = c(50, 50)
#     )

df_events_text <- 
    tibble(
        date  = as.Date(c("2021-02-15", "2021-07-01")),
        event = c("Gamma variant dominance", "Delta variant introduction"),
        y = c(6, 7)
    )

f_scale <- max(df_covid_hosp_adm_vacc_br$hosp_adm_100k, na.rm = T) / 
    max(df_covid_hosp_adm_vacc_br$D1_cov, na.rm = T)


plot_df_covid_hosp_adm_vacc_br <-
    df_covid_hosp_adm_vacc_br %>% 
    ggplot() +
    geom_rect(aes(xmin = as.Date("2021-01-14"), 
                  xmax = as.Date("2021-08-31"), 
                  ymin = 0, ymax = Inf), 
              fill = "lightyellow", alpha = 0.5) +
    geom_col(aes(x = date, y = hosp_adm_100k), width = 1, color = NA, linetype = 0, alpha = 0.5) +
    geom_line(aes(x = date, y = hosp_adm_100k_MM7, 
                  color = "Hospital admissions\n/100,000 pop.\n(Moving average)"), size = 1) +
    geom_line(aes(x = date, y = D1_cov * f_scale, color = "Vaccine: At least\none dose"), size = 1) +
    geom_line(aes(x = date, y = D2_single_cov * f_scale, color = "Vaccine: Two doses"), size = 1) +
    geom_label(data = df_events_text, aes(x = date - 45, y = y, label = event), size = 3.5) +
    geom_segment(data = df_events_text, aes(x = date, xend = date, 
                                            y = 0, yend = y-0.3), linetype = "dashed") +
    scale_y_continuous(
        limits = c(0, 8),
        breaks = seq(0, 8, 2),
        sec.axis = sec_axis( ~ . / f_scale, 
                             labels = scales::percent_format(),
                             name = "Vaccine Coverage (%)", breaks = seq(0, 0.60, 0.2))
        ) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b/%y") +
    scale_color_manual(values = c("Black", "Blue", "Dark Green"), name = "") +
    labs(
        x = "",
        y = "Hospital admissions per 100,000 population"
        ) +
    theme_classic() +
    theme(
        legend.position = "top"
        )







################################################################################
#### (B) Vaccine coverage per age group ####

### Calculate total doses per age and sex 
df_vaccine_br_age <- 
    df_vaccine_br_uf %>% 
    bind_rows(
        df_vaccine_br_uf %>% 
            mutate(idade_grupo = "All")
    ) %>% 
    filter(!is.na(sexo)) %>% 
    group_by(idade_grupo, vacina_dose_agg) %>% 
    summarise(
        total_doses = sum(total)
        ) %>% 
    ungroup() %>% 
    pivot_wider(names_from = "vacina_dose_agg", values_from = "total_doses") %>% 
    left_join(
        df_population_city_sex %>% 
            bind_rows(
                df_population_city_sex %>% 
                    mutate(idade_grupo = "All")
            ) %>%
            group_by(idade_grupo) %>% 
            summarise(
                total_population = sum(population)
            ) %>% 
            ungroup()
        , by = c("idade_grupo" = "idade_grupo")
        ) %>% 
    mutate(
        D1_wo_D2 = D1 - D2_single,
        pop_wo_vacc = total_population - D1,
        
        D1_cov = (D1 / total_population),
        D2_single_cov = (D2_single / total_population)
        ) %>% 
    mutate(
        pop_wo_vacc = if_else(pop_wo_vacc < 0, 0L, pop_wo_vacc)
        ) %>% 
    mutate(
        D1_wo_D2_cov = D1_wo_D2 / total_population,
        pop_wo_vacc_cov = pop_wo_vacc / total_population
        ) %>% 
    mutate(
        idade_grupo = factor(idade_grupo,
                             levels = c("All", "0-4", "5-9", "10-14", "15-19", "20-29", 
                                        "30-39", "40-49", "50-59", "60-69", "70-79", 
                                        "80+")
                             )
        ) %>% 
    arrange(idade_grupo)




### Calculate total doses per age and sex 
df_vaccine_br_age_sex <- 
    df_vaccine_br_uf %>%
    bind_rows(
        df_vaccine_br_uf %>% 
            mutate(idade_grupo = "All")
    ) %>% 
    filter(!is.na(sexo)) %>% 
    group_by(sexo, idade_grupo, vacina_dose_agg) %>% 
    summarise(
        total_doses = sum(total)
    ) %>% 
    ungroup() %>% 
    pivot_wider(names_from = "vacina_dose_agg", values_from = "total_doses") %>% 
    left_join(
        df_population_city_sex %>% 
            bind_rows(
                df_population_city_sex %>% 
                    mutate(idade_grupo = "All")
            ) %>% 
            group_by(idade_grupo, sexo) %>% 
            summarise(
                total_population = sum(population)
            ) %>% 
            ungroup()
        , by = c("sexo" = "sexo",
                 "idade_grupo" = "idade_grupo")
    ) %>% 
    mutate(
        D1_wo_D2 = D1 - D2_single,
        pop_wo_vacc = total_population - D1,
        
        D1_cov = (D1 / total_population),
        D2_single_cov = (D2_single / total_population)
    ) %>% 
    mutate(
        pop_wo_vacc = if_else(pop_wo_vacc < 0, 0L, pop_wo_vacc)
    ) %>% 
    mutate(
        D1_wo_D2_cov = D1_wo_D2 / total_population,
        pop_wo_vacc_cov = pop_wo_vacc / total_population
    ) %>% 
    mutate(
        idade_grupo = factor(idade_grupo,
                             levels = c("All", "0-4", "5-9", "10-14", "15-19", "20-29", 
                                        "30-39", "40-49", "50-59", "60-69", "70-79", 
                                        "80+" )
                             )
        ) %>% 
    arrange(sexo, idade_grupo)



writexl::write_xlsx(
    df_vaccine_br_age_sex %>% 
        bind_rows(
            df_vaccine_br_age
        ) %>% 
        mutate(
            D1_cov_label = paste0(D1, "/", total_population, " (", round(100 * D1_cov, 2), "%)"),
            D2_single_cov_label = paste0(D2_single, "/", total_population, " (", round(100 * D2_single_cov, 2), "%)")
        ) %>% 
        select(sexo, idade_grupo, total_population, D1_cov_label, D2_single_cov_label) 
    , "output/Statistics/vaccine_coverage_age_sex.xlsx"
    )


df_age_gender_text <- 
    tibble(
        x = c("80+", "80+"),
        y =  c(-10e6, 10e6),
        label = c("Female", "Male")
    )

plot_vaccine_br_age_sex <-
    df_vaccine_br_age_sex %>% 
    filter(idade_grupo != "All") %>% 
    select(sexo, idade_grupo, D1_wo_D2, D2_single, pop_wo_vacc) %>% 
    pivot_longer(-c(sexo, idade_grupo), names_to = "vacc_dose", values_to = "coverage") %>% 
    mutate(
        vacc_dose = factor(vacc_dose, 
                           levels = c("pop_wo_vacc", "D1_wo_D2", "D2_single"),
                           labels = c("Estimated\nPopulation", "Partially\nvaccinated", "Fully\nvaccinated")
                           )
        ) %>% 
    mutate(
        coverage = if_else(sexo == "female", coverage, -coverage)
        ) %>%
    ggplot() +
    geom_col(aes(x = idade_grupo, y = coverage, fill = vacc_dose), width = 0.8) +
    geom_hline(aes(yintercept  = 0)) +
    geom_label(data = df_age_gender_text, aes(x = x, y = y, label = label)) +
    scale_y_continuous(
        breaks = c(-15e6, -10e6, -5e6, -2e6, 0, 2e6, 5e6, 10e6, 15e6),
        labels = c("15M", "10M", "5M", "2M", "0", "2M", "5M", "10M", "15M")
        ) +
    scale_fill_manual(name = "", values = c("#42B54033", "#42B54099", "#42B540FF")) +
    labs(
        x = "Age (years)",
        y = "Population"
        ) +
    coord_flip() +
    theme_classic() +
    theme(
        legend.position = "top",
        legend.key.size =  unit(x = 0.5, units = "cm")
        )
    



################################################################################
#### (C) Vaccine application rates per State (age-and-sex adj) ####

## Applied vaccine rates per age and sex rates - Brazil
df_vaccine_br_age_sex <- ## Includes single dose with D1
    data.table::fread("input/df_vaccine_br_city_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
    as_tibble() %>% 
    mutate(
        vacina_dose_agg = vacina_dose1_agg
    ) %>% 
    group_by(uf, data_aplicacao, sexo, idade_grupo, vacina, vacina_dose_agg) %>% ## Keeps D1 and D2 + single separated
    summarise(
        total = sum(total)
    ) %>% 
    ungroup() %>% 
    mutate(
        total_D1_single = if_else(vacina_dose_agg == "D1_single", total, 0L)
    ) %>% 
    group_by(sexo, idade_grupo) %>%
    summarise(
        total_doses = sum(total_D1_single)
    ) %>% 
    ungroup() %>% 
    left_join(
        df_population_city_sex %>% 
            group_by(sexo, idade_grupo) %>% 
            summarise(
                total_population = sum(population)
            ) %>% 
            ungroup()
        , by = c("sexo" = "sexo",
                 "idade_grupo" = "idade_grupo")
    ) %>% 
    filter(!is.na(sexo)) 


df_vaccine_br_age_sex_uf <-
    df_population_city_sex %>% 
    group_by(uf, sexo, idade_grupo) %>% 
    summarise(
        uf_total_population = sum(population)
    ) %>% 
    left_join(
        df_vaccine_br_age_sex %>% 
            select(sexo, idade_grupo, br_total_population = total_population)
    ) %>% 
    left_join(
        data.table::fread("input/df_vaccine_br_city_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
            as_tibble() %>% 
            mutate(
                vacina_dose_agg = vacina_dose1_agg
            ) %>% 
            group_by(uf, data_aplicacao, sexo, idade_grupo, vacina, vacina_dose_agg) %>% ## Keeps D1 and D2 + single separated
            summarise(
                total = sum(total)
            ) %>% 
            mutate(
                total_D1_single = if_else(vacina_dose_agg == "D1_single", total, 0L)
            ) %>% 
            group_by(uf, sexo, idade_grupo) %>%
            summarise(
                total_doses =  sum(total_D1_single)
            ) %>% 
            ungroup()
        , by = c("uf" = "uf", 
                 "sexo" = "sexo",
                 "idade_grupo" = "idade_grupo")
        ) %>% 
    filter(uf != "", !is.na(sexo)) %>% 
    mutate(
        total_doses = if_else(is.na(total_doses), 0L, total_doses)
    ) %>% 
    mutate(
        doses_pop = total_doses / uf_total_population
        ) %>% 
    mutate(
        doses_pop_exp = doses_pop * br_total_population
        ) %>% 
    group_by(uf) %>% 
    summarise(
        doses_pop_100_crude = (sum(total_doses) / sum(uf_total_population)) * 100,
        doses_pop_100_age_sex = (sum(doses_pop_exp) / sum(br_total_population)) * 100
    ) %>% 
    ungroup()


writexl::write_xlsx(df_vaccine_br_age_sex_uf, 
                    "output/Statistics/vaccine_doses_100_people_state.csv")


# library(geobr)
## Obtaining map file
# shp_state_map <- geobr::read_state()
# write_rds(shp_state_map, "input/shp_state_brazil_map.rds")

shp_state_map <- read_rds("data/shp_state_brazil_map.rds")

library(sf)



df_vaccine_br_age_sex_uf_map <- 
    shp_state_map %>% 
    left_join(
        df_vaccine_br_age_sex_uf %>% 
            select(uf, doses_pop_100_age_sex, doses_pop_100_crude)
        , by = c("abbrev_state" = "uf")
    )


plot_vaccine_br_age_sex_uf_map <- 
    df_vaccine_br_age_sex_uf_map %>% 
    ggplot() +
    geom_sf(aes(fill = doses_pop_100_age_sex)) +
    scale_fill_viridis_c(name = "Age-and-sex-adjusted\nfirst doses/100 people", 
                         direction = -1) +
    theme_void() +
    theme(legend.position = "top")




    
################################################################################
#### (D) Proportion of vaccine plataforms applied per epi week  ####

df_vacc_type_month <-
    data.table::fread("input/df_vaccine_br_city_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
    as_tibble() %>% 
    mutate(
        vacina_dose_agg = vacina_dose1_agg
    ) %>% 
    group_by(uf, data_aplicacao, sexo, idade_grupo, vacina, vacina_dose_agg) %>% ## Keeps D1 and D2 + single separated
    summarise(
        total = sum(total)
    ) %>% 
    mutate(
        mes_aplicacao = lubridate::month(data_aplicacao),
        # semana_aplicacao_cont = semana_aplicacao + 53 * (lubridate::year(data_aplicacao) - min(lubridate::year(data_aplicacao))),
        mes_label = paste0(lubridate::month(data_aplicacao, label = TRUE, abbr = TRUE), "/", 21)
    ) %>% 
    mutate(
        total_D1_single = if_else(vacina_dose_agg == "D1_single", total, 0L)
    ) %>% 
    group_by(mes_aplicacao, mes_label, vacina) %>% 
    summarise(
        total_doses = sum(total_D1_single)
    ) %>% 
    ungroup() %>% 
    filter(
        vacina != "sem_identificacao"
    ) %>% 
    mutate(
        vacina_label = factor(
            vacina, 
            levels = rev(c("coronavac", "janssen", "astrazeneca","pfizer")),
            labels = rev(c("Sinovac-\nCoronaVac", "Ad26.COV2.S\n(Janssen)",
                           "ChAdOx1-S/nCoV-19\n(Oxford-AstraZeneca)", 
                           "BNT162b2\n(Pfizer/BioNTech)"))
            )
        )





df_vacc_type_month_alldoses <-
    data.table::fread("input/df_vaccine_br_city_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
    as_tibble() %>% 
    mutate(
        vacina_dose_agg = vacina_dose1_agg
        ) %>% 
    mutate(
        mes_aplicacao = lubridate::month(data_aplicacao),
        mes_label = paste0(lubridate::month(data_aplicacao, label = TRUE, abbr = TRUE), "/", 21)
        ) %>% 
    group_by(mes_aplicacao, mes_label, vacina, vacina_dose_agg) %>% ## Keeps D1 and D2 + single separated
    summarise(
        total = sum(total)
        ) %>% 
    ungroup()
    

df_vacc_type_month_alldoses_format <-
    bind_rows(
        df_vacc_type_month_alldoses %>% 
            group_by(mes_aplicacao, vacina_dose_agg) %>% 
            mutate(
                doses_label = paste0(format(total, big.mark = ","), " (", round(100 * total / sum(total), 2),"%)")
            ) %>% 
            ungroup()
            ,
        df_vacc_type_month_alldoses %>%
            mutate(
                vacina = vacina_dose_agg
                ) %>%
            group_by(mes_aplicacao, mes_label, vacina, vacina_dose_agg) %>%
            summarise(
                total = sum(total)
                ) %>% 
            group_by(vacina_dose_agg) %>% 
            mutate(
                doses_label = paste0(format(total, big.mark = ","), " (", round(100 * total / sum(total), 2),"%)")
            ) %>% 
            ungroup(),
        df_vacc_type_month_alldoses %>%
            mutate(
            vacina = "All"
                                                  ) %>%
            group_by(mes_aplicacao, mes_label, vacina, vacina_dose_agg) %>%
            summarise(
                total = sum(total)
            ) %>% 
            ungroup() %>% 
            # group_by(vacina_dose_agg) %>% 
            mutate(
                doses_label = paste0(format(total, big.mark = ","), " (", round(100 * total / sum(total), 2),"%)")
            ) 
        )


writexl::write_xlsx(
    df_vacc_type_month_alldoses_format %>% 
        mutate(
            vacina_dose = paste0(vacina, "_", vacina_dose_agg)
        ) %>% 
        select(mes_aplicacao, mes_label, vacina_dose, total) %>% 
        pivot_wider(names_from = "vacina_dose", values_from = "total")
    , "output/Statistics/vaccine_platform_proportion_month.xlsx"
    )

writexl::write_xlsx(
    df_vacc_type_month_alldoses_format %>% 
        mutate(
            vacina_dose = paste0(vacina, "_", vacina_dose_agg)
        ) %>% 
        select(mes_aplicacao, mes_label, vacina_dose, doses_label) %>% 
        pivot_wider(names_from = "vacina_dose", values_from = "doses_label")
    , "output/Statistics/vaccine_platform_proportion_month_label.xlsx"
    )


# writexl::write_xlsx(
#     df_vacc_type_month %>% 
#         select(mes_label, vacina_label, total_doses) %>% 
#         group_by(mes_label) %>% 
#         mutate(
#             doses_label = paste0(format(total_doses, big.mark = ","), " (", round(100 * total_doses / sum(total_doses), 2),"%)")
#         ) %>% 
#         ungroup() %>% 
#         select(
#             -total_doses
#         ) %>% 
#         pivot_wider(
#             names_from = vacina_label, values_from = doses_label
#         ) %>% 
#         left_join(
#             df_vacc_type_month %>%
#                 group_by(mes_label) %>% 
#                 summarise(
#                     total_doses = sum(total_doses)
#                 ) %>% 
#                 ungroup() %>% 
#                 mutate(
#                     total_doses = paste0(format(total_doses, big.mark = ","), " (", round(100 * total_doses / sum(total_doses), 2),"%)")
#                 )
#         ) %>% 
#         select(mes_label, total_doses, everything())
#     , "output/Statistics/vaccine_platform_proportion_month.xlsx"
# )




month_labels <-
    df_vacc_type_month %>%
    # filter(mes_aplicacao %in% seq(2, max(semana_aplicacao), 6)) %>%
    distinct(
        mes_aplicacao, mes_label
    )


plot_vacc_type_month <-
    df_vacc_type_month %>% 
    ggplot() +
    geom_area(aes(x = mes_aplicacao, y = total_doses / 1e6, fill = vacina_label)) +
    scale_y_continuous(
        labels = scales::comma_format(), n.breaks = 6) +
    scale_x_continuous(
        breaks = month_labels$mes_aplicacao,
        labels = month_labels$mes_label
        ) +
    scale_fill_discrete(name = "") +
    labs(
        x = "Month of vaccine administration",
        y = "Number of first vaccine doses (Millions)"
    ) +
    theme_bw() +
    theme(
        legend.position = "top", 
        legend.key.size =  unit(x = 0.5, units = "cm")
    )




################################################################################
#### Combining plots  ####

library(patchwork)

plot_combined <-
    (
        (plot_df_covid_hosp_adm_vacc_br ) /
            ( plot_vaccine_br_age_sex + plot_vaccine_br_age_sex_uf_map + plot_vacc_type_month)
    ) + 
    # (
    #     (plot_covid_cases_br_events) / 
    #     (plot_df_covid_cases_vacc_br + plot_vaccine_br_age_sex) /
    #     (plot_vaccine_br_age_sex_uf_map + plot_vacc_type_week)
    # ) + 
    plot_annotation(tag_levels = "A") + 
    plot_layout(widths = c(1, 1, 1, 0.5))


ggsave("output/Figure1_PandemicProgression.pdf",
       plot_combined, units = "in", dpi = 800,
       width = 14, height = 10)








# Auxiliary Tables --------------------------------------------------------
# 
# ## Applied vaccine rates per age and sex rates - Brazil and Regions
# df_vaccine_br_age_sex_br_region <- ## Includes single dose with D1
#     data.table::fread("input/df_vaccine_br_city_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
#     as_tibble() %>% 
#     mutate(
#         vacina_dose_agg = vacina_dose1_agg
#     ) %>% 
#     group_by(region, data_aplicacao, sexo, idade_grupo, vacina, vacina_dose_agg) %>% ## Keeps D1 and D2 + single separated
#     summarise(
#         total = sum(total)
#     ) %>% 
#     ungroup() %>% 
#     mutate(
#         total_D1_single = if_else(vacina_dose_agg == "D1_single", total, 0L)
#     ) %>% 
#     group_by(sexo, idade_grupo) %>%
#     summarise(
#         total_doses = sum(total_D1_single)
#     ) %>% 
#     ungroup() %>% 
#     left_join(
#         df_population_city_sex %>% 
#             group_by(sexo, idade_grupo) %>% 
#             summarise(
#                 total_population = sum(population)
#             ) %>% 
#             ungroup()
#         , by = c("sexo" = "sexo",
#                  "idade_grupo" = "idade_grupo")
#     ) %>% 
#     filter(!is.na(sexo)) 
# 
# 
# df_vaccine_br_age_sex_uf <-
#     df_population_city_sex %>% 
#     group_by(uf, sexo, idade_grupo) %>% 
#     summarise(
#         uf_total_population = sum(population)
#     ) %>% 
#     left_join(
#         df_vaccine_br_age_sex %>% 
#             select(sexo, idade_grupo, br_total_population = total_population)
#     ) %>% 
#     left_join(
#         data.table::fread("input/df_vaccine_br_city_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
#             as_tibble() %>% 
#             mutate(
#                 vacina_dose_agg = vacina_dose1_agg
#             ) %>% 
#             group_by(uf, data_aplicacao, sexo, idade_grupo, vacina, vacina_dose_agg) %>% ## Keeps D1 and D2 + single separated
#             summarise(
#                 total = sum(total)
#             ) %>% 
#             mutate(
#                 total_D1_single = if_else(vacina_dose_agg == "D1_single", total, 0L)
#             ) %>% 
#             group_by(uf, sexo, idade_grupo) %>%
#             summarise(
#                 total_doses =  sum(total_D1_single)
#             ) %>% 
#             ungroup()
#         , by = c("uf" = "uf", 
#                  "sexo" = "sexo",
#                  "idade_grupo" = "idade_grupo")
#     ) %>% 
#     filter(uf != "", !is.na(sexo)) %>% 
#     mutate(
#         total_doses = if_else(is.na(total_doses), 0L, total_doses)
#     ) %>% 
#     mutate(
#         doses_pop = total_doses / uf_total_population
#     ) %>% 
#     mutate(
#         doses_pop_exp = doses_pop * br_total_population
#     ) %>% 
#     group_by(uf) %>% 
#     summarise(
#         doses_pop_100_crude = (sum(total_doses) / sum(uf_total_population)) * 100,
#         doses_pop_100_age_sex = (sum(doses_pop_exp) / sum(br_total_population)) * 100
#     ) %>% 
#     ungroup()
# 
# 
# writexl::write_xlsx(df_vaccine_br_age_sex_uf, 
#                     "output/Statistics/vaccine_doses_100_people_state.csv")
# 








# ## end code ##
