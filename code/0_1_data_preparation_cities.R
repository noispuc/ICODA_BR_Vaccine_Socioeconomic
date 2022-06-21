################################################################################
####### Data Preparation - Variables per Cities
################################################################################

# Libraries ---------------------------------------------------------------
library(tidyverse)

# Data Input --------------------------------------------------------------
### BR cities - socioeconmic info
# df_br_cities_info_socio <- 
#     read_csv("data/vacceq_svi.csv") 


### BR cities - general info + socioeconomic info
df_br_cities_info <- 
    read_csv("input/df_br_cities_info.csv")
    # %>% 
    # select(codigo_ibge, codigo_ibge_6, codigo_uf, uf, nome, 
    #        capital, gini, rm, pib_cap) %>% 
    # left_join(
    #     df_br_cities_info_socio
        # , by = c("codigo_ibge_6" = "codmun")
    # )


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
df_covid_cases_city <-
    data.table::fread("input/df_covid_cases_city_2021-08-31.csv.gz",
                      na.strings = c("NA", ""), encoding = "UTF-8") %>%
    as_tibble()

    
### Vaccine Data
df_vaccine_br_city <- 
    data.table::fread("input/df_vaccine_br_city_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
    as_tibble() %>% 
    mutate(vacina_dose_agg = vacina_dose1_agg) %>% ## Adding D1 + Single
    group_by(uf, cidade_resid_ibge, sexo, idade_grupo, vacina, vacina_dose_agg) %>% 
    summarise(
        total = sum(total)
    ) %>% 
    ungroup() %>% 
    filter(!is.na(cidade_resid_ibge))


## SIVEP Data
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


## SIVEP Data
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
df_br_cities_pop_adults <-
    df_population_city_sex %>% 
    filter(!(idade_grupo %in% c("0-4", "5-9", "10-14", "15-19"))) %>% 
    group_by(cod_ibge) %>% 
    summarise(
        total_pop_adult = sum(population)
    )


################################################################################
#### Total doses per city
df_dose_city <- 
    df_vaccine_br_city %>% 
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
#### Total doses per city - age and sex adjusted

## Total doses per city age and sex
df_dose_city_age_sex <- 
    df_vaccine_br_city %>% 
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
df_dose_city_age_sex_adj <- 
    df_population_city_sex %>%
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
        df_dose_city_age_sex
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
df_covid_notif_br <-
    df_covid_cases_city %>%
    group_by(city_ibge_code, city) %>%
    summarise(
        confirmed_cases  = sum(new_confirmed),
        confirmed_deaths = sum(new_deaths)
    ) %>%
    mutate(
        city_ibge_code_6 = as.numeric(stringr::str_sub(city_ibge_code, 1, 6))
    ) %>%
    ungroup() %>%
    left_join(
        df_population_city %>%
            group_by(city = cod_ibge) %>%
            summarise(
                total_pop = sum(population)
            )
        , by = c("city_ibge_code_6" = "city")
    ) %>%
    mutate(
        confirmed_cases_100k  = (confirmed_cases  / total_pop) * 100000,
        confirmed_deaths_100k = (confirmed_deaths / total_pop) * 100000
    ) %>%
    select(city_ibge_code_6, confirmed_cases, confirmed_deaths, 
           confirmed_cases_100k, confirmed_deaths_100k)


df_covid_notif_br_pre_vacc <-
    df_covid_cases_city %>%
    filter(date < as.Date("2021-01-01")) %>% 
    group_by(city_ibge_code, city) %>%
    summarise(
        confirmed_cases_2020  = sum(new_confirmed),
        confirmed_deaths_2020 = sum(new_deaths)
    ) %>%
    mutate(
        city_ibge_code_6 = as.numeric(stringr::str_sub(city_ibge_code, 1, 6))
    ) %>%
    ungroup() %>%
    left_join(
        df_population_city %>%
            group_by(city = cod_ibge) %>%
            summarise(
                total_pop = sum(population)
            )
        , by = c("city_ibge_code_6" = "city")
    ) %>%
    mutate(
        confirmed_cases_100k_2020  = (confirmed_cases_2020  / total_pop) * 100000,
        confirmed_deaths_100k_2020 = (confirmed_deaths_2020 / total_pop) * 100000
    ) %>%
    select(city_ibge_code_6, confirmed_cases_2020, confirmed_deaths_2020, 
           confirmed_cases_100k_2020, confirmed_deaths_100k_2020)





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




################################################################################
#### General dataset per city

## Cleaning up memory
rm(df_vaccine_br_city)

## Merging tables
df_city_vacc_sivep_info <- 
    df_br_cities_info %>% 
    left_join(
        df_br_cities_pop
        , by = c("codigo_ibge_6" = "cod_ibge")
    ) %>% 
    left_join(
        df_br_cities_pop_adults
        , by = c("codigo_ibge_6" = "cod_ibge")
    ) %>% 
    left_join(
        df_dose_city %>% 
            select(-uf)
        , by = c("codigo_ibge_6" = "cidade_resid_ibge")
    ) %>% 
    left_join(
        df_dose_city_age_sex_adj
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
    left_join(
        df_covid_notif_br
        , by = c("codigo_ibge_6" = "city_ibge_code_6")
    ) %>%
    left_join(
        df_covid_notif_br_pre_vacc
        , by = c("codigo_ibge_6" = "city_ibge_code_6")
    ) %>%
    mutate(
        region = case_when(
            uf %in% c("SP", "RJ", "ES", "MG") ~ "Southeast",
            uf %in% c("SC", "RS", "PR") ~ "South",
            uf %in% c("MT", "MS", "GO", "DF") ~ "Central-West",
            uf %in% c("AM", "AP", "TO", "PA", "RO", "RR", "AC") ~ "North",
            uf %in% c("BA", "AL", "SE", "PE", "MA", "RN", "PB", "CE", "PI") ~ "Northeast"
            )
        )




write_csv(df_city_vacc_sivep_info, "input/df_city_vacc_adm_sivep_D1_single.csv")
