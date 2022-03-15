################################################################################
####### Data Preparation - Variables per Cities
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
df_covid_cases_city <-
    data.table::fread("input/df_covid_cases_city_2021-08-31.csv.gz",
                      na.strings = c("NA", ""), encoding = "UTF-8") %>%
    as_tibble() %>% 
    mutate(
        period = format(as.Date(date), "%Y-%m")
    ) %>% 
    as_tibble() %>% 
    group_by(city_ibge_code, city, period) %>% 
    summarise(
        confirmed_cases = sum(new_confirmed),
        confirmed_deaths = sum(new_deaths)
    ) %>% 
    ungroup() %>% 
    mutate(
        confirmed_cases = if_else(confirmed_cases < 0, 0L, confirmed_cases),
        confirmed_deaths = if_else(confirmed_deaths < 0, 0L, confirmed_deaths)    
    )

    
### Vaccine Data
df_vaccine_br_city <- 
    data.table::fread("input/df_vaccine_br_city_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
    as_tibble() %>% 
    mutate(
        period = format(as.Date(data_aplicacao), "%Y-%m"),
        vacina_dose_agg = vacina_dose1_agg  ## Adding D1 + Single
    ) %>% 
    group_by(uf, cidade_resid_ibge, period, sexo, idade_grupo, vacina, vacina_dose_agg) %>% 
    summarise(
        total = sum(total)
    ) %>% 
    ungroup() %>% 
    filter(!is.na(cidade_resid_ibge))





## SIVEP Data
df_sivep_covid_raw <- data.table::fread("input/df_sivep_covid_2021_08_31.csv.gz", na.strings = c("NA", "")) %>% 
    as_tibble()

df_covid_sivep <- 
    left_join(
        df_sivep_covid_raw %>% 
            mutate(period = format(as.Date(date_sint), "%Y-%m")) %>% 
            group_by(CO_MUN_RES, period, idade_grupo, CS_SEXO) %>% 
            summarise(
                hosp_adm = n()
            ),
        df_sivep_covid_raw %>% 
            filter(EVOLUCAO == "Death") %>% 
            mutate(period = format(as.Date(date_desf), "%Y-%m")) %>% 
            group_by(CO_MUN_RES, period, idade_grupo, CS_SEXO) %>% 
            summarise(
                hosp_death = n()
            )
        ) %>% 
    ungroup() %>% 
    mutate(
        hosp_adm = if_else(is.na(hosp_adm), 0L, hosp_adm),
        hosp_death = if_else(is.na(hosp_death), 0L, hosp_death),
        CS_SEXO = stringr::str_to_lower(CS_SEXO)
    )



rm(df_sivep_covid_raw)




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
    group_by(uf, period, cidade_resid_ibge, vacina_dose_agg) %>% 
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
    group_by(cidade_resid_ibge, period, sexo, idade_grupo) %>%
    summarise(
        total_doses_D1    = sum(total_D1),
        total_doses_D2    = sum(total_D2), # D2 + Single
        total_doses_D1_D2 = sum(total_D1_D2)
    ) %>%
    ungroup() 

### Adjusting by age and sex from the BR population distribution
df_dose_city_age_sex_adj <- 
    df_dose_city_age_sex %>% 
    inner_join(
        df_population_city_sex %>%
            group_by(cod_ibge, sexo, idade_grupo) %>%
            summarise(
                city_total_population = sum(population)
            ) %>% 
            ungroup()
        , by = c("cidade_resid_ibge" = "cod_ibge",
                 # "period" = "period",
                 "sexo" = "sexo",
                 "idade_grupo" = "idade_grupo")
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
    group_by(cidade_resid_ibge, period) %>%
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
    group_by(cidade_resid_ibge) %>%
    arrange(cidade_resid_ibge, period) %>% 
    mutate(
        doses_pop_100_D1_crude_agg   = (cumsum(doses_D1_total)     / city_total_pop) * 100,
        doses_pop_100_D1_age_sex_agg = (cumsum(doses_exp_D1_total) / br_total_pop) * 100,
        
        doses_pop_100_D2_crude_agg   = (cumsum(doses_D2_total)     / city_total_pop) * 100,
        doses_pop_100_D2_age_sex_agg = (cumsum(doses_exp_D2_total) / br_total_pop) * 100,
        
        doses_pop_100_D1_D2_crude_agg   = (cumsum(doses_D1_D2_total)     / city_total_pop) * 100,
        doses_pop_100_D1_D2_age_sex_agg = (cumsum(doses_exp_D1_d2_total) / br_total_pop) * 100
    ) %>% 
    ungroup() %>% 
    select(-c(city_total_pop, br_total_pop)) 







################################################################################
#### Total number of COVID_19 confirmed cases and deaths per population for each city
df_covid_notif_br <-
    df_covid_cases_city %>%
    group_by(city_ibge_code, city, period) %>%
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
    select(city_ibge_code_6, period, confirmed_cases, confirmed_deaths, 
           confirmed_cases_100k, confirmed_deaths_100k)





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
            group_by(sexo, idade_grupo) %>% 
            summarise(
                br_total_population = sum(population)
            )
        , by = c("sexo" = "sexo",
                 "idade_grupo" = "idade_grupo")
        ) %>%
    left_join(
        df_dose_city_age_sex %>% 
            distinct(period),
        by = character()
        ) %>% 
    left_join(
        df_covid_sivep
        , by = c("cod_ibge" = "CO_MUN_RES", 
                 "period" = "period",
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
    group_by(cod_ibge, period) %>%
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
    select(-c(city_total_pop))





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
        df_dose_city_age_sex_adj
        , by = c("codigo_ibge_6" = "cidade_resid_ibge")
    ) %>% 
    left_join(
        df_covid_adm_br 
        , by = c("codigo_ibge_6" = "cod_ibge",
                 "period" = "period")
    ) %>% 
    left_join(
        df_covid_notif_br
        , by = c("codigo_ibge_6" = "city_ibge_code_6",
                 "period" = "period")
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




write_csv(df_city_vacc_sivep_info, "input/df_city_vacc_adm_sivep_month_D1_single.csv")
