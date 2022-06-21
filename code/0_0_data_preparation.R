################################################################################
####### Data Preparation - Vaccine Descriptive
################################################################################


# Libraries ---------------------------------------------------------------
library(tidyverse)

date_limit <- "2021-08-31"

# Data Input --------------------------------------------------------------
################################################################################
### Socioeconomic data (population census)
df_socioeco <- 
    read_csv2("data/dados_socioeconomicos_horacio.csv") %>% 
    select(
        cod_ibge = COD7, 
        cod_ibge_6 = COD6,
        rm = RM,
        gini = GINI,
        pib_cap = PIB_P_CAP,
        area_km2 = AREA
        ) %>% 
    mutate(
        cod_ibge = as.character(cod_ibge),
        cod_ibge_6 = as.character(cod_ibge_6),
        gini = as.numeric(stringr::str_replace(gini, ",", "."))
    ) 

df_socioeco_extra <- 
    read_csv("data/vacceq_svi.csv") %>% 
    select(
        codmun, idhm, idhm_educ, idhm_long, idhm_renda, dist_to_capital, 
        perc_votos_bolso, svi_original
    ) %>% 
    mutate(
        codmun = as.character(codmun)
    )

df_ibge_socio <- 
    readxl::read_excel("data/prop_raca_ibge_2010.xlsx") %>% 
    mutate(
        cod_ibge_6 = stringr::str_sub(cod_ibge, 1, 6),
        black_brown = Preta + Parda
    ) %>% 
    select(
        cod_ibge_6, white = Branca, black_brown, asian = Amarela,  indigenous = Indígena
    )


df_dados_rural <- readxl::read_excel("data/Tipologia_municipal_rural_urbano.xlsx", sheet = 2) %>% 
    mutate(
        cod_ibge_6 = stringr::str_sub(CD_GCMUN, 1, 6),
        type_urban = case_when(
            str_detect(TIPO, "Rural") ~ "Rural",
            str_detect(TIPO, "Intermediario") ~ "Semi-Urban",
            TIPO == "Urbano" ~ "Urban"
        )
    )

df_dados_ab_2019 <- 
    readxl::read_xlsx("data/Historico-AB-MUNICIPIOS-2007-202012.xlsx", sheet = "2019") %>% 
    filter(NU_COMPETENCIA == "201912") %>% 
    select(
        cod_ibge = CO_MUNICIPIO_IBGE,
        qt_pricare_fhs_team = QT_EQUIPE_SF_AB, 
        qt_fhs_team         = QT_EQUIPE_SF, 
        qt_pcare_fhs_equi_team = QT_EQUIPE_AB_EQUIVALENTE_CH,
        qt_fhs_cov          = QT_COBERTURA_SF, 
        qt_pricare_cov      = QT_COBERTURA_AB,
        pricare_cov = PC_COBERTURA_AB, 
        fhs_cov = PC_COBERTURA_SF, 
        qt_populacao = QT_POPULACAO
        ) %>%
    # select()
    mutate_at(
        vars(starts_with(c("qt", "pricare", "fhs"))),
        function(x) {
            x <- str_replace_all(x, "\\.", "")
            x <- str_replace_all(x, "\\,", ".")
            x <- as.numeric(x)
            
            return(x)
            }
        ) %>% 
    mutate(
        pricare_cov_hab =  (qt_pricare_fhs_team / qt_populacao) * 3000,
        pricare_cov_calc =  (qt_pricare_fhs_team * 3000) / qt_populacao
    )


df_city_general_info <- 
    read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/municipios.csv") %>% 
    left_join(
        read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/estados.csv") %>% 
            select(codigo_uf, uf, nome_uf = nome),
        by = "codigo_uf"
    ) %>% 
    mutate(
        codigo_ibge_6 = stringr::str_sub(codigo_ibge, 1, 6)
    )


################################################################################
### Population per city - per Sex
df_population_city_sex <- 
    bind_rows(
        data.table::fread("data/population_MS_age_male_2020.csv", na.strings = c("NA", "")) %>% 
            as_tibble() %>% 
            mutate(
                sexo = "male"
            ),
        data.table::fread("data/population_MS_age_female_2020.csv", na.strings = c("NA", "")) %>% 
            as_tibble() %>% 
            mutate(
                sexo = "female"
            )
    ) %>% 
    pivot_longer(-c(cod_ibge, nome, sexo), names_to = "idade_grupo", values_to = "population") %>% 
    mutate(
        idade_grupo = case_when(
            idade_grupo == "0 a 4 anos"   ~ "0-4",
            idade_grupo == "5 a 9 anos"   ~ "5-9",
            idade_grupo == "10 a 14 anos" ~ "10-14",
            idade_grupo == "15 a 19 anos" ~ "15-19",
            idade_grupo == "20 a 29 anos" ~ "20-29",
            idade_grupo == "30 a 39 anos" ~ "30-39",
            idade_grupo == "40 a 49 anos" ~ "40-49",
            idade_grupo == "50 a 59 anos" ~ "50-59",
            idade_grupo == "60 a 69 anos" ~ "60-69",
            idade_grupo == "70 a 79 anos" ~ "70-79",
            TRUE                          ~ "80+"
        )
    ) %>% 
    mutate(
        cod_ibge = as.character(cod_ibge) 
    ) %>% 
    left_join(
        df_city_general_info %>% 
            select(codigo_ibge_6, uf),
        by = c("cod_ibge" = "codigo_ibge_6")
    )

# write_csv(df_pnad_population_city_sex, "input/df_pnad_population_city_sex.csv")
data.table::fwrite(df_population_city_sex, "input/df_population_city_sex.csv")



### Municipality data references
df_br_cities_info <- 
    df_city_general_info %>% 
    left_join(
        df_socioeco
        , by = c("codigo_ibge_6" = "cod_ibge_6")
        ) %>%
    left_join(
        df_ibge_socio
        , by = c("codigo_ibge_6" = "cod_ibge_6")
    ) %>% 
    left_join(
        df_socioeco_extra
        , by = c("codigo_ibge_6" = "codmun")
        ) %>% 
    left_join(
        df_dados_ab_2019
        , by = c("codigo_ibge_6" = "cod_ibge")
        ) %>% 
    left_join(
        df_dados_rural %>% 
            select(cod_ibge_6, type_urban),
        by = c("codigo_ibge_6" = "cod_ibge_6")
    ) %>% 
    left_join(
        df_population_city_sex %>% 
            group_by(cod_ibge) %>% 
            summarise(
                population_2020 = sum(population)
            ),
        by = c("codigo_ibge_6" = "cod_ibge")
        ) %>% 
    mutate(
        pop_area = population_2020 / area_km2
    )


write_csv(df_br_cities_info, "input/df_br_cities_info.csv")



### Population per city (PNAD) - All sexes
# df_pnad_population_city <-
#     df_pnad_population_city_sex %>%
#     group_by(cod_ibge, nome, idade_grupo) %>%
#     summarise(
#         population = sum(population)
#     ) %>%
#     ungroup()
# 
# write_csv(df_pnad_population_city, "input/df_pnad_population_city.csv")
# 



################################################################################
### COVID-19 confirmed cases (Brasil.io)
df_covid_cases_city <-
    data.table::fread("https://data.brasil.io/dataset/covid19/caso_full.csv.gz",
                      na.strings = c("NA", ""), encoding = "UTF-8") %>%
    as_tibble() %>%
    # filter(date >= as.Date("2021-01-01")) %>%
    filter(
        date <= as.Date(date_limit),
        place_type == "city",
        !is.na(city_ibge_code)
        ) %>%
    mutate(
        city_ibge_code = case_when(
            state == "DF" ~ 5300108L,
            TRUE ~ city_ibge_code
        )
    )

data.table::fwrite(df_covid_cases_city,
                   "input/df_covid_cases_city_2021-08-31.csv.gz")



################################################################################
### Vaccine Data
df_vaccine_br_city <- 
    data.table::fread("data/vw_vacc_date_city_age_dose_2022-01-10.csv.gz",
                      na.strings = c("NA", "")) %>%
    as_tibble() %>%    
    mutate(
        uf = if_else(uf %in% c("XX", ""), NA_character_, uf),
        cidade_resid_ibge = case_when(
            uf == "DF"   ~ "530010",
            uf == ""     ~ NA_character_,
            uf == "None" ~ NA_character_,
            TRUE ~ cidade_resid_ibge
        )
    ) %>% 
    filter(
        data_aplicacao >= as.Date("2021-01-17")
    ) %>% 
    # filter(data_aplicacao <= as.Date(date_limit)) %>%
    filter(data_aplicacao <= as.Date(date_limit)) %>%
    filter(!is.na(uf)) %>% 
    filter(!is.na(cidade_resid_ibge)) %>% 
    filter(vacina_dose %in% c("D1", "D2", "D_unica")) %>%
    filter(vacina != "sem_identificacao") %>% 
    mutate(
        vacina_dose_agg =
            case_when(
                vacina_dose == "D1" ~ "D1",
                TRUE ~ "D2_single"
            ),
        vacina_dose1_agg =
            case_when(
                vacina_dose == "D2" ~ "D2",
                TRUE ~ "D1_single"
            ),
        sexo = case_when(
            sexo == "M" ~ "male",
            sexo == "F" ~ "female",
            TRUE ~ NA_character_,
            )
        )

data.table::fwrite(df_vaccine_br_city, 
                   "input/df_vaccine_br_city_2021_08_31.csv.gz")








# SIVEP - COVID-19 hospitalizations and in-hospital deaths ----------------

df_sivep_covid <- 
    data.table::fread("data/srag_adults_covid_hosp_2022-02-07.csv.gz",
                      na.strings = c("NA", "")) %>%
    as_tibble() %>% 
    filter(date_sint <= as.Date(date_limit)) %>%
    # filter(between(date_sint, as.Date("2021-01-01"), as.Date(date_limit))) %>% 
    mutate(
        idade_grupo = case_when(
            NU_IDADE_N <= 29 ~ "20-29",
            NU_IDADE_N <= 39 ~ "30-39",
            NU_IDADE_N <= 49 ~ "40-49",
            NU_IDADE_N <= 59 ~ "50-59",
            NU_IDADE_N <= 69 ~ "60-69",
            NU_IDADE_N <= 79 ~ "70-79",
            TRUE ~ "80+"
            ),
        CO_MUN_RES = case_when(
            SG_UF == "DF" ~ 530010L,
            TRUE ~ CO_MUN_RES
            )
        ) %>% 
    select(REGIAO, SG_UF_INTE, CO_MUN_RES, date_sint, date_desf, 
           idade_grupo, CS_SEXO, EVOLUCAO)

data.table::fwrite(df_sivep_covid, "input/df_sivep_covid_2021_08_31.csv.gz")

