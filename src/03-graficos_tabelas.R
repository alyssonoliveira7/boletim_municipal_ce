
# Limpando ambiente --------------------------------------------------

rm(list = ls())
graphics.off()

# Pacotes ------------------------------------------------------------

library(tidyverse)
library(scales)
library(clock)

# Importando - Indicadores e balancentes -----------------------------

indicadores <- read_rds(
  here::here('out/indicadores/indicadores_mun_ce.rds')
)


balancete_uf <- readr::read_rds(
  here::here(
    "out/dados_consolidados/balancetes/quadrimestral/balancete_uf_quadrimestral.rds"
  )
)

balancete_mun <- readr::read_rds(
  here::here(
    "out/dados_consolidados/balancetes/quadrimestral/balancete_mun_quadrimestral.rds"
  )
)

# Arrumadno balancete - UF -------------------------------------------

x <- 1

data_atual <- clock::date_build(year = 2021,month = case_when(
  x == 1 ~ 4,
  x == 2 ~ 8,
  x == 3 ~ 12
  )
  )

data_defasada <- add_months(data_atual, -12)

variacao <- balancete_mun %>% 
  mutate(quadrimestre = as.character(quadrimestre)) %>% 
  filter(
    date == data_atual| date == data_defasada,
    mun == 'Fortaleza'
  ) %>% 
  group_by(conta) %>%
  mutate(
    variacao = (valor/lag(valor)) - 1
  ) %>%
  ungroup() %>% 
  drop_na(variacao) %>% 
  select(conta, variacao)

balancete_mun %>% 
  mutate(quadrimestre = as.character(quadrimestre)) %>% 
  filter(
    date == data_atual| date == data_defasada,
    mun == 'Fortaleza'
  ) %>% 
  mutate(
    date = format(date, 'p%Y%m'),
    quadrimestre = as.character(quadrimestre)
    ) %>%
  select(conta, date, valor) %>%
  pivot_wider(
    names_from = date, 
    values_from = valor,
  ) %>% view

