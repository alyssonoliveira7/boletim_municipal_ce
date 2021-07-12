# Removendo Objetos --------------------------------------------------

rm(list = ls())
graphics.off()
gc()

# Pacotes ------------------------------------------------------------

library(tidyverse)

# Parâmetros ---------------------------------------------------------

date_filter <- '2018-01-01'

# Importando - Balancetes --------------------------------------------

balancete_uf <- readr::read_csv2(
  here::here('out/dados_consolidados/balancetes/balancete_uf.csv')
)

balancete_mun <- readr::read_csv2(
  here::here('out/dados_consolidados/balancetes/balancete_mun.csv')
)

investimento.df <- readr::read_csv2(
  here::here('out/dados_consolidados/investimento/investimento.df.csv'),
  col_types = cols(
    date = col_date(format = ""),
    cod_ibge = col_character(),
    transferencias_de_capital = col_double(),
    operacoes_de_credito = col_double(),
    investimentos = col_double()
  )
)

rc_liquida.df <- readr::read_csv2(
  here::here('out/dados_consolidados/rcl/rc_liquida.df.csv'),
  col_types = cols(
    date = col_date(format = ""),
    cod_ibge = col_character(),
    rcl_bimestral = col_double()
  )
)

servico_divida <- readr::read_csv2(
  here::here('out/dados_consolidados/servico_divida/servico_divida.csv'),
  col_types = cols(
    exercicio = col_character(),
    periodo = col_factor(),
    date = col_date(format = ""),
    cod_ibge = col_character(),
    servico_divida = col_double()
  )
)

d_pessoal <- readr::read_csv2(
  here::here('out/dados_consolidados/despesa_pessoal/d_pessoal.csv'),
  col_types = cols(
    date = col_date(format = ""),
    cod_ibge = col_double(),
    Executivo = col_double(),
    Legislativo = col_double(),
    total = col_double()
  )
)

d_pessoal_uf <- readr::read_csv2(
  here::here('out/dados_consolidados/despesa_pessoal/d_pessoal_uf.csv'),
  col_types = cols(
    date = col_date(format = ""),
    cod_ibge = col_double(),
    uf = col_character(),
    Executivo = col_double(),
    Legislativo = col_double(),
    total = col_double()
  )
)

dcl <- readr::read_csv2(
  here::here('out/dados_consolidados/dcl/dcl.csv'),
  col_types = cols(
    date = col_date(format = ""),
    instituicao = col_character(),
    cod_ibge = col_double(),
    uf = col_character(),
    poder = col_character(),
    populacao = col_double(),
    coluna = col_character(),
    conta = col_character(),
    valor = col_double()
  )
)


# Transformando dados bimestrais em quadrimestrais -------------------

balancete_uf_quadrimestral <- balancete_uf %>% 
  mutate(
    ano = clock::get_year(date),
    mes = clock::get_month(date),
    quadrimestre = case_when(
      mes == 2 ~ 1,
      mes == 4 ~ 1,
      mes == 6 ~ 2,
      mes == 8 ~ 2,
      mes == 10 ~ 3,
      mes == 12 ~ 3
    )
  ) %>% 
  group_by(ano, quadrimestre, tp_conta, conta) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  mutate(
    mes = case_when(
      quadrimestre == 1 ~ 4,
      quadrimestre == 2 ~ 8,
      quadrimestre == 3 ~ 12
    ),
    date = clock::date_build(ano, mes, 1)
  ) %>% 
  relocate(date) %>% relocate(mes, .after = quadrimestre) %>% 
  select(-ano, -quadrimestre, -mes)


balancete_mun_quadrimestral <- balancete_mun %>% 
  mutate(
    ano = clock::get_year(date),
    mes = clock::get_month(date),
    quadrimestre = case_when(
      mes == 2 ~ 1,
      mes == 4 ~ 1,
      mes == 6 ~ 2,
      mes == 8 ~ 2,
      mes == 10 ~ 3,
      mes == 12 ~ 3
    )
  ) %>% 
  group_by(ano, quadrimestre, cod_ibge, tp_conta, conta) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  mutate(
    mes = case_when(
      quadrimestre == 1 ~ 4,
      quadrimestre == 2 ~ 8,
      quadrimestre == 3 ~ 12
    ),
    date = clock::date_build(ano, mes, 1)
  ) %>% 
  relocate(date) %>% relocate(mes, .after = quadrimestre) %>% 
  select(-ano, -quadrimestre, -mes)

investimento_quadrimestral <- investimento.df %>% 
  mutate(
    ano = clock::get_year(date),
    mes = clock::get_month(date),
    quadrimestre = case_when(
      mes == 2 ~ 1,
      mes == 4 ~ 1,
      mes == 6 ~ 2,
      mes == 8 ~ 2,
      mes == 10 ~ 3,
      mes == 12 ~ 3
    )
  ) %>% 
  pivot_longer(
    transferencias_de_capital:investimentos,
    names_to = 'conta', 
    values_to = 'valor'
    ) %>% 
  group_by(ano, quadrimestre, cod_ibge, conta) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  mutate(
    mes = case_when(
      quadrimestre == 1 ~ 4,
      quadrimestre == 2 ~ 8,
      quadrimestre == 3 ~ 12
    ),
    date = clock::date_build(ano, mes, 1)
  ) %>% 
  relocate(date) %>% relocate(mes, .after = quadrimestre) %>% 
  select(-ano, -quadrimestre, -mes) %>% 
  pivot_wider(
    names_from = conta,
    values_from = valor
  )

servico_divida_quadrimestre <- servico_divida %>% 
  mutate(
    ano = clock::get_year(date),
    mes = clock::get_month(date),
    quadrimestre = case_when(
      mes == 2 ~ 1,
      mes == 4 ~ 1,
      mes == 6 ~ 2,
      mes == 8 ~ 2,
      mes == 10 ~ 3,
      mes == 12 ~ 3
    )
  ) %>% 
  group_by(ano, quadrimestre, cod_ibge) %>% 
  summarise(servico_divida = sum(servico_divida)) %>% 
  ungroup() %>% 
  mutate(
    mes = case_when(
      quadrimestre == 1 ~ 4,
      quadrimestre == 2 ~ 8,
      quadrimestre == 3 ~ 12
    ),
    date = clock::date_build(ano, mes, 1)
  ) %>% 
  relocate(date) %>% relocate(mes, .after = quadrimestre) %>% 
  select(-ano, -quadrimestre, -mes)
  


rc_liquida_qudrimestral <- rc_liquida.df %>% 
  mutate(
    ano = clock::get_year(date),
    mes = clock::get_month(date),
    quadrimestre = case_when(
      mes == 2 ~ 1,
      mes == 4 ~ 1,
      mes == 6 ~ 2,
      mes == 8 ~ 2,
      mes == 10 ~ 3,
      mes == 12 ~ 3
    )
  ) %>% 
  group_by(ano, quadrimestre, cod_ibge) %>% 
  summarise(rcl_quadrimestral = sum(rcl_bimestral)) %>% 
  ungroup() %>% 
  mutate(
    mes = case_when(
      quadrimestre == 1 ~ 4,
      quadrimestre == 2 ~ 8,
      quadrimestre == 3 ~ 12
    ),
    date = clock::date_build(ano, mes, 1)
  ) %>% 
  relocate(date) %>% relocate(mes, .after = quadrimestre) %>% 
  select(-ano, -quadrimestre, -mes)

# Indicador 1 - Independência Fiscal ---------------------------------

indicador_1_ce <- balancete_uf_quadrimestral %>% 
  filter(
    date >= date_filter,
    conta %in% c(
      'receitas_tributarias', 'receitas_correntes', 'receitas_de_capital')
    ) %>% 
  arrange(date) %>% 
  pivot_wider(names_from = conta, values_from = valor) %>% 
  mutate(
    independecia_fiscal = receitas_tributarias/(receitas_correntes + receitas_de_capital)
  ) %>% 
  select(date, independecia_fiscal)

indicador_1_mun <- balancete_mun_quadrimestral %>% 
  filter(
    date >= date_filter,
    conta %in% c(
      'receitas_tributarias', 'receitas_correntes', 'receitas_de_capital')
  ) %>% 
  arrange(date) %>% 
  pivot_wider(names_from = conta, values_from = valor) %>% 
  mutate(
    independecia_fiscal = receitas_tributarias/(receitas_correntes + receitas_de_capital),
    cod_ibge = as.character(cod_ibge)
  ) %>% 
  select(date, cod_ibge, independecia_fiscal)

# Indicador 2 - Rigidez Despesa --------------------------------------

indicador_2_mun <- balancete_mun_quadrimestral %>%
  filter(
    date >= date_filter,
    conta %in% c(
      "despesas_correntes",
      'desp_total'
    )
  ) %>% 
  pivot_wider(
    names_from = conta,
    values_from = valor
  ) %>% 
  mutate(
    custeio_total = despesas_correntes/desp_total,
    cod_ibge = as.character(cod_ibge)
  ) %>% 
  select(date, cod_ibge, custeio_total)

# Indicador 3 - Investimento com Recursos Próprios -------------------

indicador_3_mun <- investimento_quadrimestral %>% 
  filter(date >= date_filter) %>% 
  mutate(
    investimentos_rec_prop = (
      investimentos - operacoes_de_credito - transferencias_de_capital
      )/investimentos,
    cod_ibge = as.character(cod_ibge)
  ) %>%
  select(date, cod_ibge, investimentos_rec_prop)

# Indicador 4 - Serviço da Dívida / Receita Corrente Líquida ---------

indicador_4_mun <- rc_liquida_qudrimestral %>% 
  left_join(
    servico_divida_quadrimestre,
    by = c("date", "cod_ibge")
  ) %>% 
  mutate(
    servico_divida = if_else(is.na(servico_divida), 0, servico_divida),
    servico_divida_rcl = servico_divida/rcl_quadrimestral,
    cod_ibge = as.character(cod_ibge)
  ) %>% 
  select(-servico_divida, -rcl_quadrimestral)

# Indicador 5 - Despesa com pessoal ----------------------------------

indicador_5_mun <- d_pessoal %>% 
  rename(desp_pessoal = total) %>% 
  select(-Executivo, -Legislativo) %>% 
  mutate(
    cod_ibge = as.character(cod_ibge),
    desp_pessoal = desp_pessoal/100
  )

indicador_5_uf <- d_pessoal_uf %>% 
  rename(indicador_5 = total) %>% 
  select(-Executivo, -Legislativo)

# Indicador 6 - Dívida Corrente Líquida ------------------------------

indicador_6_mun <- dcl %>% 
  select(date, cod_ibge, valor) %>% 
  mutate(valor = valor/100, cod_ibge = as.character(cod_ibge)) %>% 
  rename(dcl_rcl = valor)


# Consolidando Base de Dados -----------------------------------------

municipios <- indicador_1_mun %>% 
  select(date, cod_ibge) %>% 
  bind_rows(
    indicador_2_mun %>%
      select(date, cod_ibge)
  ) %>% 
  bind_rows(
    indicador_3_mun %>% 
      select(date, cod_ibge)
  ) %>% 
  bind_rows(
    indicador_4_mun %>% 
      select(date, cod_ibge)
  ) %>% 
  bind_rows(
    indicador_5_mun %>% 
      select(date, cod_ibge)
  ) %>% 
  bind_rows(
    indicador_6_mun %>% 
      select(date, cod_ibge)
  ) %>% 
  unique.data.frame()


indicadores <- municipios %>% 
  left_join(indicador_1_mun, by = c("date", "cod_ibge")) %>% 
  left_join(indicador_2_mun, by = c("date", "cod_ibge")) %>% 
  left_join(indicador_3_mun, by = c("date", "cod_ibge")) %>% 
  left_join(indicador_4_mun, by = c("date", "cod_ibge")) %>% 
  left_join(indicador_5_mun, by = c("date", "cod_ibge")) %>% 
  left_join(indicador_6_mun, by = c("date", "cod_ibge")) %>% 
  arrange(cod_ibge)

# Save data ----------------------------------------------------------

writexl::write_xlsx(indicadores, 'out/indicadores/indicadores_mun_ce.xlsx')

write_rds(
  indicadores, 
  here::here('out/indicadores/indicadores_mun_ce.xlsx')
)



