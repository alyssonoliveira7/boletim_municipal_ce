# Removendo Objetos --------------------------------------------------

rm(list = ls())
graphics.off()

# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(clock)

# Parâmetros ---------------------------------------------------------

#' *Objeto com a data da atualização monetária feita pelo:* _IPCA_

data_atualizacao_mone <- '2021-04-01'

#' Códigos utilizados para filtrar as contas das *Receitas Primárias*

cod_conta_filtro_rp <- c(
  "CotaParteDoITR",
  "OutrasAlienacoesDeBens",
  "OutrasReceitasDeCapitalNaoPrimarias",
  "OutrasReceitasDeCapitalPrimarias",
  "OutrasReceitasFinanceiras",
  "OutrasReceitasPatrimoniais",
  "ReceitasCorrentes",
  "ReceitasDeAlienacaoDeInvestimentosPermanentes",
  "ReceitasDeAlienacaoDeInvestimentosTemporarios",
  "ReceitasPrimariasCorrentes",
  "RREO6AlienacaoDeBens",
  "RREO6AmortizacaoDeEmprestimos",
  "RREO6AplicacoesFinanceiras",
  "RREO6ConveniosCapital",
  "RREO6CotaParteDoFPM",
  "RREO6CotaParteDoICMS",
  "RREO6CotaParteDoIPVA",
  "RREO6DemaisReceitasCorrentes",
  "RREO6DiversasReceitasCorrentes",
  "RREO6IPTU",
  "RREO6IRRF",
  "RREO6ISS",
  "RREO6ITBI",
  "RREO6OperacoesDeCredito",
  "RREO6OutrasReceitasDeCapital",
  "RREO6OutrasReceitasTributarias",
  "RREO6OutrasTransferenciasCorrentes",
  "RREO6OutrasTransferenciasDeCapital",
  "RREO6ReceitaPatrimonial",
  "RREO6ReceitasDeCapital",
  "RREO6ReceitasDeContribuicoes",
  "RREO6ReceitasPrimariasDeCapital",
  "RREO6ReceitasTributarias",
  "RREO6TotalReceitaPrimaria",
  "RREO6TransferenciasCorrentes",
  "RREO6TransferenciasDeCapital",
  "TransferenciasDaLC871996",
  "TransferenciasDaLCn611989",
  "TransferenciasDoFUNDEB"
)

#' Códigos utilizados para filtrar as contas das *Despesas Primárias*

cod_conta_filtro_dp <- c(
  'RREO6DespesasCorrentes',
  'RREO6PessoalEEncargosSociais',
  'RREO6JurosEEncargosDaDivida',
  'RREO6OutrasDespesasCorrentes',
  'RREO6DespesasDeCapital',
  'RREO6Investimentos',
  'RREO6DemaisInversoesFinanceiras',
  'RREO6AmortizacaoDaDivida'
)

despesas_emprenhadas <- c(
  'DESPESAS EMPENHADAS',
  'DESPESAS EMPENHADAS ATÉ O BIMESTRE / 2015',
  'Despesas Empenhadas Até o Bimestre / 2016',
  'DESPESAS EMPENHADAS ATÉ O BIMESTRE / 2017'
)

#' Códigos utilizados para filtrar as contas de *Investimento*

cod_conta_filtro_investimento <- c(
  'RREO6Investimentos',
  'RREO6OperacoesDeCredito',
  'RREO6TransferenciasDeCapital'
)

#' Códigos utilizados para filtrar as contas de *Serviços da dívida*

cod_conta_filtro_sd <- c(
  'RREO6JurosEEncargosDaDivida',
  'RREO6AmortizacaoDaDivida'
)

# Importando Dados ---------------------------------------------------

#' _Receita Primária_

rp <- fs::dir_ls(
  here::here("data/siconfi/rp"),
  glob = "*.csv"
) %>%
  map_df(
    .f = ~ readr::read_csv2(
      .,
      col_types = cols(
        exercicio = col_character(),
        demonstrativo = col_character(),
        periodo = col_factor(),
        periodicidade = col_factor(),
        instituicao = col_character(),
        cod_ibge = col_character(),
        uf = col_character(),
        populacao = col_double(),
        anexo = col_character(),
        rotulo = col_character(),
        coluna = col_character(),
        cod_conta = col_character(),
        conta = col_character(),
        valor = col_double()
      )
    )
  )

#' _Receita Corrente Líquida_

rc_liquida <- fs::dir_ls(
  here::here("data/siconfi/rcl"),
  glob = "*.csv"
) %>%
  map_df(
    .f = ~ readr::read_csv2(
      .,
      col_types = cols(
        exercicio = col_character(),
        demonstrativo = col_character(),
        periodo = col_factor(),
        periodicidade = col_factor(),
        instituicao = col_character(),
        cod_ibge = col_character(),
        uf = col_character(),
        populacao = col_double(),
        anexo = col_character(),
        rotulo = col_character(),
        coluna = col_character(),
        cod_conta = col_character(),
        conta = col_character(),
        valor = col_double()
      )
    )
  )

#' _Despesa Primária_

despesas <- readr::read_csv2(
  here::here('data/siconfi/despesas/despesas_2015_2020.csv'),
  col_types = cols(
    exercicio = col_character(),
    demonstrativo = col_character(),
    periodo = col_factor(),
    periodicidade = col_factor(),
    instituicao = col_character(),
    cod_ibge = col_character(),
    uf = col_character(),
    populacao = col_double(),
    anexo = col_character(),
    rotulo = col_character(),
    coluna = col_character(),
    cod_conta = col_character(),
    conta = col_character(),
    valor = col_double()
  )
)

# Importando - IPCA -------------------------------------------------------

ipca <- sidrar::get_sidra(
  x = 1737,
  variable = 2266,
  period = 'all'
)

ipca <- ipca %>% 
  janitor::clean_names() %>% 
  select(
    mes_codigo,
    valor
  ) %>% 
  rename(
    date = mes_codigo,
    ipca = valor
  ) %>% 
  mutate(
    date = parse_date(date, '%Y%m')
  ) %>% as_tibble()

#' _Deflator_

deflator <- ipca %>% 
  mutate(
    deflator = nth(ipca, which(date == data_atualizacao_mone))/ipca
  )

# Selecionando código de contas --------------------------------------

#' *Códigos das contas para as Receitas Primárias*

codigos_conta_rp <- rp %>% 
  mutate(exercicio = as.numeric(exercicio)) %>% 
  filter(exercicio >= 2018) %>% 
  count(conta, cod_conta) %>% 
  select(-n)

#' *Códigos das contas para as Despesas Primárias*

codigos_conta_dp <- despesas %>% 
  mutate(exercicio = as.numeric(exercicio)) %>% 
  filter(exercicio >= 2018) %>% 
  count(conta, cod_conta) %>% 
  select(-n)


#' *Códigos das contas para as Receitas Correntes Líquidas*

codigos_conta_rcl <- rc_liquida %>% 
  mutate(exercicio = as.numeric(exercicio)) %>% 
  filter(exercicio >= 2018) %>% 
  count(conta, cod_conta) %>% 
  select(-n)

# Importando - Receitas Primárias Bimestrais -------------------------

rp_concatenar <- fs::dir_ls(
  here::here('data/siconfi/rp/dados_bimetrais')
) %>% 
  map_df(
    .f = ~readr::read_csv2(
      .,
      skip = 6,
      locale = locale(encoding = 'latin1'),
      col_types = cols(
        Instituição = col_character(),
        Cod.IBGE = col_character(),
        UF = col_character(),
        População = col_double(),
        Coluna = col_character(),
        Conta = col_character(),
        Valor = col_double(),
        date = col_date(format = '%Y%m%d')
      )
    ) %>% 
      janitor::clean_names() %>% 
      relocate(date)
  )

rp_concatenar <- rp_concatenar %>% 
  left_join(codigos_conta_rp, by = 'conta') %>% 
  mutate(
    cod_conta = ifelse(
      is.na(cod_conta), 'RREO6TotalReceitaPrimaria', cod_conta)
  ) %>% 
  relocate(cod_conta, .before = conta)

#' Atualizando valores 

rp_concatenar <- rp_concatenar %>% 
  left_join(deflator %>% select(-ipca), by = 'date') %>% 
  mutate(
    valor_constante = valor * deflator
  ) %>% 
  select(-deflator)


rp_concatenar <- rp_concatenar %>% 
  filter(cod_conta %in% cod_conta_filtro_rp & coluna != 'PREVISÃO ATUALIZADA')


# Importando - Despesas Primárias Bimestrais -------------------------

dp_concatenar <- fs::dir_ls(
  here::here('data/siconfi/despesas/despesa_primaria')
) %>% 
  map_df(
    .f = ~readr::read_csv2(
      .,
      skip = 6,
      locale = locale(encoding = 'latin1'),
      col_types = cols(
        Instituição = col_character(),
        Cod.IBGE = col_character(),
        UF = col_character(),
        População = col_double(),
        Coluna = col_character(),
        Conta = col_character(),
        Valor = col_double(),
        date = col_date(format = '%Y%m%d')
      )
    ) %>% 
      janitor::clean_names() %>% 
      relocate(date)
  )

dp_concatenar <- dp_concatenar %>% 
  left_join(codigos_conta_dp, by = 'conta') %>% 
  filter(
    cod_conta %in% cod_conta_filtro_dp,
    coluna == 'DESPESAS EMPENHADAS'
    )

dp_concatenar <- dp_concatenar %>% 
  left_join(deflator %>% select(-ipca), by = 'date') %>% 
  mutate(
    valor_constante = valor * deflator
  ) %>% 
  select(-deflator)

# Importando - Receitas Correntes Líquidas Bimestrais ----------------

rcl_concatenar <- fs::dir_ls(
  here::here('data/siconfi/rcl/dados_bimestrais')
) %>% 
  map_df(
    .f = ~readr::read_csv2(
      .,
      skip = 6,
      locale = locale(encoding = 'latin1'),
      col_types = cols(
        Instituição = col_character(),
        Cod.IBGE = col_character(),
        UF = col_character(),
        População = col_double(),
        Coluna = col_character(),
        Conta = col_character(),
        Valor = col_double(),
        date = col_date(format = '%Y%m%d')
      )
    ) %>% 
      janitor::clean_names() %>% 
      relocate(date)
  )


rcl_concatenar <- rcl_concatenar %>% 
  left_join(codigos_conta_rcl, by = 'conta') %>% 
  mutate(
    filtro_conta = str_detect(coluna, 'PREVISÃO ATUALIZADA')
  ) %>% 
  filter(
    cod_conta %in% c('RREO3ReceitaCorrenteLiquida', 'ReceitaCorrenteLiquida'),
    !(coluna %in% c('TOTAL (ÚLTIMOS 12 MESES)')),
    filtro_conta == FALSE
  ) %>% 
  select(-filtro_conta)


rcl_concatenar <- rcl_concatenar %>% 
  mutate(
    periodo = case_when(
      get_month(date) == 2 ~ 1,
      get_month(date) == 4 ~ 2,
      get_month(date) == 5 ~ 3,
      get_month(date) == 8 ~ 4,
      get_month(date) == 10 ~ 5,
      get_month(date) == 12 ~ 6
    ) %>% as_factor(),
    .after = date
  ) %>% 
  pivot_wider(
  names_from = coluna,
  values_from = valor,
  values_fill = 0
) %>% 
  pivot_longer(
    !(date:cod_conta),
    names_to = 'coluna',
    values_to = 'valor'
  ) %>% 
  relocate(coluna, .before = cod_conta) %>% 
  mutate(
    coluna = fct_relevel(
      coluna,
      "<MR-11>",
      "<MR-10>",
      "<MR-9>",
      "<MR-8>",
      "<MR-7>",
      "<MR-6>",
      "<MR-5>",
      "<MR-4>",
      "<MR-3>",
      "<MR-2>",
      "<MR-1>",
      "<MR>"
    )
  ) %>%   
  arrange(periodo, cod_ibge, coluna)

# Arrumando - Receitas Primárias -------------------------------------

receitas.df <- rp %>% 
  filter(cod_conta %in% cod_conta_filtro_rp & coluna != 'PREVISÃO ATUALIZADA') %>% 
  select(
    exercicio, 
    periodo, 
    periodicidade, 
    uf, 
    cod_ibge, 
    coluna, 
    cod_conta, 
    conta, 
    valor
  ) %>% 
  mutate(
    date = case_when(
      periodo == 1 ~ '02',
      periodo == 2 ~ '04',
      periodo == 3 ~ '06',
      periodo == 4 ~ '08',
      periodo == 5 ~ '10',
      periodo == 6 ~ '12'
    ),
    date = parse_date(
      paste0(exercicio, date),
      '%Y%m'
    ),
    .after = periodicidade
  )

#' Efetuando a atualizaçã monetária

receitas.df <- receitas.df %>% 
  left_join(deflator %>% select(-ipca), by = 'date') %>% 
  mutate(
    valor_constante = valor * deflator
  ) %>% 
  select(-deflator)

# Removendo dados repetidos da base 

#' Incialmente é efetuado a separação da variável *coluna*, pois
#' assim, posteriormente, é possível selecionar as linhas em que o 
#' *Ano de Referência* é igual ao *Exercício*.

receitas.df <- receitas.df %>% 
  separate(
    col = coluna, 
    into = c('coluna', 'ano_referencia'),
    sep = '/'
    ) %>% 
  mutate(
    
    #' A função *str_squish()* retira os espaços desnecessários em
    #' variáveis do tipo _character_.
    
    ano_referencia = str_squish(ano_referencia),
    
    #' Aqui fazemos a seleção das linhas em que o *ano_referencia*
    #' seja igual ao *exercicio*.
    
    remover_linha = ifelse(
      ano_referencia == exercicio | is.na(ano_referencia), 1,0
    )
  ) %>% 
  filter(remover_linha == 1)

#' Por fim, modifica-se os rótulos das linhas de _Até o Bimestre_
#' para * Receitas Realizadas*

receitas.df <- receitas.df %>% 
  mutate(
    coluna = fct_recode(
      coluna,
      "RECEITAS REALIZADAS (a)" = "Até o Bimestre "
    )
  ) %>% 
  select(-ano_referencia, -remover_linha) %>% 
  mutate(
    quadrimestre = case_when(
      periodo == 1 ~ 1,
      periodo == 2 ~ 1,
      periodo == 3 ~ 2,
      periodo == 4 ~ 2,
      periodo == 5 ~ 3,
      periodo == 6 ~ 3
    ),
    .after = periodo
  ) %>% 
  filter(date >= '2018-01-01')

receitas.df <- receitas.df %>% 
  select(
    date,
    cod_ibge,
    coluna,
    cod_conta,
    conta,
    valor, 
    valor_constante
  )

rp_concatenar <- rp_concatenar %>% 
  select(
    date,
    cod_ibge,
    coluna,
    cod_conta,
    conta,
    valor, 
    valor_constante
  )

receitas.df <- receitas.df %>% 
  bind_rows(rp_concatenar)

# Arrumando - Despesas Primárias -------------------------------------

despesas.df <- despesas %>% 
  filter(cod_conta %in% cod_conta_filtro_dp) %>% 
  select(
    exercicio, 
    periodo, 
    periodicidade, 
    uf, 
    cod_ibge, 
    coluna, 
    cod_conta, 
    conta, 
    valor
  ) %>% 
  mutate(
    date = case_when(
      periodo == 1 ~ '02',
      periodo == 2 ~ '04',
      periodo == 3 ~ '06',
      periodo == 4 ~ '08',
      periodo == 5 ~ '10',
      periodo == 6 ~ '12'
    ),
    date = parse_date(
      paste0(exercicio, date),
      '%Y%m'
    ),
    .after = periodicidade
  )

#' Efetuando a atualização monetária

despesas.df <- despesas.df %>%
  left_join(deflator %>% select(-ipca), by = 'date') %>% 
  mutate(
    valor_constante = valor * deflator
  ) %>% 
  select(-deflator)

# Removendo dados repetidos da base 

#' Incialmente é efetuado a separação da variável *coluna*, pois
#' assim, posteriormente, é possível selecionar as linhas em que o 
#' *Ano de Referência* é igual ao *Exercício*.

despesas.df <- despesas.df %>% 
  separate(
    col = coluna, 
    into = c('coluna', 'ano_referencia'),
    sep = '/'
  ) %>% 
  mutate(
    
    #' A função *str_squish()* retira os espaços desnecessários em
    #' variáveis do tipo _character_.
    
    ano_referencia = str_squish(ano_referencia),
    coluna = str_squish(coluna),
    
    #' Aqui fazemos a seleção das linhas em que o *ano_referencia*
    #' seja igual ao *exercicio*.
    
    remover_linha = ifelse(
      ano_referencia == exercicio | is.na(ano_referencia), 1,0
    )
  ) %>% 
  filter(remover_linha == 1)

#' Por fim, modifica-se os rótulos das linhas de _Até o Bimestre_
#' para * Despesas Realizadas*

despesas.df <- despesas.df %>% 
  mutate(
    coluna = fct_recode(
      coluna,
      "DESPESAS EMPENHADAS" = "Despesas Empenhadas Até o Bimestre",
      "DESPESAS EMPENHADAS" = "DESPESAS EMPENHADAS ATÉ O BIMESTRE"
    )
  ) %>% 
  select(-ano_referencia, -remover_linha) %>% 
  mutate(
    quadrimestre = case_when(
      periodo == 1 ~ 1,
      periodo == 2 ~ 1,
      periodo == 3 ~ 2,
      periodo == 4 ~ 2,
      periodo == 5 ~ 3,
      periodo == 6 ~ 3
    ),
    .after = periodo
  ) %>% 
  filter(date >= '2018-01-01')

despesas.df <- despesas.df %>% 
  select(
    date,
    cod_ibge,
    coluna,
    cod_conta,
    conta,
    valor,
    valor_constante
  )

dp_concatenar <- dp_concatenar %>% 
  select(
    date,
    cod_ibge,
    coluna,
    cod_conta,
    conta,
    valor,
    valor_constante
  )

despesas.df <- despesas.df %>% 
  bind_rows(dp_concatenar)

# Arrumando - Investimentos ------------------------------------------

investimento.df <- receitas.df %>% 
  filter(
    cod_conta %in% c('RREO6TransferenciasDeCapital', 'RREO6OperacoesDeCredito')
  ) %>%
  bind_rows(
    despesas.df %>% 
      filter(cod_conta == 'RREO6Investimentos')
  )

investimento.df <- investimento.df %>% 
  select(-coluna, -conta, -valor) %>% 
  mutate(
    cod_conta = str_remove_all(cod_conta, 'RREO6')
  ) %>% 
  pivot_wider(
    names_from = cod_conta,
    values_from = valor_constante,
    values_fill = 0
  ) %>% 
  janitor::clean_names()

# Arrumando - Receita Corrente Líquida ------------------------------------

#' Nesta seção é feito a obtenção das receitas correntes líquidas
#' mensais para cada município cearense

rc_liquida.df <- rc_liquida %>% 
  as_tibble() %>% 
  filter(
    cod_conta %in% c('RREO3ReceitaCorrenteLiquida', 'ReceitaCorrenteLiquida'),
    !(coluna %in% c('TOTAL (ÚLTIMOS 12 MESES)', 'PREVISÃO ATUALIZADA 2018'))
  ) %>% 
  select(
    -demonstrativo, 
    -instituicao, 
    -populacao, 
    -anexo, 
    -rotulo
  ) %>% 
  mutate(
    date = case_when(
      periodo == 1 ~ '02',
      periodo == 2 ~ '04',
      periodo == 3 ~ '06',
      periodo == 4 ~ '08',
      periodo == 5 ~ '10',
      periodo == 6 ~ '12'
    ),
    date = parse_date(
      paste0(exercicio, date),
      '%Y%m'
    ),
    .after = periodicidade
  )

rc_liquida.df <- rc_liquida.df %>% 
  pivot_wider(
    names_from = coluna,
    values_from = valor,
    values_fill = 0
  ) %>% 
  select(-starts_with('PREVISÃO')) %>% 
  pivot_longer(
    !(exercicio:conta),
    names_to = 'coluna',
    values_to = 'valor'
  ) %>% 
  relocate(coluna, .before = cod_conta) %>% 
  mutate(
    coluna = fct_relevel(
      coluna,
      "<MR-11>",
      "<MR-10>",
      "<MR-9>",
      "<MR-8>",
      "<MR-7>",
      "<MR-6>",
      "<MR-5>",
      "<MR-4>",
      "<MR-3>",
      "<MR-2>",
      "<MR-1>",
      "<MR>"
    )
  ) %>%   
  arrange(periodo, cod_ibge, coluna)

rc_liquida.df <- rc_liquida.df %>% 
  filter(date >= '2018-01-01') %>% 
  select(date, periodo, cod_ibge, coluna, valor) %>% 
  bind_rows(
    rcl_concatenar %>% 
      select(date, periodo, cod_ibge, coluna, valor)
  ) %>% 
  arrange(date) %>% 
  mutate(
    defasagem = str_extract_all(coluna, "[[:digit:]]+") %>% as.numeric(),
    defasagem = ifelse(is.na(defasagem), 0, defasagem),
    defasagem = defasagem * -1,
    date_month = add_months(date, defasagem)
  ) %>% 
  relocate(date_month, .after = date)


rc_liquida.df <- rc_liquida.df %>% 
  left_join(deflator %>% select(-ipca), by = 'date') %>% 
  mutate(
    valor_constante = valor * deflator,
    ano = get_year(date),
    mes = get_month(date)
  ) %>% 
  select(-deflator) %>% 
  group_by(cod_ibge) %>% 
  mutate(
    rcl_bimestral = case_when(
      periodo == 1 ~ zoo::rollsum(valor_constante, 2, na.pad = TRUE, align = 'right'),
      periodo == 2 ~ zoo::rollsum(valor_constante, 2, na.pad = TRUE, align = 'right'),
      periodo == 3 ~ zoo::rollsum(valor_constante, 2, na.pad = TRUE, align = 'right'),
      periodo == 4 ~ zoo::rollsum(valor_constante, 2, na.pad = TRUE, align = 'right'),
      periodo == 5 ~ zoo::rollsum(valor_constante, 2, na.pad = TRUE, align = 'right'),
      periodo == 6 ~ zoo::rollsum(valor_constante, 2, na.pad = TRUE, align = 'right')
    )
  ) %>% 
  ungroup() %>% 
  filter(coluna == '<MR>') %>% 
  select(date, cod_ibge, rcl_bimestral)
  
# Arrumando - Serviço da Dívida -------------------------------------------

#' *Finalizando base* selecionando os _serviços da dívida_ de cada
#' município.

servico_divida <- despesas.df %>% 
  mutate(
    periodo = case_when(
      get_month(date) == 2 ~ 1,
      get_month(date) == 4 ~ 2,
      get_month(date) == 5 ~ 3,
      get_month(date) == 8 ~ 4,
      get_month(date) == 10 ~ 5,
      get_month(date) == 12 ~ 6
    ) %>% as_factor(),
    .before = date
  ) %>% 
  filter(cod_conta %in% cod_conta_filtro_sd) %>% 
  group_by(
    exercicio = get_year(date), periodo, date, cod_ibge
  ) %>% 
  summarise(servico_divida = sum(valor_constante)) %>% 
  ungroup()

# Arrumando - Despesas com pessoal -----------------------------------

despesa_pessoal <- fs::dir_ls(
  here::here('data/siconfi/despesas/d_pessoal/mun/')
) %>% 
  map_df(
    .f = ~read_csv2(
      .,
      skip = 6,
      locale = locale(encoding = 'latin1'),
      col_types =   cols(
        Instituição = col_character(),
        Cod.IBGE = col_double(),
        UF = col_character(),
        PODER = col_character(),
        População = col_double(),
        Coluna = col_character(),
        Conta = col_character(),
        Valor = col_double(),
        date = col_date(format = '%Y%m%d')
      )
    ) %>% 
      janitor::clean_names() %>% 
      relocate(date)
  )

despesa_pessoal_uf <- fs::dir_ls(
  here::here('data/siconfi/despesas/d_pessoal/uf/')
) %>% 
  map_df(
    .f = ~read_csv2(
      .,
      skip = 6,
      locale = locale(encoding = 'latin1'),
      col_types =   cols(
        Instituição = col_character(),
        Cod.IBGE = col_double(),
        UF = col_character(),
        PODER = col_character(),
        População = col_double(),
        Coluna = col_character(),
        Conta = col_character(),
        Valor = col_double(),
        date = col_date(format = '%Y%m%d')
      )
    ) %>% 
      janitor::clean_names() %>% 
      relocate(date) %>% 
      filter(uf == 'CE')
  )


d_pessoal_uf.df <- despesa_pessoal_uf %>% 
  mutate(
    filtro_conta = str_detect(conta, 'DESPESA TOTAL COM PESSOAL')
  ) %>% 
  filter(
    coluna == '% sobre a RCL Ajustada',
    filtro_conta == TRUE
  ) %>% 
  mutate(
    conta = fct_recode(
      conta,
      "DTP" = "DESPESA TOTAL COM PESSOAL - DTP (VII) = (III a + III b)",
      "DTP" = "DESPESA TOTAL COM PESSOAL - DTP (VII) = (IIIa + IIIb)",
      "DTP" = "DESPESA TOTAL COM PESSOAL - DTP (VIII) = (III a + III b)",
      "DTP" = "DESPESA TOTAL COM PESSOAL - DTP (VIII) = (IIIa + IIIb)"
    ) %>% as.character()
  ) %>% 
  select(-filtro_conta) %>% select(date, cod_ibge, uf, poder, valor) %>% 
  filter(poder %in% c('Legislativo', 'Executivo')) %>% 
  pivot_wider(
    names_from = poder,
    values_from = valor,
    values_fn = sum
  ) %>% 
  mutate(
    total = Executivo + Legislativo
  ) %>% 
  arrange(date)

d_pessoal.df <- despesa_pessoal %>% 
  mutate(
    filtro_conta = str_detect(conta, 'DESPESA TOTAL COM PESSOAL')
  ) %>% 
  filter(
    coluna == '% sobre a RCL Ajustada',
    filtro_conta == TRUE
  ) %>% 
  mutate(
    conta = fct_recode(
      conta,
      "DTP" = "DESPESA TOTAL COM PESSOAL - DTP (VII) = (III a + III b)",
      "DTP" = "DESPESA TOTAL COM PESSOAL - DTP (VII) = (IIIa + IIIb)",
      "DTP" = "DESPESA TOTAL COM PESSOAL - DTP (VIII) = (III a + III b)",
      "DTP" = "DESPESA TOTAL COM PESSOAL - DTP (VIII) = (IIIa + IIIb)"
    ) %>% as.character()
  ) %>% 
  select(-filtro_conta) %>% select(date, cod_ibge, poder, valor) %>% 
  pivot_wider(
    names_from = poder,
    values_from = valor
  ) %>% 
  mutate(
    total = Executivo + Legislativo
  ) %>% 
  arrange(cod_ibge, date)

# Arrumando - Dívida Corrente Líquida --------------------------------

dcl <- fs::dir_ls(
  here::here('data/siconfi/dcl')
) %>% 
  map_df(
    .f = ~readr::read_csv2(
      .,
      skip = 6,
      locale = locale(encoding = 'latin1'),
      col_types =   cols(
        Instituição = col_character(),
        Cod.IBGE = col_double(),
        UF = col_character(),
        PODER = col_character(),
        População = col_double(),
        Coluna = col_character(),
        Conta = col_character(),
        Valor = col_double(),
        date = col_date(format = '%Y%m%d')
      )
    ) %>% 
      janitor::clean_names() %>% 
      relocate(date)
  )

dcl.df <- dcl %>% 
  mutate(
    filtro_conta = str_detect(conta, '% da DC'),
    mes = clock::get_month(date),
    quadrimestre = case_when(
      mes == 4 ~ 1,
      mes == 8 ~ 2,
      mes == 12 ~ 3
    )
  ) %>%
  filter(
    filtro_conta == TRUE,
    coluna != 'SALDO DO EXERCÍCIO ANTERIOR',
    conta %in% c(
      '% da DCL sobre a RCL (III/RCL)', 
      '% da DCL sobre a RCL AJUSTADA (III/VI)')
  ) %>% 
  select(-filtro_conta) %>% 
  mutate(
    new_col = str_extract_all(coluna, "[[:digit:]]+") %>% as.numeric(),
    filtro_coluna = case_when(
      quadrimestre == new_col ~ TRUE
    )
  ) %>% 
  drop_na(filtro_coluna) %>% 
  select(-(mes:filtro_coluna))

# Criando Balancete das Receitas -------------------------------------

balancete_receitas <- receitas.df %>% 
  select(date, cod_ibge, cod_conta, valor_constante) %>% 
  pivot_wider(
    names_from = cod_conta,
    values_from = valor_constante
  ) %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .))

balancete_receitas.wide <- balancete_receitas %>%
  mutate(
    
    #' Variável *OUTROS IMPOSTOS*
    
    outros_impostos = rowSums(
      across(
        c(RREO6IRRF, RREO6OutrasReceitasTributarias)
      )
    ),
    
    #' Variável *DEMAIS RECEITAS CORRENTES*
    
    demais_rc = rowSums(
      across(
        c(
          RREO6DemaisReceitasCorrentes,
          RREO6ReceitaPatrimonial
        )
      )
    ),
    
    #' Variável *DEMAIS TRANSFERÊNCIAS*
    
    demais_transferencias = rowSums(
      across(
        c(
          RREO6CotaParteDoIPVA,
          CotaParteDoITR,
          RREO6OutrasTransferenciasCorrentes,
          TransferenciasDaLC871996,
          TransferenciasDaLCn611989
        )
      )
    ),
    
    #' Variável *DEMAIS RECEITAS DE CAPITAL*
    
    demais_rec_capital = rowSums(
      across(
        c(
          RREO6AlienacaoDeBens,
          RREO6AmortizacaoDeEmprestimos
        )
      )
    )
    
  ) %>% 
  select(
    date, 
    cod_ibge,
    ReceitasCorrentes,
    RREO6ReceitasTributarias,
    RREO6IPTU,
    RREO6ISS,
    RREO6ITBI,
    outros_impostos,
    RREO6TransferenciasCorrentes,
    RREO6CotaParteDoFPM,
    RREO6CotaParteDoICMS,
    TransferenciasDoFUNDEB,
    demais_transferencias,
    demais_rc,
    RREO6ReceitasDeCapital,
    RREO6OperacoesDeCredito,
    RREO6TransferenciasDeCapital,
    demais_rec_capital
  ) %>% 
  janitor::clean_names()


balancete_receitas_uf <- balancete_receitas.wide %>% 
  pivot_longer(
    !(date:cod_ibge),
    names_to = 'conta',
    values_to = 'valor'
  ) %>% 
  group_by(date, conta) %>% 
  summarise(
    valor = sum(ifelse(is.na(valor), 0, valor))
  ) %>% 
  ungroup() %>% 
  mutate(
    conta = str_remove_all(conta, 'rreo6')
  ) %>% 
  arrange(valor)

balancete_receitas_mun <- balancete_receitas.wide %>% 
  pivot_longer(
    !(date:cod_ibge),
    names_to = 'conta',
    values_to = 'valor'
  ) %>% 
  group_by(date, cod_ibge, conta) %>% 
  summarise(
    valor = sum(ifelse(is.na(valor), 0, valor))
  ) %>% 
  ungroup() %>% 
  mutate(
    conta = str_remove_all(conta, 'rreo6')
  ) %>% 
  arrange(cod_ibge, valor) %>% 
  mutate(
    tp_conta = 'receita',
    .before = conta
  )

# Criando Balancete das Despesas -------------------------------------

balancete_despesa_mun <- despesas.df %>% 
  select(date, cod_ibge, cod_conta, valor_constante) %>% 
  mutate(
    cod_conta = str_remove(cod_conta, 'RREO6')
  ) %>% 
  pivot_wider(
    names_from = cod_conta,
    values_from = valor_constante
  ) %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>% 
  janitor::clean_names() %>% 
  mutate(
    desp_total = despesas_correntes + despesas_de_capital
  )


balancete_despesa_mun <- balancete_despesa_mun %>%
  pivot_longer(
    !c(date, cod_ibge),
    names_to = "conta",
    values_to = "valor"
  ) %>%
  group_by(date, cod_ibge, conta) %>%
  summarise(
    valor = sum(ifelse(is.na(valor), 0, valor))
  ) %>%
  ungroup() %>% 
  mutate(
    tp_conta = 'despesa',
    .before = conta
  )


balancete_despesa_uf <- despesas.df %>% 
  select(date, cod_ibge, cod_conta, valor_constante) %>% 
  mutate(
    cod_conta = str_remove(cod_conta, 'RREO6')
  ) %>% 
  pivot_wider(
    names_from = cod_conta,
    values_from = valor_constante
  ) %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>% 
  janitor::clean_names() %>% 
  mutate(
    desp_total = despesas_correntes + despesas_de_capital
  ) %>%
  pivot_longer(
    !c(date, cod_ibge),
    names_to = "conta",
    values_to = "valor"
  ) %>%
  group_by(date, conta) %>%
  summarise(
    valor = sum(ifelse(is.na(valor), 0, valor))
  ) %>%
  ungroup() %>% 
  mutate(
    tp_conta = 'despesa',
    .before = conta
  )

# Consolidando balancetes de receitas e despesas ---------------------

#' _Balancetes Municipais_

balancete_mun <- balancete_receitas_mun %>% 
  bind_rows(balancete_despesa_mun) %>% 
  arrange(cod_ibge, tp_conta)

balancete_uf <- balancete_receitas_uf %>% 
  bind_rows(balancete_despesa_uf) %>% 
  arrange(tp_conta)

# Save data ----------------------------------------------------------

#' _Salvando_ balancete das *Unidades Federativas*

write_csv2(
  balancete_uf,
  here::here('out/dados_consolidados/balancetes/balancete_uf.csv'),
  na = ""
)

#' _Salvando_ balancetes dos *Municípios*

write_csv2(
  balancete_mun,
  here::here('out/dados_consolidados/balancetes/balancete_mun.csv'),
  na = ""
)

#' _Salvando_  *Receitas Correntes Líquidas dos Municípios*

write_csv2(
  rc_liquida.df,
  here::here('out/dados_consolidados/rcl/rc_liquida.df.csv'),
  na = ""
)

#' _Salvando_  *Investimentos dos Municípios*

write_csv2(
  investimento.df,
  here::here('out/dados_consolidados/investimento/investimento.df.csv'),
  na = ""
)

write_csv2(
  servico_divida,
  here::here('out/dados_consolidados/servico_divida/servico_divida.csv'),
  na = ""
)


#' _Salvando_  *Despesa com Pessoal*

write_csv2(
  d_pessoal_uf.df,
  here::here('out/dados_consolidados/despesa_pessoal/d_pessoal_uf.csv'),
  na = ""
)

write_csv2(
  d_pessoal.df,
  here::here('out/dados_consolidados/despesa_pessoal/d_pessoal.csv'),
  na = ""
)

#' _Salvando_  *Dívida Corrente Líquida*

write_csv2(
  dcl.df,
  here::here('out/dados_consolidados/dcl/dcl.csv'),
  na = ""
)


