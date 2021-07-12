#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#         Sintaxe para o Boletim das Finças Públicas Municipais
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#      Balancetes e Indicadores Contábeis Derivados dos Balancetes
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Ativar Pacotes
#------------------------------------------------------------------------------

library(tidyverse)

library(gridExtra)

#------------------------------------------------------------------------------
# A opção será por baixar os dados manualmente, porém é possível baixá-los 
# diretament pelo pacote Rsiconfi, entretanto essa opção torna-se mais demorada
# por necessitar de fazer cada download de forma individual e tonrar-se 
# dependente da conexão com Internet.
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Primeiro carregar o IPCA para correção de valores e escolher o mês que será
# atualizado, informando o mes (no formato anomes) no tiddle referência
#------------------------------------------------------------------------------

ipca <- read_csv2("data/ipca.csv", na=c(""))

referencia <- filter(ipca, data==202012) # Informar o mês de referência aqui

ipca <-  ipca %>%
  mutate(indice=referencia$ipca/ipca) 

#------------------------------------------------------------------------------
# Segundo passo é baixar os dados que se deseja apresentar. 
# Por simplicidade todos os dados a serem utilizados serão elencados aqui. Dada
# a decisão de usar dados baixados deve-se tomar cuidado para informar os 
# períodos para os quais se deseja calcular os indicadores.
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#
#              DADOS RECEITAs
#
# Procedimentos para baixar os dados e deixá-los compátiveis para leitura
# 1 - Baixar dados do Sincofi
# 2 - Apagar as três primeiras linhas
# 3 - Criar variável periodo e atribuir valor anomes (ex. 200804)
# 4 - Retirar acentos das variáveis populção e Instituição
# 5 - Corrigir o nome da Coluna para "RECEITAS REALIZADAS"
# 6 - Corrigir acentuações dos valores em "Conta"
# 7 - Excluir virgulas e trocar espaços por "_"
# 8 - Trocar "-" e "/" por "_"
# 9 - Excluir a parte (*) constante nas Receitas correntes e de capital
# 10 - Salvar como csv
#----------------------------------------------------------

#------------------------------------------------------------------------------
#
#              DADOS Despesas
#
# Procedimentos para baixar os dados e deixá-los compátiveis para leitura
# 1 - Baixar dados do Sincofi
# 2 - Apagar as cinco primeiras linhas
# 3 - Criar variável data e atribuir valor anomes (ex. 200804)
# 4 - Retirar acentos das variáveis populção e Instituição
# 5 - Corrigir o nome da Coluna para "RECEITAS REALIZADAS"
# 6 - Corrigir acentuações dos valores em "Conta"
# 7 - Excluir virgulas e trocar espaços por "_"
# 8 - Trocar "-", "=", "+" e "/" por "_"
# 9 - Excluir a parte (*) constante nas Receitas correntes e de capital
# 10 - Salvar como csv
#----------------------------------------------------------

receitas_p1 <- read_csv2("data/rp_201804.csv", na=c("")) # Informar 1° periodo

receitas_p2 <- read_csv2("data/rp_201904.csv", na=c("")) # Informar 2° periodo

despesas_p1 <- read_csv2("data/dp_201804.csv", na=c("")) # Informar 1° periodo

despesas_p2 <- read_csv2("data/dp_201904.csv", na=c("")) # Informar 2° periodo

receitas <- rbind(receitas_p1, receitas_p2)

despesas <- rbind(despesas_p1, despesas_p2)

receitas <- rename(receitas, data = periodo)

codigos <- read_csv2("data/cod_contas.csv", na=c(""))

grupo <- group_by(receitas, data)
summarize(grupo, mean=mean(Valor) )

#-----------------------------------------
# Aplicando valores constantes
#-----------------------------------------

receitas <- receitas %>%
  left_join(ipca, by = "data") %>%
  mutate(receitas, Valor_constante = Valor*indice) %>%
  select(
    -(mes:indice)
  ) %>%
  filter(Coluna == 'RECEITAS REALIZADAS') %>%
  select(
    -(Coluna)
  )
 
despesas <- despesas %>%
  left_join(ipca, by = "data") %>%
  mutate(despesas, Valor_constante = Valor*indice) %>%
  select(
    -(mes:indice)
  ) %>%
  filter(Coluna == 'DESPESAS EMPENHADAS') %>%
  select(
    -(Coluna)
  )

#-----------------------------------------------------------
# Fazendo transformações para deixar no formato de balancete
# Primeiro passo é o de agrupar linhas menos significativas
# e mudar alguns nomes.
# O segundo passo foi criar um arquivo só com as receitas de interesse em 
#----------------------------------------------------------- 

receitas2 <- receitas %>%
  select(data, Cod.IBGE, Conta, Valor_constante) %>%
  mutate(Cod.IBGE = as.character(Cod.IBGE)) %>%
  pivot_wider(
    names_from = Conta,
    values_from = Valor_constante) %>%
  rename(Receitas_Correntes = RECEITAS_CORRENTES) %>%
  rename(Tributaria = Impostos_Taxas_e_Contribuicoes_de_Melhoria) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
  mutate(Outros_impostos = Tributaria 
                         - ITBI
                         - ISS
                         - IPTU ,
         Demais_RC = Receitas_Correntes
                   - Transferencias_Correntes
                   - Tributaria,
         Demais_transferencias = Transferencias_Correntes
                               - Cota_Parte_do_FPM
                               - Cota_Parte_do_ICMS
                               - Transferencias_do_FUNDEB,
         Demais_Rec_Capital = RECEITAS_DE_CAPITAL_V
                            - Operacoes_de_Credito_VI
                            - Transferencias_de_Capital
         ) 

balancete_receitas <- receitas2 %>%
  select(data, Cod.IBGE, Receitas_Correntes, 
         Tributaria, IPTU, ISS, ITBI,
         Outros_impostos, Transferencias_Correntes,
         Cota_Parte_do_FPM, Cota_Parte_do_ICMS, 
         Transferencias_do_FUNDEB, Demais_transferencias,
         Demais_RC, RECEITAS_DE_CAPITAL_V, Operacoes_de_Credito_VI, 
         Transferencias_de_Capital, Demais_Rec_Capital)


balancete_receitas2 <- balancete_receitas %>% 
  mutate(Rec_Total = Receitas_Correntes + RECEITAS_DE_CAPITAL_V) %>%
  pivot_longer(
    !(data:Cod.IBGE),
    names_to = 'conta',
    values_to = 'Valor'
  ) %>% 
  left_join(codigos, by = "conta") %>%
  group_by(data, codigo, conta) %>% 
  summarise(
    Valor = sum(ifelse(is.na(Valor), 0, Valor))
  ) %>%
  pivot_wider(
    names_from = data,
    values_from = Valor) %>%
  rename(p201804 = '201804') %>%
  rename(p201904 = '201904') %>%
  mutate(variacao = 100*(p201904/p201804)-100)

# -------------------------------------------------------------
#           SEGUNDO PASSO
# Aqui finaliza a primeira parte das receitas
# Desse ponto em diante será realizado o balancete das despesas
# No caso das despesas foram consideradas as DEPESAS EMPENHADAS,
# que foram selecionadas na linha 115.
# Dada a estrutura dos dados de despesas não foi necessário
# transfromação de dados.
# -------------------------------------------------------------

despesas2 <- despesas %>%
  select(data, Cod.IBGE, Conta, Valor_constante) %>%
  mutate(Cod.IBGE = as.character(Cod.IBGE)) %>%
  pivot_wider(
    names_from = Conta,
    values_from = Valor_constante)
  
balancete_despesas <- despesas2 %>%
  select(data, 
         Cod.IBGE, 
         DESPESAS_CORRENTES_XIII,
         Pessoal_e_Encargos_Sociais,
         Juros_e_Encargos_da_Divida_XIV,
         Outras_Despesas_Correntes,
         DESPESAS_DE_CAPITAL_XVI,
         Investimentos,
         Inversoes_Financeiras,
         Amortizacao_da_Divida_XX) %>%
  mutate(Desp_Total = DESPESAS_CORRENTES_XIII + DESPESAS_DE_CAPITAL_XVI)

balancete_despesas2 <- balancete_despesas %>% 
  pivot_longer(
    !(data:Cod.IBGE),
    names_to = 'conta',
    values_to = 'Valor'
  ) %>% 
  left_join(codigos, by = "conta") %>%
  group_by(data, codigo, conta) %>% 
  summarise(
    Valor = sum(ifelse(is.na(Valor), 0, Valor))
  ) %>%
  pivot_wider(
    names_from = data,
    values_from = Valor) %>%
  rename(p201804 = '201804') %>%
  rename(p201904 = '201904') %>%
  mutate(variacao = 100*(p201904/p201804)-100)


# -------------------------------------------------------------
#           TERCEIRO PASSO
#
# Juntar os balancetes parciais de receitas e despesas e salvar
# no formato Excell
# -------------------------------------------------------------


balancete <- rbind(balancete_receitas2, balancete_despesas2)

writexl::write_xlsx(
  list(
    'balancete' = balancete),
  'out/balancete.xlsx'
)

# -------------------------------------------------------------
#           Quarto PASSO
#
# Calcular os Indicadores contábeis por município e depois o geral
# -------------------------------------------------------------

# Indicador Independência fiscal 
# (Arrecadacao própria / Arrecadação total)

Independencia_Fiscal <- balancete_receitas %>%
  mutate(independencia_fiscal = 100*Tributaria 
                              / (Receitas_Correntes+RECEITAS_DE_CAPITAL_V)) %>%
  select(data,
       Cod.IBGE,
       independencia_fiscal) %>%
  pivot_wider(
    names_from = data,
    values_from = independencia_fiscal) %>%
  rename(p201804 = '201804') %>%
  rename(p201904 = '201904')

## Cutting Independencia_Fiscal$p201804 into Independencia_Fiscal$p201804_rec
Independencia_Fiscal$p201804_rec <- cut(Independencia_Fiscal$p201804,
                                        include.lowest = FALSE,
                                        right = FALSE,
                                        dig.lab = 4,
                                        breaks = c(0, 5, 10, 15, 20, 25, 30, 100)
)

## Cutting Independencia_Fiscal$p201904 into Independencia_Fiscal$p201904_rec
Independencia_Fiscal$p201904_rec <- cut(Independencia_Fiscal$p201904,
                                        include.lowest = FALSE,
                                        right = FALSE,
                                        dig.lab = 4,
                                        breaks = c(0, 5, 10, 15, 20, 25, 30, 100)
)

IF_p1 <- Independencia_Fiscal %>%
  group_by(p201804_rec) %>%
  summarise(frequencia=n()) %>%
  rename(intervalo = p201804_rec)

IF_p2 <- Independencia_Fiscal %>%
  group_by(p201904_rec) %>%
  summarise(frequencia=n()) %>%
  rename(intervalo = p201904_rec)


(IF <- full_join(IF_p1,IF_p2, by = 'intervalo'))

 IF_p1g <- IF %>% 
  ggplot(aes(x = intervalo, y = frequencia.x)) +
  geom_col() +
  # adicionar camada de texto
  geom_label(aes(label = frequencia.x)) +
  labs(title = 'Independência Fiscal em 2018',
        y = 'Frequencia',
        x = 'Intervalo (%)')

IF_p2g <- IF %>% 
  ggplot(aes(x = intervalo, y = frequencia.y)) +
  geom_col() +
  # adicionar camada de texto
  geom_label(aes(label = frequencia.y)) +
  labs(title = 'Independência Fiscal em 2019',
       y = 'Frequencia',
       x = 'Intervalo (%)')

grid.arrange(IF_p1g , IF_p2g , ncol=2)

 
# Indicador Rigidez Despesa 
# (Despesa corrente / Despesa total)

Custeio_Total <- balancete_despesas %>%
  mutate(custeio_total = DESPESAS_CORRENTES_XIII*100/Desp_Total) %>%
  select(data,
         Cod.IBGE,
         custeio_total) %>%
  pivot_wider(
    names_from = data,
    values_from = custeio_total) %>%
  rename(p201804 = '201804') %>%
  rename(p201904 = '201904')

## Cutting Custeio_Total$p201804 into Custeio_Total$p201804_rec
Custeio_Total$p201804_rec <- cut(Custeio_Total$p201804,
  include.lowest = FALSE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(60, 80, 85, 90, 95, 100)
)

## Cutting Custeio_Total$p201904 into Custeio_Total$p201904_rec
Custeio_Total$p201904_rec <- cut(Custeio_Total$p201904,
                                 include.lowest = FALSE,
                                 right = FALSE,
                                 dig.lab = 4,
                                 breaks = c(60, 80, 85, 90, 95, 100)
)

CT_p1 <- Custeio_Total %>%
  group_by(p201804_rec) %>%
  summarise(frequencia=n()) %>%
  rename(intervalo = p201804_rec)

CT_p2 <- Custeio_Total %>%
  group_by(p201904_rec) %>%
  summarise(frequencia=n()) %>%
  rename(intervalo = p201904_rec)

(CT <- full_join(CT_p1,CT_p2, by = 'intervalo'))

CT_p1g <- CT %>% 
  ggplot(aes(x = intervalo, y = frequencia.x)) +
  geom_col() +
  # adicionar camada de texto
  geom_label(aes(label = frequencia.x)) +
  labs(title = 'Custeio/Total em 2018',
       y = 'Frequencia',
       x = 'Intervalo (%)')

CT_p2g <- CT %>% 
  ggplot(aes(x = intervalo, y = frequencia.y)) +
  geom_col() +
  # adicionar camada de texto
  geom_label(aes(label = frequencia.y)) +
  labs(title = 'Custeio/Total em 2019',
       y = 'Frequencia',
       x = 'Intervalo (%)')

grid.arrange(CT_p1g , CT_p2g , ncol=2)

# Indicador Investimento com Recursos Próprios 
# [Investimentos - Transf. Capital - Op. Credito) / Investimento]


Investimento_RP <- receitas2 %>%
  left_join(despesas2, by =c("data", "Cod.IBGE")) %>%
  select(data,
         Cod.IBGE,
         Investimentos,
         Operacoes_de_Credito_VI, 
         Transferencias_de_Capital) %>%
  mutate(investimento_rp = (100*( Investimentos 
                                - Operacoes_de_Credito_VI 
                                - Transferencias_de_Capital)
                            /Investimentos)) %>%
  select(data,
         Cod.IBGE,
         investimento_rp) %>%
  pivot_wider(
    names_from = data,
    values_from = investimento_rp) %>%
  rename(p201804 = '201804') %>%
  rename(p201904 = '201904')

## Cutting Investimento_RP$p201804 into Investimento_RP$p201804_rec
Investimento_RP$p201804_rec <- cut(Investimento_RP$p201804,
  include.lowest = FALSE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(-1000, 0, 25, 50, 75, 100))
  
## Cutting Investimento_RP$p201904 into Investimento_RP$p201904_rec
Investimento_RP$p201904_rec <- cut(Investimento_RP$p201904,
                                   include.lowest = FALSE,
                                   right = FALSE,
                                   dig.lab = 4,
                                   breaks = c(-1000, 0, 25, 50, 75, 100))

IRP_p1 <- Investimento_RP %>%
  group_by(p201804_rec) %>%
  summarise(frequencia=n()) %>%
  rename(intervalo = p201804_rec)

IRP_p2 <- Investimento_RP %>%
  group_by(p201904_rec) %>%
  summarise(frequencia=n()) %>%
  rename(intervalo = p201904_rec)

(IRP <- full_join(IRP_p1,IRP_p2, by = 'intervalo'))

IRP_p1g <- IRP %>% 
  ggplot(aes(x = intervalo, y = frequencia.x)) +
  geom_col() +
  # adicionar camada de texto
  geom_label(aes(label = frequencia.x)) +
  labs(title = 'Investimento com Recursos Próprios em 2018',
       y = 'Frequencia',
       x = 'Intervalo (%)')

IRP_p2g <- IRP %>% 
  ggplot(aes(x = intervalo, y = frequencia.y)) +
  geom_col() +
  # adicionar camada de texto
  geom_label(aes(label = frequencia.y)) +
  labs(title = 'Investimento com Recursos Próprios em 2019',
       y = 'Frequencia',
       x = 'Intervalo (%)')


grid.arrange(IRP_p1g , IRP_p2g , ncol=2)
