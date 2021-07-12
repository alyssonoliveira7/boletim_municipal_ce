#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#         Sintaxe para o Boletim das Finças Públicas Municipais
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#              Indicadores de Despesa de Pessoal e Dívida
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
# Como serão usados apenas os percentuais relativos a RCL não será necessário 
# usar o IPCA, passando direto para o segundo passo
#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
# Segundo passo é baixar os dados que se deseja apresentar. 
# Por simplicidade todos os dados a serem utilizados serão elencados aqui. Dada
# a decisão de usar dados baixados deve-se tomar cuidado para informar os 
# períodos para os quais se deseja calcular os indicadores.
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#
#              DADOS 
#
# Procedimentos para baixar os dados e deixá-los compátiveis para leitura
# 1 - Baixar dados do Sincofi
# 2 - Apagar as três primeiras linhas
# 3 - Criar variável data e atribuir valor anomes (ex. 200804)
# 4 - Retirar acentos das variáveis populção e Instituição
# 5 - Corrigir o nome da Coluna 
# 6 - Corrigir acentuações 
# 7 - Excluir virgulas e trocar espaços por "_"
# 8 - Trocar "-" e "/" por "_"
# 9 - Excluir a parte (*) 
# 10 - Salvar como csv
#----------------------------------------------------------

pessoal_p1 <- read_csv2("data/pessoal_201804.csv", na=c("")) # Informar 1° periodo

pessoal_p2 <- read_csv2("data/pessoal_201904.csv", na=c("")) # Informar 2° periodo

pessoal <- rbind(pessoal_p1, pessoal_p2)

divida_p1 <- read_csv2("data/divida_201804.csv", na=c("")) # Informar 1° periodo

divida_p2 <- read_csv2("data/divida_201904.csv", na=c("")) # Informar 2° periodo



#------------------------------------------------------------------------------
# Filtrando apenas o percentual do gasto do Pessoal do Legislativo e Executivo
#------------------------------------------------------------------------------

pessoal <- pessoal %>%
  filter(Coluna == '% sobre a RCL Ajustada' ) %>%
  select(
    -(Coluna)) %>%
  filter(Conta == 'DESPESA_TOTAL_COM_PESSOAL__DTP_VII__IIIa__IIIb' |
         Conta == 'DESPESA_TOTAL_COM_PESSOAL__DTP_VII__III_a__III_b') %>%
  select(
    -(Conta))

#------------------------------------------------------------------------------
# Selecionando apenas a soma do gasto do Pessoal do Legislativo e Executivo
#------------------------------------------------------------------------------

pessoal2 <- pessoal %>%
  select(data, Cod.IBGE, PODER, Valor) %>%
  mutate(Cod.IBGE = as.character(Cod.IBGE)) %>%
  pivot_wider(
    names_from = PODER,
    values_from = Valor) %>%
  mutate(Total = Executivo 
               + Legislativo) %>%
  select(data, Cod.IBGE, Total) %>%
  pivot_wider(
    names_from = data,
    values_from = Total) %>%
  rename(p201804 = '201804') %>%
  rename(p201904 = '201904')

#------------------------------------------------------------------------------
# Cosntruindo histograma d0 gasto de Pessoal do Legislativo e Executivo
#------------------------------------------------------------------------------

## Cutting pessoal2$p201804 into pessoal2$p201804_rec
pessoal2$p201804_rec <- cut(pessoal2$p201804,
  include.lowest = FALSE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 45, 50, 55, 60, 65, 70, 100)
)

## Cutting pessoal2$p201904 into pessoal2$p201904_rec
pessoal2$p201904_rec <- cut(pessoal2$p201904,
                            include.lowest = FALSE,
                            right = FALSE,
                            dig.lab = 4,
                            breaks = c(0, 45, 50, 55, 60, 65, 70, 100)
)

IPessoal_p1 <- pessoal2 %>%
  group_by(p201804_rec) %>%
  summarise(frequencia=n()) %>%
  rename(intervalo = p201804_rec)

IPessoal_p2 <- pessoal2 %>%
  group_by(p201904_rec) %>%
  summarise(frequencia=n()) %>%
  rename(intervalo = p201904_rec)


(IPessoal <- full_join(IPessoal_p1,IPessoal_p2, by = 'intervalo'))

IPessoal_p1g <- IPessoal %>% 
  ggplot(aes(x = intervalo, y = frequencia.x)) +
  geom_col() +
  # adicionar camada de texto
  geom_label(aes(label = frequencia.x)) +
  labs(title = 'Gasto Pessoal em 2018',
       y = 'Frequencia',
       x = 'Intervalo (% RCL)')

IPessoal_p2g <- IPessoal %>% 
  ggplot(aes(x = intervalo, y = frequencia.y)) +
  geom_col() +
  # adicionar camada de texto
  geom_label(aes(label = frequencia.y)) +
  labs(title = 'Gasto Pessoal em 2019',
       y = 'Frequencia',
       x = 'Intervalo (% RCL)')

grid.arrange(IPessoal_p1g , IPessoal_p2g , ncol=2)

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


#------------------------------------------------------------------------------
# Filtrando apenas o percentual da DCL em relação a RCL
#------------------------------------------------------------------------------

divida <- rbind(divida_p1, divida_p2)

divida <- divida %>%
  filter(Coluna == 'Ate o 1 Quadrimestre' ) %>%   # Ajustar esse dado se for outro quadrimeste
  select(
    -(Coluna)) %>%
  filter(Conta == '%_da_DCL_sobre_aRCL_III/RCL') %>%
  select(
    -(Conta))

#------------------------------------------------------------------------------
# Selecionando apenas a soma do gasto do Pessoal do Legislativo e Executivo
#
# No caso da dívida é necessário analisar se há, em algum período há dívida do 
# Legislativo
#------------------------------------------------------------------------------

divida2 <- divida %>%
  select(data, Cod.IBGE, Valor) %>%
  mutate(Cod.IBGE = as.character(Cod.IBGE)) %>%
  pivot_wider(
    names_from = data,
    values_from = Valor) %>%
  rename(p201804 = '201804') %>%
  rename(p201904 = '201904')

#------------------------------------------------------------------------------
# Cosntruindo histograma d0 gasto de Pessoal do Legislativo e Executivo
#------------------------------------------------------------------------------

## Cutting divida2$p201804 into divida2$p201804_rec
divida2$p201804_rec <- cut(divida2$p201804,
  include.lowest = FALSE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(-100, 0, 20, 40, 60, 100, 150, 200)
)

## Cutting divida2$p201904 into divida2$p201904_rec
divida2$p201904_rec <- cut(divida2$p201904,
                           include.lowest = FALSE,
                           right = FALSE,
                           dig.lab = 4,
                           breaks = c(-100, 0, 20, 40, 60, 100, 150, 200)
)


IDivida_p1 <- divida2 %>%
  group_by(p201804_rec) %>%
  summarise(frequencia=n()) %>%
  rename(intervalo = p201804_rec)

IDivida_p2 <- divida2 %>%
  group_by(p201904_rec) %>%
  summarise(frequencia=n()) %>%
  rename(intervalo = p201904_rec)


(IDivida <- full_join(IDivida_p1,IDivida_p2, by = 'intervalo'))

IDivida_p1g <- IDivida %>% 
  ggplot(aes(x = intervalo, y = frequencia.x)) +
  geom_col() +
  # adicionar camada de texto
  geom_label(aes(label = frequencia.x)) +
  labs(title = 'DCL/RCL em 2018',
       y = 'Frequencia',
       x = 'Intervalo (% RCL)')

IDivida_p2g <- IDivida %>% 
  ggplot(aes(x = intervalo, y = frequencia.y)) +
  geom_col() +
  # adicionar camada de texto
  geom_label(aes(label = frequencia.y)) +
  labs(title = 'DCL/RCL em 2019',
       y = 'Frequencia',
       x = 'Intervalo (% RCL)')

grid.arrange(IDivida_p1g , IDivida_p2g , ncol=2)
