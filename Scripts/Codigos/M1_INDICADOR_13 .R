# Curso Visualização e Análise de Dados com Software R
# Aplicadas à Vigilância em Saúde
# Programa de Fortalecimento da Epidemiologia nos Serviços de Saúde - PROFEPI

# Módulo 1 - Análise indicador 13 do PQA-VS

# Carregamento dos pacotes necessários ------------------------------------

library(rio)        # Pacote para ler arquivos
library(tidyverse)  # Pacote para manipulação de dados
library(janitor)    # Pacote para manipulação de dados


# Carregamento das bases de dados para análise ----------------------------

ac_mat_biologico <- import("F:/PROJETO_CURSO_R/Curso 2/dados/ACD_MATBIO_PROFEPI_NOVO.dbf")

ac_trabalho <- import("F:/PROJETO_CURSO_R/Curso 2/dados/ACD_TRAB_PROFEPI_NOVO.dbf")

intox_exogena <- import("F:/PROJETO_CURSO_R/Curso 2/dados/INTOX_EXOG_PROFEPI_NOVO.dbf")

cnae <- import("F:/PROJETO_CURSO_R/Curso 2/dados/CNAENET.dbf")

cbo <- import("F:/PROJETO_CURSO_R/Curso 2/dados/OCUPANET.dbf")


# Indicador 13 ------------------------------------------------------------


# Proporção de preenchimento dos campos “Ocupação” e “Atividade Econômica (CNAE)”
# nas notificações de acidente de trabalho,
# acidente de trabalho com exposição a material biológico e
# intoxicação exógena relacionada ao trabalho segundo município de notificação.


# Identificando intoxicações relacionadas ao trabalho ---------------------

# São consideradas para o indicador as notificações  de 
# intoxicação que o campo 56 DOENCA_TRA está marcado como "1", Sim.


intox_exogena_trabalho <- intox_exogena |>
                          filter(DOENCA_TRA == "1")


# Juntando as bases de dados para o cálculo do indicador ------------------

# Para junção das bases de dados é importante que as variáveis sejam as mesmas
# Selecionando as variáveis de interesse para o cálculo do indicador
# Para isso vamos criar objetos com as mesmas variáveis


acmatbio_indicador <- ac_mat_biologico |> 
                      select(ID_AGRAVO, DT_NOTIFIC,
                             SG_UF_NOT, ID_MUNICIP, ID_REGIONA, 
                             ID_MN_RESI, ID_RG_RESI,
                             SIT_TRAB, ID_OCUPA_N, CNAE)

actrab_indicador <- ac_trabalho |> 
                    select(ID_AGRAVO, DT_NOTIFIC,
                           SG_UF_NOT, ID_MUNICIP, ID_REGIONA,
                           ID_MN_RESI, ID_RG_RESI,
                           SIT_TRAB, ID_OCUPA_N, CNAE)

intoxexogen_indicador <- intox_exogena_trabalho |>
                         select(ID_AGRAVO, DT_NOTIFIC,
                                SG_UF_NOT, ID_MUNICIP, ID_REGIONA,
                                ID_MN_RESI, ID_RG_RESI,
                                SIT_TRAB, ID_OCUPA_N, CNAE)

# Unindo as bases de dados

indicador_13 <- bind_rows(acmatbio_indicador,
                          actrab_indicador,
                          intoxexogen_indicador)

# Atribuir aos códigos CBO e CNAE os respectivos nomes --------------------
# Para essa operação vamos realizar a união de tabelas

# Incorporando os nomes das ocupações (CBO)

indicador_13 <- left_join(indicador_13, cbo,
                          by = "ID_OCUPA_N")
                
# Incorporando os nomes das atividades econômicas (CNAE)

indicador_13 <- left_join(indicador_13, cnae,
                          by = c("CNAE" = "CLASS_CNAE"))

# Corrigindo os caracteres codificados

library(stringi) # pacote para manipulação de textos (strings)

# Converter a coluna NM_OCUPACA da codificação CP850 para UTF-8
indicador_13$NM_OCUPACA <- stri_encode(indicador_13$NM_OCUPACA,
                                       from = "CP850", to = "UTF-8")

# Cálculo do indicador ----------------------------------------------------

# Precisamos lembrar que o indicador se refere ao município de notificação
# Neste caso só queremos os notificados do estado avaliado, o PR
# Vamos filtrar a base de dados, para garantir que a condição esteja atendida
# Vamos aplicar filter() na variável SG_UF_NOT

indicador13_pr <- indicador_13 |> 
                  filter(SG_UF_NOT == "41")

# Se você está em um município pode filtrar pela coluna ID_MUNICIP
# e utilizar o código do seu município

# Exemplo:
# indicador13_curitiba <- indicador_13 |> filter(ID_MUNICIP == "410690")

# Forma de cálculo
# Passo 1

# Calcular a proporção de preenchimento do campo “Ocupação”:
# Numerador: Número de notificações dos agravos com o campo “Ocupação”
# preenchido de acordo com os códigos da (CBO) correspondente
# Denominador: Número total de casos de agravos notificados,
# em determinado ano e município de notificação.

# IMPORTANTE, os seguintes CBOs não devem ser incluídos no numerador

# XXX - Não informado",
# "000000 - CBO sem definição" e
# "998999 - Ignorado"

# Verificar os CBOs informados na nossa base de dados

cbo_informados <- indicador13_pr |> 
                  group_by(ID_OCUPA_N) |>
                  summarise(NOTIFICACOES = n())
cbo_informados

# Excluindo os CBO inválidos diretamente na base de dados
# Excluindo os campos vazios NA

indicador13_cbo_validos <- indicador13_pr |>
                           filter(!ID_OCUPA_N %in% c("XXX",
                                                    "000000",
                                                    "998999")) |>
                           filter(!is.na(ID_OCUPA_N)) # não vazios

# Armazenando os valores em objetos 

numerador_indicador13a <- nrow(indicador13_cbo_validos)
numerador_indicador13a

denominador_indicador13 <- nrow(indicador13_pr)
denominador_indicador13

# Cálculo do Passo 1

indicador13a <- (numerador_indicador13a / denominador_indicador13)
indicador13a

# Forma de cálculo
# Passo 2

# Proporção de preenchimento do campo “Atividade Econômica”
# Numerador: Número de notificações dos agravos com o campo
# “Atividade Econômica” preenchido de acordo com os códigos da CNAE
# Denominador: Número total de casos de agravos notificados

indicador13_cnae_validos <- indicador13_pr |>
                            filter(!is.na(CNAE)) # não vazios

numerador_indicador13b <- nrow(indicador13_cnae_validos)
numerador_indicador13b

# Cálculo do Passo 2

indicador13b <- (numerador_indicador13b / denominador_indicador13)
indicador13b

# Cálculo do Passo 3

# Média dos resultados das proporções de preenchimento dos
# campos “ocupação” e “atividade econômica” obtidas para os agravos

indicador13_final <- ((indicador13a + indicador13b) / 2) * 100
indicador13_final

# Meta do indicador
# Para 2023: = 60% de preenchimento qualificado
# Para 2024: = 75% de preenchimento qualificado
# Para 2025: = 90% de preenchimento qualificado

# Fim da operação de cálculo do indicador 13 do PQA-VS


# Formatando um tabela para exportação ------------------------------------

# Vamos trabalhar com a informação do indicador para CBO e CNAE,
# distribuído pelas regionais de saúde. Assim podemos indicar para os gestores
# se temos áreas para ação da Vigilância em Saúde do Trabalhador
# para qualificar as notificações.

# Criando variáveis para avaliar as informações de CBO e CNAE

# Avaliando CBO

# XXX - Não informado",
# "000000 - CBO sem definição" e
# "998999 - Ignorado"

# Criar uma REGEX para os CBOs inválidos

cbo_invalido <- c("XXX","000000", "998999")
cbo_invalido <- str_c(cbo_invalido, collapse = "|")
cbo_invalido

# Criar variável para avaliar CBO

indicador13_pr <- indicador13_pr |>
                  mutate(cbo_adequado = if_else(
                    is.na(ID_OCUPA_N)| str_detect(ID_OCUPA_N, cbo_invalido),
                    "Inadequado",
                    "Adequado"))

# Avaliando a CNAE
                  
indicador13_pr <- indicador13_pr |>
                  mutate(cnae_adequado = if_else(
                    is.na(CNAE),
                    "Inadequado",
                    "Adequado"))

# Verificando os resultados encontrados anteriormente

resumo_cbo <- indicador13_pr |>
              group_by(cbo_adequado) |>
              summarise(notificacoes = n())

resumo_cnae <- indicador13_pr |>
               group_by(cnae_adequado) |>
               summarise(notificacoes = n())

# Resultados OK

# Agora vamos agrupar pelas regionais de saúde

# Passo 1: contar as notificações por regional de saúde

notificacoes_regionais <- indicador13_pr |>
                          group_by(ID_REGIONA) |>
                          summarise(notificacoes = n())

# Passo 2: contar as notificações adequadas de CBO

cbo_adequado_regionais <- indicador13_pr |>
                          filter(cbo_adequado == "Adequado") |> 
                          group_by(ID_REGIONA) |>
                          summarise(cbo_adequado = n())

# Passo 3: contar as notificações adequadas de CNAE

cnae_adequado_regionais <- indicador13_pr |>
                          filter(cnae_adequado == "Adequado") |> 
                          group_by(ID_REGIONA) |>
                          summarise(cnae_adequado = n())

# Passo 4: juntar as informações

ind13_regionais <- left_join(notificacoes_regionais,
                             cbo_adequado_regionais,
                             by = "ID_REGIONA")

ind13_regionais <- left_join(ind13_regionais,
                             cnae_adequado_regionais,
                             by = "ID_REGIONA")

# Passo 5: Para o cálculo precisamos corrigir os NA

ind13_regionais <- ind13_regionais |>
                   mutate(cnae_adequado = replace_na(cnae_adequado, 0))

# Passo 6: calcular o indicador para cada regional

ind13_regionais <- ind13_regionais |>
                   mutate(indicador13 = ((cbo_adequado/notificacoes) +
                                           (cnae_adequado/notificacoes))/2*100)

# Passo 7: Vamos nominar as regionais

# Para isso precisamos da tabela com o nome das regionais

regionais <- import("F:/PROJETO_CURSO_R/Curso 2/dados/REGIONET.dbf")

# Juntando as tabelas

ind13_regionais <- left_join(ind13_regionais,
                             regionais,
                             by = "ID_REGIONA")

# Passo 8: Selecionar as colunas de interesse e renomear as colunas

ind13_regionais <- ind13_regionais |>
                   select(NM_REGIONA, cbo_adequado, cnae_adequado,
                          notificacoes, indicador13) |>
                   rename("Regional de saúde" = "NM_REGIONA",
                          "CBO Adequado" = "cbo_adequado",
                          "CNAE Adequado" = "cnae_adequado",
                          "Notificações" = "notificacoes",
                          "Indicador 13" = "indicador13")
ind13_regionais

# Passo 9: Formatar uma tabela

library(gt) # Pacote para formatação de tabelas


indicador13_gt <- ind13_regionais |>
  gt() |> # atribui o formato gt
  tab_header( # atibui um título
    title = md("**Indicador 13, PQA-VS, segundo Regional de Saúde de Notificação, Paraná, Brasil, 2022**")
  ) |> 
  tab_style(                          # Estilo da tabela
    style = cell_text(weight = "bold"), # negrito
    locations = cells_column_labels(everything())
) |> 
  cols_align(       # Alinhamento das colunas
    align = "center", # centraliza colunas
    columns = c(2, 3, 4, 5) # define as colunas
  ) |>
  fmt_number(
    columns = c(5),  # Formata coluna específica
    sep_mark = ".",  # Separador de milhar
    dec_mark = ",",  # Separador decimal
    decimals = 1     # Dígitos
  ) |>
  tab_footnote(
    footnote = "Nota: Meta de 60% para CBO e CNAE corretamente preenchidos",
    locations = cells_title(groups = "title")) |> # Adiciona o indicador da nota
  tab_style(
    style = cell_text(weight = "bold"),  # Aplicar negrito
    locations = cells_body(
      columns = everything(),  # Todas as colunas
      rows = which(ind13_regionais[[5]] >= 60)  # Condição para aplicar o negrito
    )) |> 
  tab_options(
    table.font.size = "small"
    )

indicador13_gt


# Podemos salvar a tabela em um arquivo PDF (*.pdf)

gtsave(data = indicador13_gt,  # Tabela formatada
       filename = "tabela_formatada_ind13.pdf")  # Nome do arquivo PDF


###############################################################################
# Fim do script indicador 13
