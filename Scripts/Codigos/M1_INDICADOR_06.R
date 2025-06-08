# Curso Visualização e Análise de Dados com Software R
# Aplicadas à Vigilância em Saúde
# Programa de Fortalecimento da Epidemiologia nos Serviços de Saúde - PROFEPI

# Módulo 1 - Construção indicador 06 do PQA-VS

# Carregamento dos pacotes necessários ------------------------------------

library(rio)        # Pacote para ler arquivos
library(tidyverse)  # Pacote para manipulação de dados
library(janitor)    # Pacote para manipulação de dados
library(lubridate)  # Pacote para operação com datas

# Definindo a pasta de trabalho

setwd("F:/PROJETO_CURSO_R/Curso 2")

# Carregamento das bases de dados para análise ----------------------------

dengue <- import("dados/DENGUE_PROFEPI.dbf")

chikungunya <- import("dados/CHIKUNGUNYA_PROFEPI.dbf")

nindinet <- import("dados/NINDINET_PROFEPI.dbf")


# Identificando registros de óbitos  --------------------------------------


dengue_obitos <- dengue |>
                 filter(EVOLUCAO == "2") # Filtra registros
                                         # com a condição
                                         # "óbito pelo agravo"

chikungunya_obitos <- chikungunya |>
                      filter(EVOLUCAO == "4") # Filtra registros
                                              # com a condição
                                              # "óbito em investigação"

zika_obitos <- nindinet |>
               filter(ID_AGRAVO == "A928", # Filtra o agravo Zika E
               EVOLUCAO == "4")            # com a condição
                                           # "óbito em investigação"


# Verificando o número de óbitos por agravo -------------------------------

# Número de óbitos por dengue
obitos_dengue <- nrow(dengue_obitos)
obitos_dengue

# Número de óbitos suspeitos por chikungunya
obitos_chikungunya <- nrow(chikungunya_obitos)
obitos_chikungunya

# Número de óbitos suspeitos por zika
obitos_zika <- nrow(zika_obitos)
obitos_zika


# Filtrando os demais agravos que compõem o indicador ---------------------

imediatas <- nindinet |>
             filter(ID_AGRAVO %in% c("A959", "A923", "A968",
                                     "J11", "B54", "A209",
                                     "A809", "B09", "A829"
                                     ))

# Verificando a quantidade de registros por agravo

imediatas |>
  group_by(ID_AGRAVO) |>
  count()

# Juntando as bases de dados para o cálculo do indicador ------------------

# Para junção das bases de dados é importante que as variáveis sejam as mesmas
# Selecionando as variáveis de interesse para o cálculo do indicador
# Para isso vamos criar objetos com as mesmas variáveis

dengue_indicador <- dengue_obitos |> 
                    select(ID_AGRAVO, ID_MN_RESI,
                           ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

chikungunya_indicador <- chikungunya_obitos |> 
                         select(ID_AGRAVO, ID_MN_RESI,
                                ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

zika_indicador <- chikungunya_obitos |> 
                         select(ID_AGRAVO, ID_MN_RESI,
                                ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

imediatas_indicador <- imediatas |> 
                       select(ID_AGRAVO, ID_MN_RESI,
                              ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

# Unindo as bases de dados

indicador_06 <- bind_rows(dengue_indicador,
                          chikungunya_indicador,
                          zika_indicador,
                          imediatas_indicador)






# Encontramos um erro na etapa de junção das tabelas
# O formato de dados não é igual nas bases de dados
# Vamos para a correção

# Primeiro vamos ver o formato de dados em cada uma das bases

glimpse(dengue_indicador)
glimpse(chikungunya_indicador)
glimpse(imediatas_indicador)
glimpse(zika_indicador)





# Só precisamos arrumar a base dengue_indicador
# Para isso vamos recorrer ao pacote {lubridate}

dengue_indicador <- dengue_indicador |> 
                    mutate(DT_NOTIFIC = lubridate::ymd(DT_NOTIFIC),
                           DT_ENCERRA = lubridate::ymd(DT_ENCERRA))

# Verificando se a correção deu certo

glimpse(dengue_indicador)

# Correção realizada, vamos retomar a junção de tabelas

indicador_06 <- bind_rows(dengue_indicador,
                          chikungunya_indicador,
                          zika_indicador,
                          imediatas_indicador)


# Cálculo do indicador ----------------------------------------------------

# Precisamos lembrar que o indicador se refere apenas aos residentes
# Neste caso só queremos os residente do estado avaliado, o PR
# Vamos filtrar a base de dados
# Para isso utilizaremos um conceito novo, "Expressões regulares"
# Vamos aplicar uma função de manipulação de texto str_detect()
# A Expressão regular ou regex "^41" refere-se às observações que iniciam
# com o texto "41", código do estado do PR.

indicador_pr <- indicador_06 |>
                filter(str_detect(ID_MN_RESI, "^41"))

# Cálculo do prazo de encerramento das notificações
# A função as.numeric(), converte o cálculo temporal em um número

indicador_pr <- indicador_pr |> 
                mutate(dias_encerramento = as.numeric(DT_ENCERRA - DT_NOTIFIC))
indicador_pr
# Criando uma variável que avalie se o encerramento foi oportuno
# Aqui vamos aplicar a regra de cálculo do indicador

indicador_pr <- indicador_pr |>
                mutate(oportunidade_encerramento = if_else(dias_encerramento <=60,
                                                           "Oportuno",
                                                           "Inoportuno"))
head(indicador_pr)
# A meta do indicador 06 do PQA-VS é:
# 80% de casos das doenças de notificação compulsória imediata registrados 
# no Sinan encerradas em até 60 dias, a partir da data de notificação.

# Contando as notificações por oportunidade de encerramento

resumo_indicador06 <- indicador_pr |> 
                      group_by(oportunidade_encerramento) |> 
                      summarise(notificacoes = n()) 

resumo_indicador06

# Método de cálculo do indicador:
# Numerador: Total de registros de DNCI, por município de residência,
# encerrados dentro de 60 dias a partir da data de notificação.
# Denominador: Total de registros de DNCI, por município de residência,
# notificados no período da avaliação.
# Fator de multiplicação: 100.


# Adicionando uma coluna dos percentuais

resumo_indicador06 <- resumo_indicador06 |>
                      mutate(proporcao = round(
                        (notificacoes/sum(notificacoes))*100, 1)) |>
                      arrange(desc(proporcao)) # Ordenando do maior para o menor
resumo_indicador06

# Adicionando a linha de total

resumo_indicador06 <- resumo_indicador06 |> 
                      janitor::adorn_totals("row", name = "Total")

resumo_indicador06

# Meta do indicador
# 80% de casos das doenças de notificação compulsória imediata
# registrados no Sinan encerradas em até 60 dias,
# a partir da data de notificação.


# Fim da operação de cálculo do indicador 06 do PQA-VS


# Formatando uma tabela para um relatório ---------------------------------

library(kableExtra) # pacote para manipulação de tabelas

# Ajustando o NA

indicador06_formatada <- resumo_indicador06 |>
    mutate(oportunidade_encerramento = replace_na(oportunidade_encerramento,
                                                  "Sem encerramento"))
                        

# Arrumando os nomes dos campos

indicador06_formatada <- indicador06_formatada |>
  rename("Oportunidade de encerramento" = oportunidade_encerramento,
         "Número de notificações" = notificacoes,
         "Proporção" = proporcao)

indicador06_formatada

# Aplicando um estilo com pacote {kableExtra}

indicador06_kable <- indicador06_formatada |>
                         kable() |> # Atribui formato kable
                         kable_styling() # Atribui o estilo
indicador06_kable

# Aplicando um estilo com pacote {gt}

library(gt) # Pacote para formatação de tabelas

indicador06_gt <- indicador06_formatada |>
                     gt() |> # atribui o formato gt
                     tab_header( # atibui um título
                     title = md("**Indicador 06, PQA-VS, Paraná, Brasil, 2022**")
                     ) |> 
                     tab_style(        # Estilo da tabela
                     style = cell_text(weight = "bold"), # negrito
                     locations = cells_column_labels(everything())) |> 
                     cols_align(       # Alinhamento das colunas
                     align = "center", # centraliza colunas
                     columns = c(2, 3) # define as colunas
                     ) |>
                     fmt_number(
                     columns = c(3),  # Formata coluna específica
                     sep_mark = ".",  # Separador de milhar
                     dec_mark = ",",  # Separador decimal
                     decimals = 1     # Dígitos
                    ) |>
                    tab_footnote(
                    footnote = "Nota: Meta de 80% de notificações encerradas oportunamente, até 60 dias",
                    locations = cells_title(groups = "title")  # Adiciona o indicador da nota
                    )
indicador06_gt

# Podemos salvar a tabela em um arquivo PDF (*.pdf)

gtsave(data = indicador06_gt,  # Tabela formatada
       filename = "tabela_formatada.pdf")  # Nome do arquivo PDF

