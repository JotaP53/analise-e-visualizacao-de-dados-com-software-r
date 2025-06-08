# Curso Visualização e Análise de Dados com Software R
# Aplicadas  Vigilância em Sa?de
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI

# Módulo 1 - Construção indicador 14 do PQA-VS

# Carregamento dos pacotes necessários ------------------------------------

library(rio)        # Pacote para ler arquivos
library(tidyverse)  # Pacote para manipulacao de dados
library(janitor)    # Pacote para manipulacao de dados
library(lubridate)  # Pacote para operacao com datas

# Definindo a pasta de trabalho

setwd("/Users/elisangelalizzi/Documents/OPAS- Ministério da Saúde/Curso - R para vigilância em Saúde - Nível 02/Produção de conteúdo/Dados")


# Carregamento das bases de dados para analise ----------------------------


nindinet <- import("NINDINET_PROFEPI.dbf")
names(nindinet) ### mostra os nomes das colunas
attach(nindinet) ## disponibiliza as colunas para manipulação



### Indicador 14
##14.Meta: 95% de notificações de violência interpessoal e autoprovocada com o campo raça/cor
##preenchido com informação válida.
##Indicador: Proporção de notificações de violência interpessoal e autoprovocada com o campo
##raça/cor preenchido com informação válida.

##Numerador: Total de notificações de violência interpessoal e
##autoprovocada com o campo raça/cor preenchido com informação
##válida (categorias Branca, Preta, Amarela, Parda ou Indígena), por
##município de notificação.
##Denominador: Total de casos notificados por município de notificação.
##Fator de multiplicação: 100.
### CID Y09: Violência interpessoal/autoprovocada


############################################################
#### Código usando informações padrão do R [ Rbase]
# Filtrar os casos de violência interpessoal e autoprovocada (ID_Agravo correspondente)
dados_filtrados <- subset(nindinet, ID_AGRAVO == "Y09")
table(dados_filtrados$ID_AGRAVO)

# Filtrar as notificações com raça/cor válida (categorias 1, 2, 3, 4, 5)
dados_validos <- subset(dados_filtrados, CS_RACA %in% c(1, 2, 3, 4, 5))
table(dados_validos$ID_AGRAVO,dados_validos$CS_RACA) ##tabela de dupla entrada

# Contar total de notificações com raça/cor preenchido por município
numerador <- aggregate(CS_RACA ~ ID_MUNICIP, data = dados_validos, FUN = length)

# Contar total de notificações por município
denominador <- aggregate(ID_AGRAVO ~ ID_MUNICIP, data = dados_filtrados, FUN = length)

# Renomear colunas para facilitar o merge
names(numerador) <- c("ID_MUNICIP", "Total_Valido")
names(denominador) <- c("ID_MUNICIP", "Total_Notificacoes")

# Juntar os dados
resultado <- merge(numerador, denominador, by = "ID_MUNICIP")
resultado


# Calcular o indicador por município
resultado$Indicador <- (resultado$Total_Valido / resultado$Total_Notificacoes) * 100

# Exibir o resultado final
print(resultado) 

## tipo de dado do resultado
glimpse(resultado)



################################################
################ Parte 01 #####################
#### Código refeito usando pacote dplyr
# Carregar pacotes necessários
library(dplyr)
library(knitr)
library(kableExtra)



# Filtrar os casos de violência interpessoal e autoprovocada (ID_Agravo correspondente)
dados_filtrados <- nindinet %>%
  filter(ID_AGRAVO == "Y09")

# Filtrar as notificações com raça/cor válida (categorias 1, 2, 3, 4, 5)
dados_validos <- dados_filtrados %>%
  filter(CS_RACA %in% c(1, 2, 3, 4, 5))

# Contar total de notificações com raça/cor preenchido por município
numerador <- dados_validos %>%
  group_by(ID_MUNICIP) %>%
  summarise(Total_Valido = n())

# Contar total de notificações por município
denominador <- dados_filtrados %>%
  group_by(ID_MUNICIP) %>%
  summarise(Total_Notificacoes = n())

# Juntar os dados
resultado <- left_join(numerador, denominador, by = "ID_MUNICIP")

# Calcular o indicador por município
resultado <- resultado %>%
  mutate(Indicador = (Total_Valido / Total_Notificacoes) * 100)

# Adicionar uma linha para o total geral
resultado_total <- resultado %>%
  summarise(ID_MUNICIP = "Total Geral",
            Total_Valido = sum(Total_Valido, na.rm = TRUE),
            Total_Notificacoes = sum(Total_Notificacoes, na.rm = TRUE),
            Indicador = (Total_Valido / Total_Notificacoes) * 100)

# Juntar o total geral ao resultado
tabela_final <- bind_rows(resultado, resultado_total)

# Gerar tabela bonita em HTML
tabela_indicador11 <- kable(tabela_final, format = "html", 
                     col.names = c("Município", "Total Notificações Válidas", "Total de Notificações", "Indicador 14 PQAVS (%)"),
                     digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Soma por Município" = 3))

# Exibir tabela HTML
tabela_indicador11




############### Fazendo uma tabela em html para exportar #######
# Carregar pacotes necessários
library(dplyr)
library(gt)

####################################################################
# Dataframe final já tenha sido calculado e se chama 'tabela_final'
# Aplicar o estilo com pacote {gt}
# Aplicar o estilo com pacote {gt}
tabela_id14 <- tabela_final %>%
  gt() %>%  # Atribui o formato gt
  tab_header(  # Atribui um título
    title = md("**Indicador de Notificações de Violência, Paraná, Brasil, 2022**")
  ) %>% 
  tab_style(  # Estilo da tabela
    style = cell_text(weight = "bold"),  # Negrito nos rótulos das colunas
    locations = cells_column_labels(everything())
  ) %>% 
  cols_label(  # Define os rótulos das colunas
    ID_MUNICIP = "Município",
    Total_Valido = "Total Notificações Válidas",
    Total_Notificacoes = "Total de Notificações",
    Indicador = "Indicador 14 PQAVS (%)"
  ) %>%
  cols_align(  # Alinhamento das colunas
    align = "center",  # Centraliza colunas
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador")  # Define as colunas
  ) %>% 
  fmt_number(  # Formata colunas numéricas
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador"),  # Colunas a formatar
    sep_mark = ".",  # Separador de milhar
    dec_mark = ",",  # Separador decimal
    decimals = 1  # Número de dígitos decimais
  ) %>%
  tab_footnote(  # Adiciona uma nota de rodapé
    footnote = "Nota: Indicador calculado como percentual de notificações válidas com raça/cor preenchida.",
    locations = cells_title(groups = "title")  # Localização da nota no título
  )



# Exibir tabela formatada
tabela_id14








# Podemos salvar a tabela em um arquivo PDF (*.pdf)

gtsave(data = tabela_id14,  # Tabela formatada
       filename = "tabela_formatada.pdf")  # Nome do arquivo PDF

















################################################
######### Parte 02 ############################
## Código ajustado para que as informações sejam mostradas por tipo de raça/cor (CS_RACA). 
###O novo código  agrupa as notificações não apenas por município, 
###mas também por cada categoria de raça/cor.

#### Código usando pacote dplyr
# Carregar pacotes necessários
library(dplyr)
library(knitr)
library(kableExtra)

# Filtrar os casos de violência interpessoal e autoprovocada (ID_Agravo correspondente)
dados_filtrados <- nindinet %>%
  filter(ID_AGRAVO == "Y09")

# Filtrar as notificações com raça/cor válida (categorias 1, 2, 3, 4, 5)
dados_validos <- dados_filtrados %>%
  filter(CS_RACA %in% c(1, 2, 3, 4, 5))

# Contar total de notificações com raça/cor preenchido por município e raça/cor
numerador <- dados_validos %>%
  group_by(ID_MUNICIP, CS_RACA) %>%
  summarise(Total_Valido = n())

# Contar total de notificações por município
denominador <- dados_filtrados %>%
  group_by(ID_MUNICIP) %>%
  summarise(Total_Notificacoes = n())

# Juntar os dados
resultado <- left_join(numerador, denominador, by = "ID_MUNICIP")

# Calcular o indicador por município e por raça/cor
resultado <- resultado %>%
  mutate(Indicador = (Total_Valido / Total_Notificacoes) * 100)

# Adicionar uma linha para o total geral por raça/cor
resultado_total <- resultado %>%
  group_by(CS_RACA) %>%
  summarise(ID_MUNICIP = "Total Geral",
            Total_Valido = sum(Total_Valido, na.rm = TRUE),
            Total_Notificacoes = sum(Total_Notificacoes, na.rm = TRUE),
            Indicador = (Total_Valido / Total_Notificacoes) * 100)

# Juntar o total geral ao resultado
tabela_final <- bind_rows(resultado, resultado_total)

# Gerar tabela bonita em HTML
tabela_indicador_raca <- kable(tabela_final, format = "html", 
                               col.names = c("Município", "Raça/Cor", "Total Notificações Válidas", "Total de Notificações", "Indicador (%)"),
                               digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Soma por Município e Raça/Cor" = 4))

# Exibir tabela HTML
tabela_indicador_raca

############### Fazendo uma tabela em html para exportar #######
# Carregar pacotes necessários
library(dplyr)
library(gt)

# Aplicar o estilo com pacote {gt}
tabela_id14_raca <- tabela_final %>%
  gt() %>%
  tab_header(
    title = md("**Indicador de Notificações de Violência por Raça/Cor, Paraná, Brasil, 2022**")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  cols_align(
    align = "center",
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador")
  ) %>%
  cols_label(  # Define os rótulos das colunas
    ID_MUNICIP = "Município",
    Total_Valido = "Total Notificações Válidas",
    Total_Notificacoes = "Total de Notificações",
    Indicador = "Indicador 14 PQAVS (%)"
  ) %>%
  fmt_number(
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador"),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%
  tab_footnote(
    footnote = "Nota: Indicador calculado como percentual de notificações válidas com raça/cor preenchida.",
    locations = cells_title(groups = "title")
  )

# Exibir tabela formatada
tabela_id14_raca

# Salvar a tabela em um arquivo PDF (*.pdf)
gtsave(data = tabela_id14_raca, filename = "tabela_formatada_id11_raca.pdf")












