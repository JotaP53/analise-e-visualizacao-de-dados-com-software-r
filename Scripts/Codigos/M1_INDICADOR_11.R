# Curso Visualização e Análise de Dados com Software R
# Aplicadas  Vigilância em Saúde
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI

# Módulo 1 - Construção indicador 11 do PQA-VS

# Carregamento dos pacotes necessários ------------------------------------

library(rio)        # Pacote para ler arquivos
library(tidyverse)  # Pacote para manipulacao de dados
library(janitor)    # Pacote para manipulacao de dados
library(lubridate)  # Pacote para operacao com datas

# Carregar outros  pacotes necessários
library(dplyr) #manipular seus dados de forma eficiente
library(knitr) #formata automaticamente seus resultados
library(kableExtra) #personalizar e formatar suas tabelas de forma elegante
library(gt) #tabelas interativas, que podem ser facilmente compartilhadas e exploradas

# Definindo a pasta de trabalho
setwd("/Users/elisangelalizzi/Documents/OPAS- Ministério da Saúde/Curso - R para vigilância em Saúde - Nível 02/Produção de conteúdo/Dados")


# Carregamento das bases de dados para analise ----------------------------
nindinet <- import("NINDINET_PROFEPI.dbf")
names(nindinet) ### mostra os nomes das colunas
attach(nindinet) ## disponibiliza as colunas para manipulação
head(nindinet)



###############################################################
### Indicador 11


##IM = (A/B) X 100
##A = Número de casos de sífilis congênita em menores de um ano em
##determinado ano, segundo município de residência
##B = Número de casos de sífilis em gestantes em determinado ano,
##segundo município de residência

### CID A509: Sífilis congênita não especificada
### CID O981: Sifilis em gestante


############################################################
#### Código usando informações padrão do R


# Filtrar os casos de sifilis congenita crianças e em gestantes (ID_Agravo correspondente) e idade menor que 1 ano
dados_filtradosn <- subset(nindinet, ID_AGRAVO == "A509" & !grepl("^4", NU_IDADE_N))  # Filtrando os casos com idade menor que 1 ano

dados_filtradosd <- subset(nindinet, ID_AGRAVO == "O981")  # Filtrando para o denominador, sifilis em gestantes

# Contar total de notificações para o numerador por município
numerador <- aggregate(ID_AGRAVO ~ ID_MUNICIP, data = dados_filtradosn, FUN = length)
names(numerador) <- c("ID_MUNICIP", "Total_Valido")  # Renomear colunas

# Contar total de notificações para o denominador por município
denominador <- aggregate(ID_AGRAVO ~ ID_MUNICIP, data = dados_filtradosd, FUN = length)
names(denominador) <- c("ID_MUNICIP", "Total_Notificacoes")  # Renomear colunas

# Juntar os dados
resultado <- merge(numerador, denominador, by = "ID_MUNICIP", all = TRUE)
head(resultado)

# Substituir valores NA por 0 (caso haja municípios sem notificações válidas ou totais)
resultado[is.na(resultado)] <- 0

# Calcular o indicador por município
resultado$Indicador <- (resultado$Total_Valido / resultado$Total_Notificacoes) * 100

# Calcular o total geral
total_geral <- data.frame(
  ID_MUNICIP = "Total Geral",
  Total_Valido = sum(resultado$Total_Valido, na.rm = TRUE),
  Total_Notificacoes = sum(resultado$Total_Notificacoes, na.rm = TRUE)
)
total_geral$Indicador <- (total_geral$Total_Valido / total_geral$Total_Notificacoes) * 100

# Juntar o total geral ao resultado
resultado_final <- rbind(resultado, total_geral)

# Exibir o resultado final
print(resultado_final)

################################################################################
# Gerar tabela formatada em HTML (usando pacotes extras para visualização)
library(knitr) #formata automaticamente seus resultados
library(kableExtra) #personalizar e formatar suas tabelas de forma elegante


tabela_indicador <- kable(resultado_final, format = "html",
                          col.names = c("Município", "Total Notificações-Sífilis Crianças < 1 ano", "Total de Notificações-Sífilis Gestantes", "Indicador 11 PQAVS (%)"),
                          digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Informações por Município-Ano 2022" = 3))

# Exibir a tabela HTML
tabela_indicador



################################################################################
############################################################
### Usando pacote dplyr para manipulação de dados

## Filtrar os casos de sifilis congenita crianças e em gestantes (ID_Agravo correspondente) e idade menor que 1 ano
dados_filtradosn <- nindinet %>%
  filter(ID_AGRAVO == "A509", !grepl("^4", NU_IDADE_N))  # Filtrando os casos com idade menor que 1 ano

dados_filtradosd <- nindinet %>%
  filter(ID_AGRAVO == "O981")  # Filtrando para o denominador sifilis em gestante

# Contar total de notificações para o numerador por município
numerador <- dados_filtradosn %>%
  group_by(ID_MUNICIP) %>%
  summarise(Total_Valido = n())

# Contar total de notificações para o denominador por município
denominador <- dados_filtradosd %>%
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
tabela_indicador <- kable(tabela_final, format = "html", 
                          col.names = c("Município", "Total Notificações-Sífilis Crianças < 1 ano", "Total de Notificações-Sífilis Gestantes", "Indicador 11 PQAVS (%)"), 
                          digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Soma por Município" = 3))

# Exibir tabela HTML
tabela_indicador



# Aplicar o estilo de tabela com pacote {gt}
tabela_id11 <- tabela_final %>%
  gt() %>%
  tab_header(
    title = md("**Indicador de Notificações de Sífilis, Paraná, Brasil, 2022**")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  cols_label( #### atribuindo rótulos para as colunas
    ID_MUNICIP	 = "Município",
    Total_Valido = "Total Notificações-Sífilis Crianças < 1 ano",
    Total_Notificacoes = "Total de Notificações-Sífilis Gestantes",
    Indicador = "Indicador 11 PQAVS (%)"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador")
  ) %>%
  fmt_number(
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador"),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%
  tab_footnote(
    footnote = "Nota: Indicador calculado conforme manual técnico PQAVS",
    locations = cells_title(groups = "title")
  )

# Exibir tabela formatada
tabela_id11


# Exibir tabela formatada
tabela_id11

# Salvar a tabela em um arquivo PDF (*.pdf)
gtsave(data = tabela_id11, filename = "tabela_formatada_id11.pdf")



