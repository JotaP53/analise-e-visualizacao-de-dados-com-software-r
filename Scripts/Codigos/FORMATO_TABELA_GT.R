# Curso Visualização e Análise de Dados com Software R
# Aplicadas à Vigilância em Saúde
# Programa de Fortalecimento da Epidemiologia nos Serviços de Saúde - PROFEPI

# Módulo 1 - Componentes de uma tabela {gt}


# Carregamento dos pacotes ------------------------------------------------

library(gt) # Criação e edição de tabelas
library(tidyverse) # Manipulação de dados
library(rio) # Importação de dados


# Carregamento da base de dados -----------------------------------------

dengue_sorotipos <- import("F:/PROJETO_CURSO_R/Curso 2/Módulo 1/sorotipos_denv_boletim.xlsx")

glimpse(dengue_sorotipos)


# Arrumando a base de dados para criação da tabela {gt} -------------------

tabela_dengue <- dengue_sorotipos |> 
                 pivot_wider(names_from = "Sorotipo",
                             values_from = "Amostras")

# Criando uma coluna "Total"

tabela_dengue <- tabela_dengue |> 
                 mutate(Total = `DENV-1` + `DENV-2` + `DENV-3` + `DENV-4`)

# Agrupando por regiões

dengue_regioes <- tabela_dengue |> 
                  group_by(Região) |> 
                  summarise(`DENV-1` = sum(`DENV-1`),
                            `DENV-2` = sum(`DENV-2`),
                            `DENV-3` = sum(`DENV-3`),
                            `DENV-4` = sum(`DENV-4`),
                            `Total` = sum(`Total`)) |>
                  mutate(UF = Região) |>
                  select(UF, everything())


tabela_dengue <- bind_rows(dengue_regioes,
                           tabela_dengue) |>
                 arrange(desc(Região))

# Criar uma coluna para estabelecer a mesma ordem do MS

tabela_dengue <- tabela_dengue |> 
                 mutate(ordem_tabela = case_when(Região == "Norte" ~ "1",
                                                 Região == "Nordeste" ~ "2",
                                                 Região == "Sudeste" ~ "3",
                                                 Região == "Sul" ~ "4",
                                                 Região == "Centro-Oeste" ~ "5"))

# Reordenar tabelas com as colunas de interesse

tabela_dengue <- tabela_dengue |>
                 arrange(ordem_tabela) |> 
                 select(-Região, -ordem_tabela) |>
                 rename("UF/Região" = "UF")

# Criar um total do Brasil

dengue_brasil <- dengue_regioes |> 
  summarise( "UF/Região" = "Brasil",
             `DENV-1` = sum(`DENV-1`),
             `DENV-2` = sum(`DENV-2`),
             `DENV-3` = sum(`DENV-3`),
             `DENV-4` = sum(`DENV-4`),
             `Total` = sum(`Total`))

# Juntando todas as tabelas

tabela_dengue <- bind_rows(tabela_dengue, dengue_brasil )


# Formatando a tabela com {gt} --------------------------------------------

# Passo 1: Atribuir um formato {gt}

dengue_gt <- tabela_dengue |> 
             gt()
dengue_gt

# Passo 2: Incluir o título

dengue_gt <- dengue_gt |> 
             gt::tab_header(title = md("**TABELA 4. Distribuição dos sorotipos de DENV por UF de residência – Brasil, SE 1 à SE 24 de 2024**"))

dengue_gt

# Passo 3: Agrupar as colunas 

dengue_gt <- dengue_gt |>
             gt::tab_spanner(label = "Número de amostras positivas",
                             columns = c(starts_with("DENV"), "Total"),
                             id = "rotulo_amostras") |>
             gt::tab_spanner(label = "Semanas epidemiológicas 1 a 26",
                             columns = everything(),
                             id = "rotulo_tabela")

dengue_gt

# Passo 4: Formatar os cabeçalhos e título das colunas

# Cabeçalhos

dengue_gt <- dengue_gt |>
             tab_style(style = cell_text(weight = "bold"), # negrito
             locations = cells_column_spanners())

# Rótulos das colunas

dengue_gt <- dengue_gt |>
             tab_style(style = cell_text(weight = "bold"), # negrito
             locations = cells_column_labels(everything()))

dengue_gt

# Passo 5: Alinhamento das colunas

dengue_gt <- dengue_gt |>
             cols_align(align = "center",
                        columns = c(starts_with("DENV"),
                                    "Total"))
dengue_gt

# Passo 6: Formatando os números

dengue_gt <- dengue_gt |>
             fmt_number(
             columns = c(starts_with("DENV"),
                         "Total"),  # Formata colunas específicas
             sep_mark = ".",
             dec_mark = ",",
             decimals = 0)
dengue_gt

# Passo 7: Aplicar negrito nas linhas das regiões e do Brasil

dengue_gt <- dengue_gt |>
             tab_style(
             style = cell_text(weight = "bold"),  # Aplicar negrito
             locations = cells_body(
                         columns = everything(),  # Todas as colunas
                         rows = tabela_dengue[[1]] %in% c("Norte",
                                                          "Nordeste",
                                                          "Sudeste",
                                                          "Sul",
                                                          "Centro-Oeste",
                                                          "Brasil")))
dengue_gt

# Passo 8: Aplicar as cores no corpo da tabela

dengue_gt <- dengue_gt |>
             tab_style(
             style = cell_fill(color = "#FFF6E6"),  # Cor de fundo rosa claro
             locations = cells_body(columns = everything()))

dengue_gt

# Passo 9: Aplicar as cores nas regiões e Brasil

# Regiões

dengue_gt <- dengue_gt |>
             tab_style(
             style = cell_fill(color = "#FFD6D6"),
             locations = cells_body(
                         columns = everything(),  # Todas as colunas
                         rows = tabela_dengue[[1]] %in% c("Norte",
                                                           "Nordeste",
                                                           "Sudeste",
                                                           "Sul",
                                                           "Centro-Oeste")))
dengue_gt

# Brasil

dengue_gt <- dengue_gt |>
  tab_style(
    style = cell_fill(color = "#FFD54F"),
    locations = cells_body(
      columns = everything(),  # Todas as colunas
      rows = tabela_dengue[[1]] %in% c("Brasil")))

dengue_gt

# Passo 10: Formatar as linhas e incluir a nota de rodapé

dengue_gt <- dengue_gt |>
             tab_style(
                       style = cell_borders(color = "white", sides = "bottom"),  
                       locations = cells_body(rows = everything()))
dengue_gt

dengue_gt <- dengue_gt |>
             tab_footnote(
                         footnote = "DENV 4 vacinal",
                         locations = cells_body(
                         columns = c(1), 
                         rows = tabela_dengue[[1]] == "Mato Grosso do Sul"))

dengue_gt

# Finalização: Gerar um formato mais compacto pelo número de linhas

dengue_gt <- dengue_gt |>
             tab_options(
             table.font.size = "small")
dengue_gt




