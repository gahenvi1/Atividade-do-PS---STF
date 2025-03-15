library(pacman)
p_load(tidyverse, ggplot2, readxl, dplyr, lubridate, DT, plotly, scales)
Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

banco <- read_excel("~/Documents/Atividade do PS - STF/banco/4797cb48-0ba9-4e66-832f-0ec6e43f44dd.xlsx") 



### Análise 1

top10 <- banco %>%
  select(Classe, Processo, Relator, `Data autuação`, `Ramo do Direito`) %>%
  arrange(`Data autuação`) %>%
  filter(Classe == "HC") %>%
  head(10) %>%
  select(-Classe) %>%
  mutate(
    Processo = str_to_sentence(Processo) %>% str_remove("^Hc\\s+"),
    Relator = str_to_title(Relator),
    `Ramo do Direito` = str_to_title(`Ramo do Direito`),
    `Data autuação` = format(`Data autuação`, "%d de %B de %Y")
  )




### Análise 2

processos_antigos_por_ministro <- banco %>%
  select(Relator, `Data último andamento`) %>%
  filter(`Data último andamento` < (Sys.time() - days(180))) %>%
  group_by(Relator) %>%
  summarise(Quantidade = n()) %>%
  filter(Relator != "*NI*") %>%
  mutate(Frequencia = Quantidade / sum(Quantidade),
         Relator = str_to_title(Relator),)



ministros_atuais <- c(
  "Min. Luís Roberto Barroso", "Min. Edson Fachin", "Min. Gilmar Mendes",
  "Min. Cármen Lúcia", "Min. Dias Toffoli", "Min. Luiz Fux",
  "Min. Alexandre De Moraes", "Min. Nunes Marques", "Min. André Mendonça",
  "Min. Cristiano Zanin", "Min. Flávio Dino"
)

tbl_min_atuais<- banco %>%
  select(Relator, `Data último andamento`) %>%
  mutate(Relator = str_to_title(Relator)) %>%
  filter(`Data último andamento` < (Sys.time() - days(180)),
         Relator %in% ministros_atuais) %>%
  group_by(Relator) %>%
  summarise(Quantidade = n()) %>%
  filter(Relator != "*NI*") %>%
  mutate(Frequencia = Quantidade / sum(Quantidade)) %>%
  mutate(Frequencia = round(Frequencia, 2)) %>%
  arrange(-Quantidade) %>%
  datatable(rownames = FALSE, options = list(pageLength = nrow(.)))






### Análise 3
situacao_final_por_ministro <- banco %>%
  select(Relator, `Situação da decisão final`) %>%
  group_by(Relator, `Situação da decisão final`) %>%
  summarise(Quantidade = n()) %>%
  filter(Relator != "*NI*") %>%
  mutate(Frequencia = Quantidade / sum(Quantidade),
         Relator = str_to_title(Relator),)
