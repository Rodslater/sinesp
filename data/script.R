library(rvest)
library(dplyr)
library(readxl)
library(lubridate)
library(writexl)


site <- read_html("https://dados.mj.gov.br/dataset/sistema-nacional-de-estatisticas-de-seguranca-publica/resource/feeae05e-faba-406c-8a4a-512aec91a9d1")

link <- site |> html_nodes(xpath="//a[contains(text(), '.xlsx')]") |> html_attr("href")
destinos <- c("SINESP.xlsx")

Map(function(u, d) download.file(u, d, mode="wb"), link, destinos)


vitimas <- read_excel("SINESP.xlsx", sheet = "Vítimas")
ocorrencias <- read_excel("SINESP.xlsx", sheet = "Ocorrências")

file.remove("SINESP.xlsx")


meses <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", 
           "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")


######
vitimas <- vitimas |> 
  mutate(Mês = match(toupper(Mês), toupper(meses)),
         Mês = ymd(paste(Ano, Mês, "01"))) |> 
  select(-Ano, Região = UF) |> 
  arrange(Região, `Tipo Crime`, desc(Mês))


vitimas_nordeste <- vitimas |>
  filter(Região %in% c("Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", 
                       "Piauí", "Alagoas", "Sergipe", "Rio Grande do Norte")) |>
  group_by(`Tipo Crime`, Mês, `Sexo da Vítima`, Região = "Nordeste")|>
  summarise(Vítimas = sum(Vítimas), .groups = 'drop') |> 
  arrange(Região, `Tipo Crime`, desc(Mês), `Sexo da Vítima`)


vitimas_brasil <- vitimas |>
  group_by(`Tipo Crime`, Mês, `Sexo da Vítima`, Região = "Brasil")|>
  summarise(Vítimas = sum(Vítimas), .groups = 'drop') |> 
  arrange(Região, `Tipo Crime`, desc(Mês), `Sexo da Vítima`)

vitimas <- bind_rows(vitimas, vitimas_nordeste, vitimas_brasil)


vitimas <- vitimas |> 
  mutate(Mês = format(as.Date(Mês, "%y-%m-%d"), "%d/%m/%Y"))


######
ocorrencias <- ocorrencias |> 
  mutate(Mês = match(toupper(Mês), toupper(meses)),
         Mês = ymd(paste(Ano, Mês, "01"))) |> 
  select(-Ano, Região = UF) |> 
  arrange(Região, `Tipo Crime`, desc(Mês))


ocorrencias_nordeste <- ocorrencias |>
  filter(Região %in% c("Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", 
                       "Piauí", "Alagoas", "Sergipe", "Rio Grande do Norte")) |>
  group_by(`Tipo Crime`, Mês, Região = "Nordeste")|>
  summarise(Ocorrências = sum(Ocorrências), .groups = 'drop') |> 
  arrange(Região, `Tipo Crime`, desc(Mês))


ocorrencias_brasil <- ocorrencias |>
  group_by(`Tipo Crime`, Mês, Região = "Brasil")|>
  summarise(Ocorrências = sum(Ocorrências), .groups = 'drop') |> 
  arrange(Região, `Tipo Crime`, desc(Mês))

ocorrencias <- bind_rows(ocorrencias, ocorrencias_nordeste, ocorrencias_brasil)


ocorrencias <- ocorrencias |> 
  mutate(Mês = format(as.Date(Mês, "%y-%m-%d"), "%d/%m/%Y"))


write_xlsx(vitimas, "data/vitimas.xlsx")
write_xlsx(ocorrencias, "data/ocorrencias.xlsx")


#VDE2024
url = 'https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-2024_.xlsx/@@download/file'
download.file(url, destfile = 'VDE2024.xlsx', mode="wb")
VDE2024 <- read_excel('VDE2024.xlsx', col_types = c("text", "text", "text", 
                                                    "date", "text", "text", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "text", "text", 
                                                    "numeric"))
file.remove("VDE2024.xlsx")
saveRDS(VDE2024, 'data/VDE2024.rds')
