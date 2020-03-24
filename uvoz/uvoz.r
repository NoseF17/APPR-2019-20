# 2. faza: Uvoz podatkov
library(readr)
library(tidyr)
library(dplyr)
#library(eurostat)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")
tabela2gdp_celotna <- read_csv('podatki/tabela_GDP_per_capita_2008+.csv', na=':', skip=1, col_names = c('Leto', 'Drzava', 'Enota','NA_ITEM', 'Vrednost', 'izbriši1'), locale= locale(encoding = 'CP1250')) %>%
  select(-'izbriši1')

tabela1_delovne_ure_moski_zenske <- read_csv('podatki/tabela_moski_zenske_2008+.csv', na=':', skip=1, col_names = c('Leto','Drzava','Spol','Zaposlenost','izbriši1','izbriši2','izbriši3', 'SteviloDelovnihUr', 'izbriši4'), locale= locale(encoding = 'CP1250')) %>%
  select(-'izbriši1',-'izbriši2',-'izbriši3',-'izbriši4')

tabela2gdp <- tabela2gdp_celotna %>% filter(Enota =="Current prices, euro per capita", 
                                            NA_ITEM == "Gross domestic product at market prices") %>% 
  select(Leto, Drzava, Vrednost)
tabela2gdp$Drzava[tabela2gdp$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
tabela2gdp$Drzava[tabela2gdp$Drzava == "Czechia"] <- "Czech Rep."

tabela3panoge <-read_csv('podatki/tabela_po_panogah_nova_2008+.csv', na=':', skip=1, col_names = c('Leto','Drzava','Spol','Zaposlenost', 'Status', 'Panoga', 'Enota', 'SteviloDelovnihUr', 'izbriši1'), locale= locale(encoding = 'CP1250')) %>%
  select(-'Status',-'izbriši1',-'Enota', -'Zaposlenost')

########################################################
#Zanima me samo Totalni delovni čas - total
A1 <- tabela1_delovne_ure_moski_zenske %>% filter(Zaposlenost=="Total") %>% 
  select(-Zaposlenost)
A1$Drzava[A1$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
A1$Drzava[A1$Drzava == "Czechia"] <- "Czech Rep."

#Slovenija v primerjavi z EU28 po letih
A2 <- A1 %>% filter(Drzava == "Slovenia", Spol == "Total") %>% 
  select(Leto, Drzava, Spol, SteviloDelovnihUr)
A3 <- A1 %>% filter(Drzava == "European Union - 28 countries", Spol == "Total") %>% 
  select(Leto, Drzava, Spol, SteviloDelovnihUr)

########## PANOGE ##########
A5 <- tabela3panoge %>% filter(Spol == "Total") %>% 
  select(-Spol)
A5$Drzava[A5$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
A5$Drzava[A5$Drzava == 'European Union - 28 countries (2013-2020)'] <- 'European Union - 28 countries'

##############################################################################################
### PANOGE ZA DOLOČENO DRŽAVO ###
#1. EU
EUTOP3 <- A5 %>% filter(Drzava == "European Union - 28 countries", Leto == "2018") %>%
  group_by(Panoga) %>% summarise(SteviloDelovnihUr) %>% arrange(desc(SteviloDelovnihUr)) %>%
  drop_na() %>% head(3)
EULOW3 <- A5 %>% filter(Drzava == "European Union - 28 countries", Leto == "2018") %>%
  group_by(Panoga) %>% summarise(SteviloDelovnihUr) %>% arrange(desc(SteviloDelovnihUr)) %>%
  drop_na() %>% tail(3)

#2. SLOVENIJA
SLOTOP3 <- A5 %>% filter(Drzava == "Slovenia", Leto == "2018") %>%
  group_by(Panoga) %>% summarise(SteviloDelovnihUr) %>% arrange(desc(SteviloDelovnihUr)) %>%
  drop_na() %>% head(3)
SLOLOW3 <- A5 %>% filter(Drzava == "Slovenia", Leto == "2018") %>%
  group_by(Panoga) %>% summarise(SteviloDelovnihUr) %>% arrange(desc(SteviloDelovnihUr)) %>%
  drop_na() %>% tail(4)
SLOLOW3 <- SLOLOW3 %>% head(3)
SLOTOP5 <- A5 %>% filter(Drzava == "Slovenia", Leto == "2018") %>%
  group_by(Panoga) %>% summarise(SteviloDelovnihUr) %>% arrange(desc(SteviloDelovnihUr)) %>%
  drop_na() %>% head(5)

#3. NEMČIJA
GERTOP5 <- A5 %>% filter(Drzava == "Germany", Leto == "2018") %>%
  group_by(Panoga) %>% summarise(SteviloDelovnihUr) %>% arrange(desc(SteviloDelovnihUr)) %>%
  drop_na() %>% head(5) 

#4. HRVAŠKA
CROTOP5 <- A5 %>% filter(Drzava == "Croatia", Leto == "2018") %>%
  group_by(Panoga) %>% summarise(SteviloDelovnihUr) %>% arrange(desc(SteviloDelovnihUr)) %>%
  drop_na() %>% head(5)

CROLOW5 <- A5 %>% filter(Drzava == "Croatia", Leto == "2018") %>%
  group_by(Panoga) %>% summarise(SteviloDelovnihUr) %>% arrange(desc(SteviloDelovnihUr))%>% 
  drop_na() %>% tail(5) 

#5. GRČIJA
GRETOP5 <- A5 %>% filter(Drzava == "Greece", Leto == "2018") %>%
  group_by(Panoga) %>% summarise(SteviloDelovnihUr) %>% arrange(desc(SteviloDelovnihUr)) %>%
  drop_na() %>% head(5)
