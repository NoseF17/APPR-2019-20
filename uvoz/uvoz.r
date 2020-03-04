# 2. faza: Uvoz podatkov
library(readr)
library(tidyr)
library(dplyr)
#library(eurostat)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")




#tabela111 <- read_csv('podatki/lfsa_ewhun2_1_Data.csv', locale= locale(encoding = 'CP1250'))

tabela1_delovne_ure_moski_zenske <- read_csv('podatki/tabela_moski_zenske_2008+.csv', na=' ', skip=1, col_names = c('Leto','Drzava','Spol','Zaposlenost','izbriši1','izbriši2','izbriši3', 'St. delovnih ur', 'izbriši4'), locale= locale(encoding = 'CP1250')) %>%
  select(-'izbriši1',-'izbriši2',-'izbriši3',-'izbriši4')
tabela2panoge <- read_csv('podatki/tabela_po_panogah_2008+.csv', na=' ', skip=1, col_names = c('Leto','Drzava','Spol','Zaposlenost', 'Status', 'Panoga', 'Enota', 'St. delovnih ur', 'izbriši1'), locale= locale(encoding = 'CP1250')) %>%
  select(-'Spol',-'Status',-'izbriši1',-'Enota')
tabela3gdp <- read_csv('podatki/tabela_GDP_per_capita_2008+.csv', na=' ', skip=1, col_names = c('Leto', 'Drzava', 'Enota','NA_ITEM', 'Vrednost', 'izbriši1'), locale= locale(encoding = 'CP1250')) %>%
  select(-'izbriši1')

tabela4_delovne_ure_moski_zenske <- read_csv('podatki/tabela_moski_zenske_2008+.csv', na=' ', skip=1, col_names = c('Leto','Drzava','Spol','Zaposlenost','izbriši1','izbriši2','izbriši3', 'SteviloDelovnihUr', 'izbriši4'), locale= locale(encoding = 'CP1250')) %>%
  select(-'izbriši1',-'izbriši2',-'izbriši3',-'izbriši4')


#Zanima me samo Totalni delovni čas - total
A1 <- tabela4_delovne_ure_moski_zenske %>% filter(Zaposlenost=="Total") %>% 
  select(Leto, Drzava, Spol, SteviloDelovnihUr)

#Slovenija v primerjavi z EU28 po letih
A2 <- A1 %>% filter(Drzava == "Slovenia", Spol == "Total") %>% 
  select(Leto, Drzava, Spol, SteviloDelovnihUr)
A3 <- A1 %>% filter(Drzava == "European Union - 28 countries", Spol == "Total") %>% 
  select(Leto, Drzava, Spol, SteviloDelovnihUr)

#samomori.drzave <- A1 %>% group_by(Drzava, Leto) %>%
#  summarise(vsota=sum(SteviloDelovnihUr, na.rm = TRUE))

#samomori.vsi<- SAMOMORI %>% group_by(leto) %>% 
#summarise(vsota=sum(vrednost, na.rm=TRUE))

#samomori.drzave <- SAMOMORI %>% group_by(država, leto) %>% 
#summarise(vsota=sum(vrednost, na.rm = TRUE))

#  imena %>% filter(leto == 2013, spol == "zenske", stevilo <= 5) %>%
#  select(ime, stevilo)
# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
    }
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}



# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="CP1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
  data <- data %>% gather(`1`:`4`, key="velikost.druzine", value="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- parse_factor(data$obcina, levels=obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
