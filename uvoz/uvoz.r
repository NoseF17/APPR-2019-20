# 2. faza: Uvoz podatkov
library(readr)
library(tidyr)
library(dplyr)
#library(eurostat)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

tabela1_delovne_ure_moski_zenske <- read_csv('podatki/tabela_moski_zenske_2008+.csv', na=':', skip=1, col_names = c('Leto','Drzava','Spol','Zaposlenost','izbriši1','izbriši2','izbriši3', 'SteviloDelovnihUr', 'izbriši4'), locale= locale(encoding = 'CP1250')) %>%
  select(-'izbriši1',-'izbriši2',-'izbriši3',-'izbriši4')
tabela2panoge <- read_csv('podatki/tabela_po_panogah_2008+.csv', na=':', skip=1, col_names = c('Leto','Drzava','Spol','Zaposlenost', 'Status', 'Panoga', 'Enota', 'SteviloDelovnihUr', 'izbriši1'), locale= locale(encoding = 'CP1250')) %>%
  select(-'Spol',-'Status',-'izbriši1',-'Enota')
tabela3gdp <- read_csv('podatki/tabela_GDP_per_capita_2008+.csv', na=':', skip=1, col_names = c('Leto', 'Drzava', 'Enota','NA_ITEM', 'Vrednost', 'izbriši1'), locale= locale(encoding = 'CP1250')) %>%
  select(-'izbriši1')
tabela4_delovne_ure_moski_zenske <- read_csv('podatki/tabela_moski_zenske_2008+.csv', na=':', skip=1, col_names = c('Leto','Drzava','Spol','Zaposlenost','izbriši1','izbriši2','izbriši3', 'SteviloDelovnihUr', 'izbriši4'), locale= locale(encoding = 'CP1250')) %>%
  select(-'izbriši1',-'izbriši2',-'izbriši3',-'izbriši4')
tabela5panoge <-read_csv('podatki/tabela_po_panogah_nova_2008+.csv', na=':', skip=1, col_names = c('Leto','Drzava','Spol','Zaposlenost', 'Status', 'Panoga', 'Enota', 'SteviloDelovnihUr', 'izbriši1'), locale= locale(encoding = 'CP1250')) %>%
  select(-'Status',-'izbriši1',-'Enota', -'Zaposlenost')


#Zanima me samo Totalni delovni čas - total
A1 <- tabela4_delovne_ure_moski_zenske %>% filter(Zaposlenost=="Total") %>% 
  select(Leto, Drzava, Spol, SteviloDelovnihUr)
A1$Drzava[A1$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'

#Slovenija v primerjavi z EU28 po letih
A2 <- A1 %>% filter(Drzava == "Slovenia", Spol == "Total") %>% 
  select(Leto, Drzava, Spol, SteviloDelovnihUr)
A3 <- A1 %>% filter(Drzava == "European Union - 28 countries", Spol == "Total") %>% 
  select(Leto, Drzava, Spol, SteviloDelovnihUr)


########## GDP per capita ##########
A4 <- tabela3gdp %>% filter(Enota =="Current prices, euro per capita", 
                            NA_ITEM == "Gross domestic product at market prices") %>% 
  select(Leto, Drzava, Vrednost)
A4$Drzava[A4$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'

########## PANOGE ##########
A5 <- tabela5panoge %>% filter(Spol == "Total") %>% 
  select(Leto, Drzava, -Spol, Panoga, SteviloDelovnihUr)
A5$Drzava[A5$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
A5$Drzava[A5$Drzava == 'European Union - 28 countries (2013-2020)'] <- 'European Union - 28 countries'

##############################################################################################
### PANOGE ZA DOLOČENO DRŽAVO ###
#1. EU
A6 <- A5 %>% filter(Drzava == "European Union - 28 countries")


#uvozi.BDP <- function() {
#  BDP_na_prebivalca <- read_csv("podatki/tabela_moski_zenske_2008+.csv", col_names=c("Drzava",2013:2018), skip=17,n_max=37, na=":",locale=locale(encoding="CP1250")) %>% drop_na()
#  #spisek_drzav_2 <- BDP_na_prebivalca %>% select(Drzava)
#  #spisek_drzav <- inner_join(spisek_drzav_1,spisek_drzav_2, by = c("Drzava"))
  #BDP <- BDP_na_prebivalca %>% filter(Drzava %in% c("spisek_drzav"))
#  BDP <- gather(BDP_na_prebivalca, -Drzava, key = "leto", value = "BDP per capita")
#  BDP$Drzava[BDP$Drzava == 'Germany (until 1990 former territory of the FRG)'] <- 'Germany'
#  return(BDP)
#}
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


#samomori.drzave <- A1 %>% group_by(Drzava, Leto) %>%
#  summarise(vsota=sum(SteviloDelovnihUr, na.rm = TRUE))

#samomori.vsi<- SAMOMORI %>% group_by(leto) %>% 
#summarise(vsota=sum(vrednost, na.rm=TRUE))

#samomori.drzave <- SAMOMORI %>% group_by(država, leto) %>% 
#summarise(vsota=sum(vrednost, na.rm = TRUE))

#  imena %>% filter(leto == 2013, spol == "zenske", stevilo <= 5) %>%
#  select(ime, stevilo)

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
