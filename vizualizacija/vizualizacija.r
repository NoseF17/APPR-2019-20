# 3. faza: Vizualizacija podatkov

#A3$SteviloDelovnihUr <- parse_double(A3$SteviloDelovnihUr)
#A2$SteviloDelovnihUr <- parse_double(A2$SteviloDelovnihUr)
#A1$SteviloDelovnihUr <- parse_double(A1$SteviloDelovnihUr)


g_slo <- ggplot(data = A2, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_line() + ggtitle("Število delovnih ur v Slovenijipo letih ") +
  theme(panel.background=element_rect(fill="grey"))

g_eu <- ggplot(data = A3, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_line() + ggtitle("Število delovnih ur v Evropi po letih") + 
  theme(panel.background=element_rect(fill="grey"))

g_drzave <- ggplot(data = A1, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_line() + ggtitle("Število delovnih ur po letih") + 
  theme(panel.background=element_rect(fill="grey"))


##########  SKUPNO IZRISAVANJE  ############
#1. Slo in EU ločeno po spolih, izrisano v točkah.
graf1 <- ggplot(data = A1 %>% filter(Drzava %in% c("Slovenia", "European Union - 28 countries")),
             aes(x=Leto, y=SteviloDelovnihUr, color = Spol, shape = Drzava)) + 
  geom_point() + ggtitle("Število delovnih ur po letih") + 
  theme(panel.background=element_rect(fill="grey"))

#2. Slo in EU total, izrisano s črto.
graf2 <- ggplot(data = A1 %>% filter(Drzava %in% c("Slovenia", "European Union - 28 countries"), Spol %in% "Total"),
               aes(x=Leto, y=SteviloDelovnihUr, color = Drzava)) + 
  geom_line() + ggtitle("Število delovnih ur po letih") + 
  theme(panel.background=element_rect(fill="grey"))

########## PRIMERJAVA BDP IN DELOVNIH UR ZA SLO ##########
colnames(tabela2gdp)[3] <- "BDP"
df <- left_join(A1 %>% filter(Drzava == "Slovenia" & Spol == "Total"), 
                tabela2gdp %>% filter(Drzava == "Slovenia"))[,-c(2,3)]
df1 <- gather(df, "tip", "vrednost", -Leto)

primerjalni_graf_slo <- df1 %>%
  ggplot(aes(x = Leto, y = vrednost, color = tip)) +
  geom_line() +
  facet_grid(tip ~ ., scales = "free_y")


########## PRIMERJAVA BDP IN DELOVNIH UR ZA EU ##########
colnames(tabela2gdp)[3] <- "BDP"
tab1 <- left_join(A1 %>% filter(Drzava == "European Union - 28 countries" & Spol == "Total"), 
                  tabela2gdp %>% filter(Drzava == "European Union - 28 countries"))[,-c(2,3)]
tab2 <- gather(tab1, "tip", "vrednost", -Leto)

primerjalni_graf_eu <- tab2 %>%
  ggplot(aes(x = Leto, y = vrednost, color = tip)) +
  geom_line() +
  facet_grid(tip ~ ., scales = "free_y")

########## PANOGE ########## primerju bom prve tri in zadne tri od SLO z EU, kot zanimivost se hrvaski/grski turizem z EU ali pa nemsko proizvodnjo z EU
#1. samo Slovenija
#panoge <- ggplot(SLOTOP5, aes(x=Panoga, y=SteviloDelovnihUr)) + 
#  geom_bar(stat='identity', position='dodge') + ggtitle("SLO panoge") + 
#  xlab("") + ylab("SteviloDelovnihUr") 

#2. SLO + EU
eu <- (A5 %>% filter(Drzava == "European Union - 28 countries", Leto == "2018"))[,-c(1,2)]
colnames(eu) <- c("Panoga", "EU")
skupna <- left_join(SLOTOP3, eu, by = "Panoga")
skupna1 <- left_join(SLOTOP3, eu, by = "Panoga")
colnames(skupna)[2] <- "Slovenija"
skupna <- gather(skupna, key = "drzava", value = "Ure", -Panoga)
skupna$Panoga[skupna$Panoga == 'Agriculture, forestry and fishing'] <- 'Kmetijstvo, gozdarstvo in ribolov'
skupna$Panoga[skupna$Panoga == 'Construction'] <- 'Gradbeništvo'
skupna$Panoga[skupna$Panoga == 'Mining and quarrying'] <- 'Rudarstvo in kamnoseštvo'
grafpanoge_top3 <- ggplot(skupna, aes(x=Panoga, y=Ure, fill=drzava)) + coord_cartesian(ylim = c(37.5, 42.5)) +
  geom_bar(stat='identity', position='dodge') + theme(axis.text.x = element_text(angle = 35, hjust = 1))

#eu2 <- (A5 %>% filter(Drzava == "European Union - 28 countries", Leto == "2018"))[,-c(1,2)]
#colnames(eu2) <- c("Panoga", "EU")
skupna2 <- left_join(SLOLOW3, eu, by = "Panoga")
colnames(skupna2)[2] <- "Slovenija"
skupna2 <- gather(skupna2, key = "drzava", value = "Ure", -Panoga)
skupna2$Panoga[skupna2$Panoga == 'Arts, entertainment and recreation'] <- 'Umetnost, zabava in rekreacija'
skupna2$Panoga[skupna2$Panoga == 'Accommodation and food service activities'] <- 'Nastanitvene in prehranske storitve'
skupna2$Panoga[skupna2$Panoga == 'Administrative and support service activities'] <- 'Administrativne in podporne storitvene dejavnosti'
grafpanoge_low3 <- ggplot(skupna2, aes(x=Panoga, y=Ure, fill=drzava)) + coord_cartesian(ylim = c(30, 40)) +
  geom_bar(stat='identity', position='dodge') + theme(axis.text.x = element_text(angle = 35, hjust = 1))

###################### ZEMLJEVIDI #####################
data("World")
#svet<- tm_shape(World) +tm_polygons(border.col = "black")
#evropskedrzave <- World%>%filter(continent == "Europe")
#names(evropskedrzave)
#z1 <- tm_shape(evropskedrzave) + tm_polygons(border.col = "black") + tm_legend(show=TRUE)

#1. BDP 2018
zemljevid_evrope_BDP <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  BDP <- tabela2gdp
  BDP <- BDP %>% filter (Leto == 2018) %>% select('Drzava', 'BDP')
  podatki <- merge(y = BDP,x = evropa, by.x='name', by.y = 'Drzava')
  evropa <- tm_shape(podatki) + tm_polygons('BDP')
  tmap_mode('view')
  return(evropa)
}

#2. Delovne ure 2018
zemljevid_evrope_delovne_ure_2018 <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  DelovneUre <- A1
  DeloneUre <- DelovneUre %>% filter (Leto == 2018, Spol == "Total") %>% select('Drzava', 'SteviloDelovnihUr')
  podatki <- merge(y = DelovneUre,x = evropa, by.x='name', by.y = 'Drzava')
  evropa <- tm_shape(podatki) + tm_polygons('SteviloDelovnihUr')
  tmap_mode('view')
  return(evropa)
}

zemljevid_evrope_delovne_ure_2009 <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  DelovneUre <- A1
  DeloneUre <- DelovneUre %>% filter (Leto == 2009, Spol == "Total") %>% select('Drzava', 'SteviloDelovnihUr')
  podatki <- merge(y = DelovneUre,x = evropa, by.x='name', by.y = 'Drzava')
  evropa <- tm_shape(podatki) + tm_polygons('SteviloDelovnihUr')
  tmap_mode('view')
  return(evropa)
}



#library(tmaptools)
#evropa1 <- World %>% filter (continent == 'Europe')
#names(evropa1)[3] <- 'Drzava'
#delure <- A1 %>% filter(Leto == 2018, Spol == "Total") %>% 
#    select(Drzava, SteviloDelovnihUr)
#bdp <- A4 %>% filter(Leto == 2018) %>%select(Drzava, BDP)
#glavni1 <- inner_join(evropa1, delure, by = 'Drzava')
#glavni2 <- inner_join(glavni1, bdp, by = 'Drzava')
#podatki_cluster <- glavni2 %>% select('Drzava', 'SteviloDelovnihUr','BDP', 'pop_est','well_being')
#st <- podatki_cluster$geometry
#podatki_cluster$geometry <- NULL
#podatki.norm <- podatki_cluster %>% select(-Drzava) %>% scale()
#rownames(podatki.norm) <- podatki_cluster$Drzava
#k <- kmeans(podatki.norm, 5, nstart=1000)
#skupine <- data.frame(Drzava=podatki_cluster$Drzava, skupina=factor(k$cluster))
#podatki_cluster <- cbind(podatki_cluster, st) 
#t <- merge(podatki_cluster, skupine, by="Drzava")
#t <- t[,c(7,6)]
#zemljevid <- tm_shape(t %>% set_projection("latlong"),
#                      xlim=c(-25, 35), ylim=c(32, 72)) + tm_polygons("skupina")

# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                             pot.zemljevida="OB", encoding="Windows-1250")
#levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
#zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
#zemljevid <- fortify(zemljevid)

# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
