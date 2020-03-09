# 3. faza: Vizualizacija podatkov

#A3$SteviloDelovnihUr <- parse_double(A3$SteviloDelovnihUr)
#A2$SteviloDelovnihUr <- parse_double(A2$SteviloDelovnihUr)
#A1$SteviloDelovnihUr <- parse_double(A1$SteviloDelovnihUr)


g_slo <- ggplot(data = A2, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_line() + ggtitle("Število delovnih ur po letih") +
  theme(panel.background=element_rect(fill="grey"))

g_eu <- ggplot(data = A3, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_line() + ggtitle("Število delovnih ur po letih") + 
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
colnames(A4)[3] <- "BDP"
df <- left_join(A1 %>% filter(Drzava == "Slovenia" & Spol == "Total"), 
                A4 %>% filter(Drzava == "Slovenia"))[,-c(2,3)]
df1 <- gather(df, "tip", "vrednost", -Leto)

grafek <- df1 %>%
  ggplot(aes(x = Leto, y = vrednost, color = tip)) +
  geom_line() +
  facet_grid(tip ~ ., scales = "free_y")


########## PRIMERJAVA BDP IN DELOVNIH UR ZA EU ##########
colnames(A4)[3] <- "BDP"
tab1 <- left_join(A1 %>% filter(Drzava == "European Union - 28 countries" & Spol == "Total"), 
                A4 %>% filter(Drzava == "European Union - 28 countries"))[,-c(2,3)]
tab2 <- gather(tab1, "tip", "vrednost", -Leto)

grafek2 <- tab2 %>%
  ggplot(aes(x = Leto, y = vrednost, color = tip)) +
  geom_line() +
  facet_grid(tip ~ ., scales = "free_y")

########## PANOGE ########## primerju bom prve tri in zadne tri od SLO z EU, kot zanimivost se hrvaski/grski turizem z EU ali pa nemsko proizvodnjo z EU
#1. samo Slovenija
panoge <- ggplot(SLOTOP5, aes(x=Panoga, y=SteviloDelovnihUr)) + 
  geom_bar(stat='identity', position='dodge') + ggtitle("SLO panoge") + 
  xlab("") + ylab("SteviloDelovnihUr") 

#2. SLO + EU
eu <- (A5 %>% filter(Drzava == "European Union - 28 countries", Leto == "2018"))[,-c(1,2)]
colnames(eu) <- c("Panoga", "EU")
skupna <- left_join(SLOTOP5, eu, by = "Panoga")
skupna1 <- left_join(SLOTOP5, eu, by = "Panoga")
colnames(skupna)[2] <- "Slovenija"
skupna <- gather(skupna, key = "drzava", value = "ure", -Panoga)
grafpanoge <- ggplot(skupna, aes(x=Panoga, y=ure, fill=drzava)) +
  geom_bar(stat='identity', position='dodge') #+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

#rojstva_smrti <- ggplot(tabela_rojeni_umrli, aes(x=stanje, y=stevilo, fill=spol)) + 
#  geom_bar(stat="identity") + facet_grid(~regija) +ggtitle("Rojstva in smrti") + 
#  xlab("") + ylab("število") +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))



###################### ZEMLJEVIDI #####################
#data("World")
#svet<- tm_shape(World) +tm_polygons(border.col = "black")

#evropskedrzave <- World%>%filter(continent == "Europe")
#names(evropskedrzave)
#z1 <- tm_shape(evropskedrzave) + tm_polygons(border.col = "black") + tm_legend(show=TRUE)

#1. BDP 2018
zemljevid_evrope_BDP <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  BDP <- A4
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
