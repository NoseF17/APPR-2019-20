# 3. faza: Vizualizacija podatkov

#A3$SteviloDelovnihUr <- parse_double(A3$SteviloDelovnihUr)
#A2$SteviloDelovnihUr <- parse_double(A2$SteviloDelovnihUr)
#A1$SteviloDelovnihUr <- parse_double(A1$SteviloDelovnihUr)


g1slo <- ggplot(data = A2, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_line() + ggtitle("Število delovnih ur po letih") +
  theme(panel.background=element_rect(fill="grey"))

g2eu <- ggplot(data = A3, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_line() + ggtitle("Število delovnih ur po letih") + 
  theme(panel.background=element_rect(fill="grey"))

g3drzave <- ggplot(data = A1, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_line() + ggtitle("Število delovnih ur po letih") + 
  theme(panel.background=element_rect(fill="grey"))


##########  SKUPNO IZRISAVANJE  ############
#1. Slo in EU ločeno po spolih, izrisano v točkah.
graf <- ggplot(data = A1 %>% filter(Drzava %in% c("Slovenia", "European Union - 28 countries")),
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


###################### ZEMLJEVIDI #####################
data("World")
svet<- tm_shape(World) +tm_polygons(border.col = "black")

evropskedrzave <- World%>%filter(continent == "Europe")
names(evropskedrzave)
z1 <- tm_shape(evropskedrzave) + tm_polygons(border.col = "black") + tm_legend(show=TRUE)


cluster_evropa <- function(){
  evropa1 <- World %>% filter (continent == 'Europe')
  zadovoljstvo <- uvozi.rating()
  brezposelnost <- uvozi.zaposlenost()
  bdp <- uvozi.BDP() %>% filter(leto == 2018) %>% select('Drzava', 'BDP per capita')
  evropa2 <- inner_join(x = evropa1, y = zadovoljstvo, by = c('sovereignt'='Drzava'))
  evropa3 <- inner_join(x = evropa2, y = brezposelnost, by = c('sovereignt'='name'))
  evropa4 <- inner_join(x = evropa3, y = bdp, by = c('sovereignt'='Drzava'))
  podatki_cluster <- evropa4 %>% filter (leto == 2018) %>% select('sovereignt', 'pop_est','BDP per capita', 'Ocena','Brezposelnost')
  podatki_cluster2 <- podatki_cluster
  podatki_cluster$geometry = NULL
  cluster <- podatki_cluster %>% select(-sovereignt) %>% scale()
  rownames(cluster) <- podatki_cluster$sovereignt
  k <- kmeans(cluster, 5, nstart=1000)
  skupine <- data.frame(sovereignt=podatki_cluster2$sovereignt, skupina=factor(k$cluster))
  podatki_za_risat <- merge(podatki_cluster2, skupine, by ='sovereignt')
  zemljevid <- tm_shape(podatki_za_risat) + tm_polygons('skupina')
  zemljevid
  tmap_mode('view')
  return(zemljevid)
}

zemljevid_evrope_BDP <- function(){
  evropa <- World %>% filter (continent == 'Europe')
  BDP <- A4
  BDP <- BDP %>% filter (Leto == 2018) %>% select('Drzava', 'BDP')
  podatki <- merge(y = BDP,x = evropa, by.x='name', by.y = 'Drzava')
  evropa <- tm_shape(podatki) + tm_polygons('BDP')
  tmap_mode('view')
  return(evropa)
}
#zemljevid_evrope_BDP()


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
