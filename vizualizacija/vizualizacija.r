# 3. faza: Vizualizacija podatkov

A3$SteviloDelovnihUr <- parse_double(A3$SteviloDelovnihUr)
A2$SteviloDelovnihUr <- parse_double(A2$SteviloDelovnihUr)
A1$SteviloDelovnihUr <- parse_double(A1$SteviloDelovnihUr)


g1slo <- ggplot(data = A2, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_line() + ggtitle("Število delovnih ur po letih") +
  theme(panel.background=element_rect(fill="grey"))

g2eu <- ggplot(data = A3, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_line() + ggtitle("Število delovnih ur po letih") + 
  theme(panel.background=element_rect(fill="grey"))

g3drzave <- ggplot(data = A1, aes(x=Leto, y=SteviloDelovnihUr)) + 
  geom_point() + ggtitle("Število delovnih ur po letih") + 
  theme(panel.background=element_rect(fill="grey"))

#gslo&eu <- merge(g1,g2) + geom_point() + ggtitle("Število delovnih ur po letih") + 
#  theme(panel.background=element_rect(fill="grey"))


#kopirano
g <- ggplot() + aes(x=Leto, y=SteviloDelovnihUr, color=tip) + facet_grid(tip ~ ., scales="free_y") +
  geom_line(data= A1 %>% filter(Drzava == "Slovenia")) +
  geom_line(data=A1 %>% filter(Drzava == "European Union - 28 countries")) + 
  scale_x_continuous(breaks=seq(2008, 2018, 2)) +
  scale_y_continuous(labels=comma_format(big.mark="")) +
  guides(color=FALSE) + ggtitle("Število prostih in zasedenih delovnih mest ")




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
