# 4. faza: Analiza podatkov

########## RAZVRŠČANJE V SKUPINE ##########

razvrscanje <- function(){
  evropa1 <- World %>% filter (continent == 'Europe')
  names(evropa1)[3] <- 'Drzava'
  delure <- A1 %>% filter(Leto == 2018, Spol == "Total") %>% 
    select(Drzava, SteviloDelovnihUr)
  delure$Drzava[delure$Drzava == "Czech Rep."] <- "Czech Republic"
  bdp <- tabela2gdp %>% filter(Leto == 2018) %>% 
    select(Drzava, BDP)
  glavni1 <- inner_join(evropa1, delure, by = 'Drzava')
  glavni2 <- inner_join(glavni1, bdp, by = 'Drzava')
  class(glavni2) <- "data.frame"
  podatki_cluster <- glavni2 %>% select('Drzava', 'SteviloDelovnihUr','BDP', 'pop_est','well_being')
  podatki.norm <- podatki_cluster %>% select(-Drzava) %>% scale()
  rownames(podatki.norm) <- podatki_cluster$Drzava
  k <- kmeans(podatki.norm, 5, nstart=1000)
  skupine <- data.frame(Drzava=podatki_cluster$Drzava, skupina=factor(k$cluster))
  #slika <- tm_shape(merge(evropa1, skupine, by="Drzava"), all.x=TRUE) + tm_polygons("skupina")
  slika <- tm_shape(merge(evropa1, skupine, by="Drzava") %>% set_projection("latlong"),
                    xlim=c(-25, 35), ylim=c(32, 72), all.x=TRUE) + tm_polygons("skupina")
  return(slika)
}
razvrscanje <- razvrscanje()

########## NAPOVEDOVANJE ##########
library(ggplot2)
library(GGally)
library(mgcv)

a_delovne_ure <- A2 %>% select(Leto, SteviloDelovnihUr)
prilagajanje <- lm(data = a_delovne_ure, SteviloDelovnihUr~I(Leto^2) +Leto + 0)
gg <- data.frame(Leto = seq(2009, 2018, 1))
napoved <- mutate(gg, NapovedanoSteviloDelovnihUr=predict(prilagajanje, gg))

graf_regresije <- ggplot(a_delovne_ure, aes(x=Leto, y=SteviloDelovnihUr))+
  geom_point() + geom_smooth(method = lm, formula =y~ x + I(x^2), fullrange = TRUE, color = 'green')+
  geom_point(data = napoved, aes(x= Leto, y=NapovedanoSteviloDelovnihUr), color='red', size = 2) +
  ggtitle('Napoved rasti stevila delovnih v Sloveniji')