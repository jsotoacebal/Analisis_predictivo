library(tidyverse)
library(factoextra)
library(dplyr)
library(caret)
library("scales")
library("ggplot2")



ONP = read_csv("OnlineNewsPopularity-1.csv")
View(ONP)
glimpse(ONP)

summary(ONP)
str(ONP)
head(ONP)
tail(ONP)

#Filtro de variables que van a ser utilizadas
ONPL <- ONP[ , c(1, 3, 4, 8,10,11,12,13,14, 15, 16, 17, 18,19, 31, 32, 33, 34, 35, 36, 37, 38,39,61)]
View(ONPL)
ONPL_num<- ONP[ , c(3, 4, 8,10,11,12,13,14, 15, 16, 17, 18,19, 31, 32, 33, 34, 35, 36, 37, 38,39,61)]
str(ONP_num)

summary(ONPL)

#Chequeo repetidos
ONPL %>% group_by(url) %>% filter(n()>1)%>% summarize(n=n())
#No hay url repetidas


#Chequeo faltantes
map_dbl(ONPL, function(x) mean(is.na(x)) * 100) %>% 
  sort(decreasing=T) %>% 
  as.data.frame()
#No hay datos faltantes


ggplot(ONPL) +
  aes(x = num_imgs) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

boxplot(ONPL$num_hrefs,
        ylab = "hwy"
)

#Creación de nueva variable para clasificar día de la semana

ONPL <- ONPL %>%
  mutate(weekday = case_when(
    weekday_is_monday == 1 ~ "Lunes",
    weekday_is_tuesday == 1 ~ "Martes",
    weekday_is_wednesday == 1 ~ "Miercoles",
    weekday_is_thursday == 1 ~ "Jueves",
    weekday_is_friday == 1 ~ "Viernes",
    weekday_is_saturday == 1 ~ "Sabado",
    weekday_is_sunday == 1 ~ "Domingo"
  ))


#Organización en orden de dia de semana
ONPL$weekday = factor(ONPL$weekday,
                      levels = c("Lunes","Martes","Miercoles","Jueves","Viernes","Sabado","Domingo"))

#Creación de nueva variable para clasificar tipo de canal
ONPL <- ONPL %>%
  mutate(data_channel = case_when(
    data_channel_is_lifestyle == 1 ~ "Lifestyle",
    data_channel_is_entertainment == 1 ~ "Entertainment",
    data_channel_is_bus == 1 ~ "Business",
    data_channel_is_socmed == 1 ~ "Social Media",
    data_channel_is_tech == 1 ~ "Tech",
    data_channel_is_world == 1 ~ "World",
  ))


#Reemplazo NA por "Others"
ONPL$data_channel <- replace(ONPL$data_channel, is.na(ONPL$data_channel), "Others")
ONPL$data_channel = factor(ONPL$data_channel,
                           levels = c("Others","Tech","Entertainment","World","Business","Social Media","Lifestyle"))


ONPL_cor = ONPL %>% dplyr:: select(n_tokens_content,n_tokens_title,num_hrefs,num_imgs,num_videos, average_token_length, num_keywords,weekday_is_monday,weekday_is_tuesday,weekday_is_wednesday,is_weekend,shares)

GGally::ggcorr(
  ONPL_cor, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)


#Graficos

#Scatter plot between n of shares and tokens content
ggplot(ONPL, aes(x = n_tokens_content, y = shares)) +
  geom_point(alpha = 0.1, color = "#802218") +
  labs(x = "Cantidad de palabras", y = "Cantidad de veces compartido") +
  ggtitle("Relación entra cantidad de palabras y cantidad de veces compartido") +
  scale_x_continuous(breaks = seq(0, 8000, by = 500)) +
  scale_y_continuous(labels = comma) +
  theme_classic()

test = ONPL$n_tokens_content
quantile(test)


#Scatter plot cantidad de palabras titulo
ggplot(data=ONPL, aes(x=n_tokens_title, y= shares)) +
  geom_point(color = "#a41916")  +
  labs(x = "Cantidad de palabras en el título", y = "Cantidad de veces compartido") +
  ggtitle("Relación entre palabras en el título  y cantidad de veces compartido") +
  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  scale_y_continuous(labels = comma) +
  theme_classic()

test = ONPL$n_tokens_title
quantile(test)

#Scatter shares y keywords
ggplot(ONPL, aes(x = num_keywords, y = shares)) +
  geom_point(alpha = 0.1, color = "#b5262a") +
  labs(x = "Cantidad de palabras claves", y = "Cantidad de veces compartido") +
  ggtitle("Relación entre cantidad de palabras clave y cantidad de veces compartido") +
  scale_x_continuous(breaks = seq(0, 10, by = 2)) +
  scale_y_continuous(labels = comma) +
  theme_classic()

test = ONPL$num_keywords
quantile(test)


#Scatter plot cantidad de referencias y cantidad de shares

ggplot(data=ONPL, aes(x=num_hrefs, y= shares)) +
  geom_point(color = "#dd1f13")  +
  labs(x = "Cantidad de referencias", y = "Cantidad de veces compartido") +
  ggtitle("Relación entre referencias  y cantidad de veces compartido") +
  scale_x_continuous(breaks = seq(0, 300, by = 25)) +
  scale_y_continuous(labels = comma) +
  theme_classic()

ONPL %>% select(num_hrefs) %>% filter(num_imgs> 50)


test = ONPL$num_hrefs
quantile(test)

#Scatter plot de imagenes y compartidas 
ggplot(ONPL, aes(x = num_imgs, y = shares)) + 
  geom_point(alpha = 0.5, color = "#dd1f13") + 
  labs(x = "Cantidad de imagenes", y = "Cantidad de veces compartido", 
       title = "Relación entre cantidad de imagenes y cantidad de veces compartido") + 
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  scale_y_continuous(labels = comma) +
  theme_classic()


test = ONPL$num_imgs
quantile(test)


#Scatter plot between n of shares and videos
ggplot(ONPL, aes(x = num_videos, y = shares)) +
  geom_point(alpha = 0.1, color = "#b5262a") +
  labs(x = "Cantidad de videos", y = "Cantidad de veces compartido") +
  ggtitle("Relación entra cantidad de videos y cantidad de veces compartido") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(labels = comma) +
  theme_classic()

test = ONPL$num_videos
quantile(test)


#Graficos de barra

#Cantiad de veces compartido por canal

#Cuadro para ver numeros exactos
ONPL_sum_ch = ONPL %>% group_by(data_channel) %>% summarise(suma_dch = sum(shares))%>% arrange(desc(suma_dch)) 


ggplot(ONPL_sum_ch, aes(x = data_channel, suma_dch, fill = data_channel)) +
  geom_col(fill = "#e73927") + scale_fill_grey(start = 0.25, end = 0.75) +
  scale_y_continuous(labels = comma) +
  labs(title = "Cantidad de veces compartido por tema",x = "Tema", y = "Cantidad de veces compartido")

p_c = ONPL_sum_channel %>% mutate(porcentaje = suma_dc / sum(suma_dc)*100)


#Grafico cantiad de veces compartido por dia de semana

#Cuadro para ver numeros exactos
ONPL_sum_dia = ONPL %>% group_by(weekday) %>% summarise(suma_d = sum(shares))
ONPL_sum_dia

p = ONPL_sum_dia %>% mutate(porcentaje = suma_d / sum(suma_d)*100)

ggplot(ONPL_sum_dia, aes(x = weekday,suma_d, fill = weekday)) +
  geom_col(fill = "#e73927") + scale_fill_grey(start = 0.25, end = 0.75) +
  labs(title = "Cantidad de veces compartido por día de semana",x = "Día de la semana", y = "Cantidad de veces compartido")


#Cantidad de veces compartido por canal y por dia de semana

ONPL_sum_dia_channel = ONPL %>% group_by(weekday,data_channel) %>% summarise(suma_2 = sum(shares))

ggplot(ONPL_sum_dia_channel, aes(x = weekday, y = suma_2, fill = data_channel  )) +
  geom_bar(stat="identity",position=position_dodge()) +
  labs(title = "Cantidad de veces compartido por dia de semana y tema",x = "Dia de la semana", y = "Cantidad de veces compartido", fill = "Tema") + 
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ef7c24","#cc0033","#e73927","red1","#c03d3e","#802218","deeppink4"))

#Cantidad de veces compartido por canal y por dia de semana excluyendo "Others"
ONPL_sum_dia_channel_f = ONPL %>% group_by(weekday,data_channel) %>% summarise(suma_2 = sum(shares)) %>% filter(data_channel != "Others")

ggplot(ONPL_sum_dia_channel_f, aes(x = weekday, y = suma_2, fill = data_channel  )) +
  geom_bar(stat="identity",position=position_dodge()) +
  labs(title = "Cantidad de veces compartido por dia de semana y tema",x = "Dia de la semana", y = "Cantidad de veces compartido", fill = "Tema") + 
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ef7c24","#cc0033","#e73927","red1","#c03d3e","#802218","deeppink4"))



