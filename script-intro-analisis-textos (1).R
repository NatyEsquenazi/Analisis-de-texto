# RLadies La Paz
# Taller: Introducción al análisis de textos usando R | junio 2020
# Taller a cargo de Riva Quiroga (@rivaquiroga / rivaquiroga.cl)

# El enlace a este archivo es: https://bit.ly/codigo-rladies-lapaz
# Los materiales para e taller: https://bit.ly/materiales-rladies-lapaz

# EL LUNES 22 QUEDARÁ ACTUALIZADO EL SCRIPT CON COMENTARIOS EN EL CÓDIGO Y ENLACES
# A MATERIALES COMPLEMENTARIOS

### Paquetes que vamos a utilizar ----

# install.packages("tidyverse")
# install.packages("pdftools")
# install.packages("tidytext")
# install.packages("wordcloud2")
# install.packages("patchwork")

library(rvest)
library(pdftools)
library(readr)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(wordcloud2)
library(patchwork)

# Discurso 2019 -----

morales_html <- read_html("http://www.cancilleria.gob.bo/webmre/discurso/3103")

morales <- morales_html %>% 
  html_nodes(".col-lg-12") %>% 
  html_text()

morales_palabras <- tibble(texto = morales[[6]])

morales_palabras <- morales_palabras %>% 
  unnest_tokens(palabra, texto, strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE)

stopwords_es <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")

mis_stopwords <- tibble(palabra = c("bolivia", "bolivianos", "boliviano", "año", "años"))

morales_frecuencias <- morales_palabras %>%
  anti_join(stopwords_es) %>% 
  anti_join(mis_stopwords)

wordcloud2(data = morales_frecuencias)

grafico_morales <- morales_frecuencias %>% 
  top_n(15) %>% 
  ggplot(aes(y = reorder(palabra, n), x = n)) + 
  geom_col(fill = "#2A8EBA") +
  labs(y = NULL, 
       x = "frecuencia",
       title = "Palabras más frecuentes discurso 2019") +
  theme_minimal()

# Discurso año 2020 ------

pdf_info("anez_2020.pdf")

anez <- pdf_text("anez_2020.pdf")

anez <- paste(anez, collapse = " ")

write_lines(anez, "anez.txt")

# eliminar números de página y "(pausa y transición)"

anez <- str_remove_all(anez, "\\(pausa y transición\\)")

anez_palabras <- tibble(texto = anez)

anez_palabras <- anez_palabras %>% 
  unnest_tokens(palabra, texto, strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE)

anez_frecuencias <- anez_palabras %>% 
  anti_join(stopwords_es) %>% 
  anti_join(mis_stopwords)

wordcloud2(data = anez_frecuencias)

grafico_anez <- anez_frecuencias %>% 
  top_n(15) %>% 
  ggplot(aes(y = reorder(palabra, n), x = n)) + 
  geom_col(fill = "#FF4105") +
  labs(y = NULL, 
       x = "frecuencia",
       title = "Palabras más frecuentes discurso 2020") +
  theme_minimal()

# patchwork 

grafico_morales + grafico_anez

grafico_morales / grafico_anez

# Comparación entre ambos texots ----

morales_palabras <- morales_palabras %>% 
  mutate(discurso = "Evo Morales")

anez_palabras <-  anez_palabras %>% 
  mutate(discurso = "Jeanine Añez")

discursos <- bind_rows(morales_palabras, anez_palabras)

discursos <- select(discursos, discurso, palabra, n)

discursos <- discursos %>% 
  bind_tf_idf(palabra, discurso, n)

discursos %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(discurso) %>% 
  top_n(15) %>% 
  ggplot(aes(y = reorder(palabra, tf_idf), tf_idf, fill = discurso)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~discurso, scales = "free")
  

# topic modelling
# representación vectorial
# keyness = clavicidad "Lingüística de Corpus"




