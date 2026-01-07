Nº_bovinos <- eurostat::get_eurostat("apro_mt_lscatl")#A2000
Nº_Porcinos <- eurostat::get_eurostat("apro_mt_lspig")#[A3100]
Nº_Ovinos <- eurostat::get_eurostat("apro_mt_lssheep")#[A4100]
Nº_equinos <- eurostat::get_eurostat("apro_mt_lsequi")#[A1000]
Nº_AvedeCoral <- eurostat::get_eurostat("ef_lsk_poultry",update_cache = TRUE)#A5000
2+2
x <- rio::import("C:/Users/Henry/Downloads/51176 (1).csv")
y <- rio::import("./Datos/Nº_TotalES.csv")



---
  title: "Población de animales de granja en Europa"
description: |
  Un estudio simpre de la demografía ganadera europea
author:
  - name: Yutao Henry Jiang Zheng
affiliation: Universitat de València
affiliation-url: https://www.uv.es
date: 2026-01-02                           #--
categories: [trabajo BigData, Animales de granja, Demografía]   #--
image: "./imagenes/imagen_01.png"
title-block-banner: true #- {true, false, "green","#AA0000"}
title-block-banner-color: "white"    #-"#FFFFFF" 
toc-depth: 3
smooth-scroll: true
format: 
  html:
  #backgroundcolor: "#F1F3F4"
  #embed-resources: true
  link-external-newwindow: true
#css: assets/my_css_file.css   #- CUIDADO!!!!
code-tools: true
code-link: true
---
  
  
  # 1.Introducción
  
En este trabajo se analiza la demografía de los animales de granja en Europa, un aspecto fundamental para comprender la estructura y la evolución del sector ganadero en el continente. La población ganadera desempeña un papel clave en la economía europea, en la seguridad alimentaria y en el desarrollo de las zonas rurales.

A través del estudio de las principales especies de animales de granja, como el ganado bovino, porcino, ovino y avícola, se pretende examinar su distribución geográfica, su evolución en los últimos años y las diferencias existentes entre los distintos países europeos.

# 2. Datos

Para realizar el trabajo utilizaremos los siguientes datos:
  
  ::: {.panel-tabset}

## Fuente de los datos
Los datos utilizados en este estudio provienen principalmente de fuentes estadísticas oficiales, concretamente de [Eurostat](https://ec.europa.eu/eurostat/en/?etrans=es){target="_blank"} y del Instituto Nacional de Estadística ([INE](https://ine.es/){target="_blank"})


## Datos usados
- Nº_Bovinos → Número de bovinos por miles de cabezas en Europa
- Nº_Porcinos → Número de porcinos por miles de cabezas en Europa
- Nº_Ovinos → Número de ovinos por miles de cabezas en Europa
- Nº_AvedeCoral → Número de aves de coral por número de cabezas en Europa
- Nº_TotalEU → Número total de animales de granja en Europa
- Nº_BovinosES → Número de bovinos por número de cabezas en España
- Nº_PorcinosES → Número de porcinos por número de cabezas en España
- Nº_OvinosES → Número de ovinos por número de cabezas en España
- Nº_AvedeCoralES → Número de aves de coral por número de cabezas en España
- Nº_TotalES → Número total de animales de granja en España

## Paquetes utilizados

```{r}
#| results: hide 
#| warning: false 
#| message: false
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(eurostat)
library(scales)
library(ggthemes)
library(highcharter)
library(sf)
library(rnaturalearth)
library(patchwork)
```


## Codigo de los datos

```{r}
#| results: hide 
#| warning: false 
#| message: false
#Número de bovinos por miles de cabezas en Europa
Nº_bovinos <- eurostat::get_eurostat("apro_mt_lscatl")|>tidyr::separate_wider_delim(col = TIME_PERIOD, names = c("Year","Date"), delim = "-",too_many = "merge")

#Número de porcinos por miles de cabezas en Europa
Nº_Porcinos <- eurostat::get_eurostat("apro_mt_lspig")|>tidyr::separate_wider_delim(col = TIME_PERIOD, names = c("Year","Date"), delim = "-",too_many = "merge")

#Número de ovinos por miles de cabezas en Europa
Nº_Ovinos <- eurostat::get_eurostat("apro_mt_lssheep")|>tidyr::separate_wider_delim(col = TIME_PERIOD, names = c("Year","Date"), delim = "-",too_many = "merge")

#Número de aves de coral por número de cabezas en Europa
Nº_AvedeCoral <- read_csv("./Datos/ef_lsk_poultry_page_linear_2_0.csv")
Nº_AvedeCoral <- Nº_AvedeCoral|> mutate(Miles_Cabeza= OBS_VALUE/1000)|> rename("n_Cabeza"="OBS_VALUE")|> mutate(TIME_PERIOD = as.character(TIME_PERIOD))|> select("geo", "TIME_PERIOD", "freq", "animals", "unit","Miles_Cabeza","n_Cabeza")

#Número total de animales de granja en Europa
Nº_TotalEU <- full_join(Nº_bovinos, Nº_Porcinos, by = c("geo", "Year", "freq", "animals", "month", "unit", "values"))
Nº_TotalEU <-Nº_TotalEU|> full_join(Nº_Ovinos, by = c("geo", "Year", "freq", "animals", "month", "unit", "values"))
Nº_TotalEU <-Nº_TotalEU|> full_join(Nº_AvedeCoral, by = c("geo", "Year"="TIME_PERIOD", "freq", "animals", "unit", "values"="Miles_Cabeza"))
Nº_TotalEU <- Nº_TotalEU|> mutate(A=case_when(animals=="A2000"~"Bovino", animals=="A3100"~"Porcino", animals=="A4100"~"Ovino", animals=="A5000"~"Aves de corral", .default = ""))|> filter(A!="")|> group_by(Year, geo)|>mutate(TotalA=sum(values))|>ungroup()
Nº_TotalEU <- Nº_TotalEU|> mutate(Tipo=case_when(geo%in%c("EU27_2020", "EU28","EU27_2020","EU27_2007","EU25","EU","EU15")~"UE", .default = "Pais"))|> group_by(A,geo)|>mutate(N_animal=sum(values))|>ungroup()

#Número de bovinos por número de cabezas en España
Nº_BovinosES <- rio::import("./Datos/Nº_TotalES.1.csv",encoding = "Latin-1")
Nº_BovinosES <- Nº_BovinosES|> mutate(Total= as.numeric(gsub("\\.", "", Total)))
Nº_BovinosES <- Nº_BovinosES|> tidyr::separate_wider_delim(col = `Especies animales`, names = c("N","Animal"), delim = " ",too_many = "merge")
Nº_BovinosES <- Nº_BovinosES|> tidyr::separate_wider_delim(col = `Comunidades y Ciudades Autónomas`, names = c("Nº","Autonomias"), delim = " ",too_many = "merge",too_few = "align_start")|> mutate(Autonomias=case_when(is.na(Autonomias)~"Total_Nacional",TRUE ~ Autonomias))|> tidyr::separate_wider_delim(col = Provincias, names = c("Nu","Provincias"), delim = " ",too_many = "merge",too_few = "align_start")
Nº_BovinosES <- Nº_BovinosES|> filter(Animal=="Bovinos")|>select(Autonomias, Provincias, Animal, Total)

#Número de porcinos por número de cabezas en España
Nº_PorcinosES <- rio::import("./Datos/Nº_TotalES.1.csv",encoding = "Latin-1")
Nº_PorcinosES <- Nº_PorcinosES|> mutate(Total= as.numeric(gsub("\\.", "", Total)))
Nº_PorcinosES <- Nº_PorcinosES|> tidyr::separate_wider_delim(col = `Especies animales`, names = c("N","Animal"), delim = " ",too_many = "merge")
Nº_PorcinosES <- Nº_PorcinosES|> tidyr::separate_wider_delim(col = `Comunidades y Ciudades Autónomas`, names = c("Nº","Autonomias"), delim = " ",too_many = "merge",too_few = "align_start")|> mutate(Autonomias=case_when(is.na(Autonomias)~"Total_Nacional",TRUE ~ Autonomias))|> tidyr::separate_wider_delim(col = Provincias, names = c("Nu","Provincias"), delim = " ",too_many = "merge",too_few = "align_start")
Nº_PorcinosES <- Nº_PorcinosES|> filter(Animal=="Porcinos")|>select(Autonomias, Provincias, Animal, Total)

#Número de ovinos por número de cabezas en España
Nº_OvinosES <- rio::import("./Datos/Nº_TotalES.1.csv",encoding = "Latin-1")
Nº_OvinosES <- Nº_OvinosES|> mutate(Total= as.numeric(gsub("\\.", "", Total)))
Nº_OvinosES <- Nº_OvinosES|> tidyr::separate_wider_delim(col = `Especies animales`, names = c("N","Animal"), delim = " ",too_many = "merge")
Nº_OvinosES <- Nº_OvinosES|> tidyr::separate_wider_delim(col = `Comunidades y Ciudades Autónomas`, names = c("Nº","Autonomias"), delim = " ",too_many = "merge",too_few = "align_start")|> mutate(Autonomias=case_when(is.na(Autonomias)~"Total_Nacional",TRUE ~ Autonomias))|> tidyr::separate_wider_delim(col = Provincias, names = c("Nu","Provincias"), delim = " ",too_many = "merge",too_few = "align_start")
Nº_OvinosES <- Nº_OvinosES|> filter(Animal=="Ovinos")|>select(Autonomias, Provincias, Animal, Total)

#Número de aves de coral por número de cabezas en España
Nº_AvedeCoralES <- rio::import("./Datos/Nº_TotalES.1.csv",encoding = "Latin-1")
Nº_AvedeCoralES <- Nº_AvedeCoralES|> mutate(Total= as.numeric(gsub("\\.", "", Total)))
Nº_AvedeCoralES <- Nº_AvedeCoralES|> tidyr::separate_wider_delim(col = `Especies animales`, names = c("N","Animal"), delim = " ",too_many = "merge")
Nº_AvedeCoralES <- Nº_AvedeCoralES|> tidyr::separate_wider_delim(col = `Comunidades y Ciudades Autónomas`, names = c("Nº","Autonomias"), delim = " ",too_many = "merge",too_few = "align_start")|> mutate(Autonomias=case_when(is.na(Autonomias)~"Total_Nacional",TRUE ~ Autonomias))|> tidyr::separate_wider_delim(col = Provincias, names = c("Nu","Provincias"), delim = " ",too_many = "merge",too_few = "align_start")
Nº_AvedeCoralES <- Nº_AvedeCoralES|> filter(Animal=="Aves de corral")|>select(Autonomias, Provincias, Animal, Total)

#Número total de animales de granja en España
Nº_TotalES <- rio::import("./Datos/Nº_TotalES.1.csv",encoding = "Latin-1")
Nº_TotalES <- Nº_TotalES|> mutate(Total= as.numeric(gsub("\\.", "", Total)))
Nº_TotalES <- Nº_TotalES|> tidyr::separate_wider_delim(col = `Especies animales`, names = c("N","Animal"), delim = " ",too_many = "merge")
Nº_TotalES <- Nº_TotalES|> tidyr::separate_wider_delim(col = `Comunidades y Ciudades Autónomas`, names = c("Nº","Autonomias"), delim = " ",too_many = "merge",too_few = "align_start")|> mutate(Autonomias=case_when(is.na(Autonomias)~"Total_Nacional",TRUE ~ Autonomias))|> tidyr::separate_wider_delim(col = Provincias, names = c("Nu","Provincias"), delim = " ",too_many = "merge",too_few = "align_start")
Nº_TotalES <- Nº_TotalES|>select(Autonomias, Provincias, Animal, Total)
```

:::
  
  ---
  
  # La poblacion de las granjas europeas
  
  Europa históricamente fue una región que dependía en gran medida de la ganadería, ya que una parte importante de su territorio presentaba suelos poco fértiles para la agricultura. Esto derivó en que muchos de los pueblos que la habitaban se dedicaran principalmente a la cría de animales como medio de subsistencia.
En la actualidad, las mejoras en las técnicas de cultivo y la disponibilidad del mercado internacional han provocado un cambio significativo en la forma de vida de los europeos, lo que ha dado lugar a una disminución del número de personas dedicadas a la actividad ganadera.

En la gráfica se puede apreciar que la población de animales de granja muestra una tendencia negativa desde el año 2021, reflejando el descenso progresivo del sector en los últimos años.

```{r}
#| warning: false
#| code-fold: true
Nº_TotalEU1 <- Nº_TotalEU|> filter(A!="Aves de corral", Year>="2000", Year<="2024")|> group_by(Year,geo)|> mutate(TotalA1=sum(values))|>ungroup()
Datos_P.UE <- Nº_TotalEU1|> filter(Tipo=="Pais")|> select(geo,Year,A,TotalA1)
Datos_EU27_2020 <- Nº_TotalEU1|> filter(geo=="EU27_2020")|> select(geo,Year,A,TotalA1)

ggplot(Datos_P.UE,aes(x= Year, y= TotalA1, group=geo)) +geom_line(color="gray")+ geom_line(data=Datos_EU27_2020, aes(x=Year, y=TotalA1, colour = "EU27_2020"))+ scale_colour_manual(values = c("EU27_2020" = "blue3"),labels = c("Unión Europea")) + scale_y_continuous(labels = label_comma(accuracy = 1))+theme_bw()+ theme(axis.text.x = element_text(angle = 49, vjust = 1, hjust = 1), plot.background = element_rect(fill = "lightyellow"), plot.caption = element_text(hjust = 0, face = "italic", size = 7),plot.title = element_text(face = "bold"))+labs(title="Evolución de la población de animales de granja en Europa (2000-2024)", x="Años",y="Miles de cabezas", colour="Región", caption = "*En esta gráfica solo se incluyen los mamíferos, ya que los datos correspondientes a las aves de corral \nno eran consistentes a lo largo del tiempo.")

```
## Distribucion por país

```{r}
#| warning: false
#| code-fold: true
Datos_mapaUE <- Nº_TotalEU|> filter(Tipo=="Pais", Year=="2020")|> select(geo,Year,A,TotalA)
Datos_mapaUE1 <- Datos_mapaUE|> filter(A=="Porcino")
mapa_ue <- get_eurostat_geospatial(resolution = "20",nuts_level = "0", year = 2021)
mapaPA_UE2023<- mapa_ue|> inner_join(Datos_mapaUE1, by = c("id" = "geo"))

M1 <- ggplot(mapaPA_UE2023) + geom_sf(aes(fill = TotalA), color = "white", linewidth = 0.01) + coord_sf(crs = 3035, xlim = c(2500000, 6500000), ylim = c(1500000, 5200000)) +
  scale_fill_viridis_c(option = "inferno", labels = scales::label_comma(big.mark = "."), 
                       name = "Miles de cabezas") + theme_void() + labs(title = "Censo ganadero por país (2020)") + theme(plot.title = element_text(face = "bold", size = 11))  

Datos_mapaUE2 <- Datos_P.UE|> filter(Year=="2020")
mapaPA_UE2023.1<- mapa_ue|> inner_join(Datos_mapaUE2, by = c("id" = "geo"))

M2 <- ggplot(mapaPA_UE2023.1) + geom_sf(aes(fill = TotalA1), color = "white", linewidth = 0.01) + coord_sf(crs = 3035, xlim = c(2500000, 6500000), ylim = c(1500000, 5200000)) +
  scale_fill_viridis_c(option = "inferno", labels = scales::label_comma(big.mark = "."), 
                       name = "Miles de cabezas") + theme_void() + labs(title = "Censo ganadero de animales mamíferos \npor país (2020)") + theme(plot.title = element_text(face = "bold", size = 11))

M1+M2
```
La población de animales de granja en Europa se concentra principalmente en Francia, seguida de España, Polonia y Alemania. No obstante, al considerar únicamente a los mamíferos, España destaca como el país con mayor número de animales de granja, debido a su liderazgo en la industria porcina y ovina y su gran relevancia en la bovina en el contienente europeo, lo que la sitúa entre los países con mayor peso ganadero dentro de la Unión Europea, como se puede apreciar en los mapas.

---
  # Distribución de las especies de animales
  
  La especie de animal de granja con mayor número de población son las aves de corral, seguidas por los porcinos, los bovinos y los ovinos. Esta distribución se debe, en gran medida, al espacio necesario para su cría.
Las aves de corral y los cerdos requieren menos espacio por individuo, lo que permite una producción más intensiva. En cambio, los bovinos y ovinos necesitan amplias zonas de pastoreo, lo que limita su número. Por este motivo, las aves de corral constituyen el grupo más numeroso, seguidas por el ganado porcino.

```{r}
#| warning: false
#| code-fold: true
Rosca <- Nº_TotalEU|>filter(geo=="EU27_2020", Year=="2020")|> select(A,Year, N_animal)|> mutate(Suma=sum(N_animal))|>mutate(Porcentaje=N_animal/Suma*100)

hchart(Rosca, "pie", hcaes(x = A, y = Porcentaje))|>hc_plotOptions(pie = list(dataLabels = list( enabled = TRUE, format = "{point.name}: {point.percentage:.0f}%")))|>hc_title(text = "Distribución porcentual de los animales de granja en la UE (2020)")|> hc_chart(backgroundColor = "#f0f0f0")

```

---
  
  ### Primer gráfico
  
  
  ```{r}

```



