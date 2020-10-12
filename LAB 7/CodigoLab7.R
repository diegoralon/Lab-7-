library(readxl)
library(dplyr)
library(highcharter)
library(tidyverse)


# Limpieza de datos 
df <- read.csv("c1.csv")
View(df)

df$Camion_5 <- gsub("Q-","0",df$Camion_5)
df$Camion_5 <- gsub("Q","",df$Camion_5)
View(df)
df$Pickup <- gsub("Q-","0",df$Pickup)
df$Pickup <- gsub("Q","",df$Pickup)
df$Moto <- gsub("Q-","0",df$Moto)
df$Moto <- gsub("Q","",df$Moto)
df$directoCamion_5 <- gsub("Q-","0",df$directoCamion_5)
df$directoCamion_5 <- gsub("Q","",df$directoCamion_5)
df$directoPickup <- gsub("Q-","0",df$directoPickup)
df$directoPickup <- gsub("Q","",df$directoPickup)
df$directoMoto <- gsub("Q-","0",df$directoMoto)
df$directoMoto <- gsub("Q","",df$directoMoto)
df$fijoCamion_5 <- gsub("Q-","0",df$fijoCamion_5)
df$fijoCamion_5 <- gsub("Q","",df$fijoCamion_5)
df$fijoPickup <- gsub("Q-","0",df$fijoPickup)
df$fijoPickup <- gsub("Q","",df$fijoPickup)
df$fijoMoto <- gsub("Q-","0",df$fijoMoto)
df$fijoMoto <- gsub("Q","",df$fijoMoto)
df$factura <- gsub("Q-","0",df$factura)
df$factura <- gsub("Q","",df$factura)
View(df)

df$Camion_5 <- as.integer(df$Camion_5)
df$Pickup <- as.integer(df$Pickup)
df$Moto <- as.integer(df$Moto)
df$factura <- as.integer(df$factura)

# Estado De resultados Breve 
str(df)
df$Ct <- df$Camion_5+df$Pickup+df$Moto
View(df)

df$utilidadop <- df$factura-df$Ct
View(df)

estadoderesultado <- data.frame(cbind(df$factura,df$Ct,df$utilidadop))
View(estadoderesultado)
estadoderesultado$X1 <- sum(estadoderesultado$X1)
estadoderesultado$X2 <-sum(estadoderesultado$X2)
estadoderesultado$X3 <- sum(estadoderesultado$X3)
View(estadoderesultado)

write_excel_csv2(estadoderesultado, "edr.csv", delim = ";")

# Como quedo el tarifario por unidad 
# Camión
df$camion <- gsub("[1-9]","1", df$Camion_5)
View(df)

camionsuma <- df %>% 
  select(camion,factura) %>% 
  group_by(camion) %>% 
  summarise(suma = sum(factura))
write_excel_csv2(camionsuma, "camsum.csv", delim = ";")

df %>% 
  select(camion,factura) %>% 
  group_by(camion) %>% 
  summarise(n())

#PickUp
df$pick <- gsub("[1-9]","1", df$Pickup)
picksum <- df %>% 
  select(pick,factura) %>% 
  group_by(pick) %>% 
  summarise(suma = sum(factura))

write_excel_csv2(picksum, "picksum.csv", delim = ";")

df %>% 
  select(pick,factura) %>% 
  group_by(pick) %>% 
  summarise(n())

# Moto 

df$motos <- gsub("[1-9]","1", df$Moto)
motosum <- df %>% 
  select(motos,factura) %>% 
  group_by(motos) %>% 
  summarise(suma = sum(factura))

write_excel_csv2(motosum, "motosum.csv", delim = ";")

df %>% 
  select(motos,factura) %>% 
  group_by(motos) %>% 
  summarise(n())

# Las tarifas actuales son aceptadas por los clientes

df %>%
  select(Cod,factura) %>% 
  group_by(Cod) %>% 
  summarise(media = mean(factura)) %>% 
  hchart(type = "bar",hcaes(x = Cod, y = media), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text = "<b>Media de facturación por codigo<b>")

df %>%
  select(Cod,factura) %>% 
  group_by(Cod) %>% 
  summarise(pedidos = n()) %>% 
  hchart(type = "bar",hcaes(x = Cod, y = pedidos), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text = "<b>Numero de pedidos por código<b>")


# Cuanto le perdemos a los cambios

# 1 escenario reducción de facturación del 20%

df$escenario1 <- df$factura*0.80
View(df)

df$utilidadescenario1 <- df$escenario1-df$Ct
View(df)
# Con facturación actual
df %>% 
  select(Cod,utilidadop) %>% 
  group_by(Cod) %>% 
  summarise(utilidad = sum(utilidadop)) %>% 
hchart(type = "bar",hcaes(x = Cod, y = utilidad), 
       dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text = "<b>Utilidad con facturación actual<b>")


# Con 10% Menos 
df$escenario4 <- df$factura*0.90
View(df)

df$utilidadescenario4 <- df$escenario4-df$Ct
View(df)

df %>%
  select(Cod,utilidadescenario4) %>% 
  group_by(Cod) %>% 
  summarise(Utilidad = sum(utilidadescenario4)) %>% 
  hchart(type = "bar",hcaes(x = Cod, y = Utilidad), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text = "<b>Utilidad con 10% menos de facturación<b>")


# Con 20% menos
df %>%
  select(Cod,utilidadescenario1) %>% 
  group_by(Cod) %>% 
  summarise(Utilidad = sum(utilidadescenario1)) %>% 
  hchart(type = "bar",hcaes(x = Cod, y = Utilidad), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text = "<b>Utilidad con 20% menos de facturación<b>")


# Con 25% menos

df$escenario3 <- df$factura*0.75
View(df)

df$utilidadescenario3 <- df$escenario3-df$Ct
View(df)

df %>%
  select(Cod,utilidadescenario3) %>% 
  group_by(Cod) %>% 
  summarise(Utilidad = sum(utilidadescenario3)) %>% 
  hchart(type = "bar",hcaes(x = Cod, y = Utilidad), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text = "<b>Utilidad con 25% menos de facturación<b>")


# Con 30% menos

df$escenario2 <- df$factura*0.70
View(df)

df$utilidadescenario2 <- df$escenario2-df$Ct
View(df)

df %>%
  select(Cod,utilidadescenario2) %>% 
  group_by(Cod) %>% 
  summarise(Utilidad = sum(utilidadescenario2)) %>% 
  hchart(type = "bar",hcaes(x = Cod, y = Utilidad), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text = "<b>Utilidad con 30% menos de facturación<b>")

# Debemos abrir más centros de distribución

costoporcdd <- df %>% 
  select(origen,Ct) %>%
  group_by(origen) %>%
  summarise(costo = sum(Ct)) 

write_excel_csv2(costoporcdd, "ccd.csv", delim = ";")

df$longpos <- df$Long*-1

longitud <- df %>% 
  select(origen,longpos) %>%
  group_by(origen) %>% 
  summarise(distancia = sum(longpos)) 

write_excel_csv2(longitud, "longi.csv", delim = ";")

ganancias <- df %>% 
  select(origen,utilidadop) %>% 
  group_by(origen) %>% 
  summarise(utilidad = sum(utilidadop))

write_excel_csv2(ganancias, "ganan.csv", delim = ";")

# 80-20 de la factura 

df %>%
  select(origen,ID) %>% 
  group_by(origen) %>% 
  summarise(postesatendidos = n_distinct(ID)) %>% 
  hchart(type = "bar",hcaes(x = origen, y = postesatendidos), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text = "<b>Postes atendidos por centro<b>")


df$origen <- gsub("150224","P1-150224",df$origen)
df$origen <- gsub("150277","P2-150277",df$origen)
df$origen <- gsub("150278","P3-150278",df$origen)
df$origen <- gsub("150841","P4-150841",df$origen)

df %>%
  select(origen,factura) %>% 
  group_by(origen) %>% 
  summarise(facturacion = sum(factura)) %>% 
  hchart(type = "bar",hcaes(x = origen, y = facturacion), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text = "<b>facturacion por centro de distribución<b>")

# Postes que necesitan más mantenimiento

df %>%
  select(ID,Cod) %>% 
  group_by(ID) %>% 
  summarise(postesmasatendidos = n()) %>% 
  arrange(desc(postesmasatendidos)) %>% 
  filter(postesmasatendidos > 154) %>% 
  hchart(type = "bar",hcaes(x = ID, y = postesmasatendidos), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text = "<b>Postes más atendidos<b>")

df$ID <- gsub("477971", "M1-477971", df$ID)
df$ID <- gsub("863979", "M2-477971", df$ID)
df$ID <- gsub("773607", "M3-477971", df$ID)
df$ID <- gsub("969156", "M4-477971", df$ID)
df$ID <- gsub("337161", "M5-477971", df$ID) 
df$ID <- gsub("519948", "M6-477971", df$ID) 
df$ID <- gsub("507880", "M7-477971", df$ID)
df$ID <- gsub("353203", "M8-477971", df$ID) 
df$ID <- gsub("452252", "M9-477971", df$ID)
df$ID <- gsub("328733", "M10-477971", df$ID)
  
  


            