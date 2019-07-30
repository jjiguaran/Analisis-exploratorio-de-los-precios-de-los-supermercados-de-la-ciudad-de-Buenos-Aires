# TP
# Los archivos json fueron abiertos con r utilizando la liberia rjson, luego fueron transformados dataframes
# una vez dataframes,  se realizo un merging atravez de la id de las sucursales y de la id de los productos para generar un archivo completo
# las columnas de x_id de los 3 dataframes fueron eliminadas
# setear wd
setwd('/home/juan/Documents/Academico/Maestria/Data mining/tp')
# caragr el data set
datos <- read.csv('data.csv')
# se abre una ventana con los datos
View(datos)
# caracteristicas atributos
str(datos)
# muestra el rango de las variables
summary(datos)
# al hacer la integracion , todas las sucursales de autoservico y 5 de de supermercado no reportaron datos asi que furon eliminados
nuevos <- read.csv('actualizados.csv')
# por otro lado, las que si reportaron, no reportaron la totalidad de las 10 mediciones, por lo que se procede a generar un nuevo dataset que incluya estas lecturas y su precio sea NA
library('dplyr')
rem <- c(-3,-4,-5)
comb <- distinct(nuevos[,rem])
comb_rep <- do.call('rbind',(replicate(10, comb, simplify = F)))
comb_rep <- comb_rep[with(comb_rep, order(producto, sucursal)), ]
med <- distinct(nuevos[,], fecha, medicion)
med_rep <- do.call('rbind',(replicate(167311, med, simplify = F)))
comp_comb  <-cbind(comb_rep,med_rep)
new <-left_join(comp_comb, nuevos)
write.csv(new, 'faltantes.csv', row.names = F)
remove(comb)
remove(comb_rep)
remove(med_rep)
remove(med)
falt <- read.csv('datos_completos.csv')
# en total se registraron 88449 faltantes
# para su tratamiento, se implementara una interpolacion de los precios para la misma sucursal del mismo producto
# para cmparacion, se copiara el datatset
library(data.table)
falt2 <- copy(falt)
# primero se obtienen lo indices de estos faltantes
ind1 <- which(is.na(falt$precio), arr.ind=TRUE)
ind <- ind1[1:length(ind1)]
# se procede a la imputacion de faltantes, pero puesto que es posible que existan productos de solo una lectura, se imputara este valor para las demas lecturas de ser el caso
cont = 0
for (i in ind1) {
  precios_list =  falt[falt$sucursal == falt$sucursal[i] & falt$producto == falt$producto[i], 'precio']
  precios <- precios_list[!is.na(precios_list)]
  medicion <- which(precios_list %in% precios)
  if (length(precios) > 2) {
    falt$precio[i] <- as.numeric(approx(medicion, precios, xout = falt$medicion[i]))[2]
  }
  if (length(precios) == 1) {
    falt$precio[i] <- precios[!is.na(precios)]
  }
  if (sum(precios == precios) == length(precios)) {
    falt$precio[i] <- as.numeric(mean(precios, na.rm = T))
  }
  cont = cont +1
  por <- (cont/length(ind))*100
  if (por%%5 == 0) {
    print(por)
  }
}
write.csv(falt,'full_data.csv', row.names = F)
nuevos <- read.csv('full_data.csv')
nuevos$marca <- NULL
nuevos$presentacion <- NULL
nuevos$direccion <- NULL
nuevos$provincia <- NULL
nuevos$banderaId <- NULL
nuevos$localidad <- NULL
nuevos$banderaDescripcion <- NULL
nuevos$lat <- NULL
nuevos$lng <- NULL
nuevos$sucursalNombre <- NULL
nuevos$comercioId <- NULL
nuevos$sucursalId <- NULL
write.csv(nuevos,'final_data.csv', row.names = F)
nuevos <- read.csv('final_data.csv')
# El objetivo del TP sera estudirar la canasta basica, a continuacion se crea un vector con los nombres de estos productos:
canasta <- c("Pan","Galletitas", "Arroz", "Harina de Trigo","Fideos", "Azucar", "Dulce","Tomate", "Leche", "Yogur","Manteca","Aceite", "Gaseosa","Jugo","Cerveza", "Vino", "Sal","Ketchup","Mayonesa","Cafe","Yerba" )
# se importa librerias para el analis
library(data.table)
library(dplyr)
# solo se toman productos no perecederos, pues son los que se encuentran en los datos
# para empezar se crea un vector con los nombres de las distintas emprezas
empresas <- as.vector(distinct(nuevos[,] , comercioRazonSocial ))
#luego, se crea un nuevo dataframe para almacenar los datos de la canasta familiar, es decir, el producto, la medicion, la media del precio, y la media del precio por empresa
canasta_data <- data.frame(matrix(NA, ncol = 9))
canasta_data <- canasta_data[-1,]
nombres <- c('medicion','item','media')
for (i in empresas) {
  nombres <- c(nombres, as.character(i))
}
names(canasta_data) <- nombres
# se procede a calcular las medias para cada producto, para cada medicion, para cada empresa, asi como su media
for (i in 1:10) {
  for (j in canasta) {
    busca <- paste("^", j, " ", sep="")
    media <-as.numeric(mean(nuevos[nuevos$nombre %like% busca & nuevos$medicion == i, 'precio']))
    media_emp <- vector()
    for (k in nombres[4:9]) {
      empr <- as.numeric(mean(nuevos[nuevos$nombre %like% busca & nuevos$medicion == i & nuevos$comercioRazonSocial == k, 'precio']))
      media_emp <- c(media_emp, empr)
    }
    canasta_data[nrow(canasta_data) + 1,] <- c(i,j,media,media_emp)
  }
}
canasta_media <- data.frame(matrix(NA, ncol = 8))
canasta_media <- canasta_media[-1,]
names(canasta_media) <- nombres[-1]
for (i in canasta) {
  full_media <- vector()
  for (j in names(canasta_media)[-1]) {
    media <- round(mean(canasta_data[canasta_data$item == i, j]),1)
    full_media <- c(full_media,media)
  }
  canasta_media[nrow(canasta_media) + 1,] <- c(i,full_media)
}
# se numerizan los datos pues en su creacion, se perdio el formato
for (i in 3:9) {
  canasta_data[,i] <- as.numeric(canasta_data[,i])
}
# ahora se calculara la media de los productos de la canasta tanto para el total, como para las empresas
media_canasta <- data.frame(matrix(NA, ncol = 8))
media_canasta <- media_canasta[-1,]
names(media_canasta) <- nombres[-2]
for (i in 1:10) {
  media_emp <- vector()
  for (j in nombres[3:9]) {
    media <-round(mean(canasta_data[canasta_data$medicion == i, j], na.rm = T))
    media_emp <- c(media_emp, media)
  }
  media_canasta[nrow(media_canasta) + 1,] <- c(i,media_emp)
}
# ahora se calculara la media para todas las mediciones con base en los resultados obtenidos
medias_totales <- data.frame(matrix(NA, ncol = 7))
medias_totales <- medias_totales[-1,]
names(medias_totales) <- nombres[c(-1,-2)]
media_emp <- vector()
for (i in 2:8) {
  media <-round(mean(media_canasta[, i], na.rm = T),1)
  media_emp <- c(media_emp, media)
}
medias_totales[nrow(medias_totales) + 1,] <- media_emp
# variaciones
variaciones <- data.frame(matrix(NA, ncol = 8))
variaciones <- variaciones[-1,]
names(variaciones) <- nombres[-2]
for (i in 1:9) {
  vari_emp <- vector()
  for (j in nombres[3:9]) {
    vari <- round(((media_canasta[i+1,j] - media_canasta[i,j])/media_canasta[i,j])*100,2)
    vari_emp <- c(vari_emp, vari)
  }
  variaciones[nrow(variaciones) + 1,] <- c(i,vari_emp)
}
names(variaciones)[1] <- 'salto'
# varaiciones acumuladas 
acum <- data.frame(matrix(NA, ncol = 8))
acum <- acum[-1,]
names(acum) <- nombres[-2]
for (i in 1:9) {
  vari_acum <- vector()
  for (j in nombres[3:9]) {
    if (i == 1) {
      ac <- variaciones[i,j]
    }
    if (i != 1) {
      ac <- variaciones[i,j] + acum[i-1,j]
    }
    vari_acum <- c(vari_acum, ac)
  }
  acum[nrow(acum) + 1,] <- c(i,vari_acum)
}
names(acum)[1] <- 'salto'
# variaciones acumuladas totales
variaciones_acumuladas <- data.frame(matrix(NA, ncol = 7))
variaciones_acumuladas <- variaciones_acumuladas[-1,]
names(variaciones_acumuladas) <- names(variaciones)[-1]
var_acm <- colSums(variaciones[,-1])
variaciones_acumuladas[nrow(variaciones_acumuladas) + 1,] <- var_acm
# comparativo
comparativo <- copy(variaciones_acumuladas)
for (i in 1:ncol(comparativo)) {
  comparativo[1,i] <- round((comparativo[1,i]/7)*100,1)
}
