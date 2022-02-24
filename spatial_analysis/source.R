# WCZYTANIE PLIKU SHAPEFILE Z OSIEDLAMI NA TERNIE KRAKOWA
library(sf)
shape<-st_read("osiedla.shp")   

library(tmap)
tm_shape(shape) +
  tm_polygons() + 
  tm_layout(main.title="Osiedla na terenie Krakowa",
            main.title.position = "center") +
  tm_scale_bar()

# KOD UKŁADU WSPÓŁRZĘDNYCH
st_crs(shape)$epsg

# WCZYTANIE PLIKU Z ZAREJESTROWANYMI WYKROCZENIAMI NA TERENIE KRAKOWA
library(readxl)
points <- read_excel("zestaw9.xlsx") 

head(points)
nrow(points)
summary(points)

# STWORZENIE OBIEKTU TYPU SF
points_sf<-st_as_sf(points,coords=c("Long","Lat"), crs="+proj=longlat")

# ZMIANA NA ODPOWIEDNI UKŁAD WSPÓŁRZĘDNYCH
points_sf<-st_transform(points_sf,crs=st_crs(shape))

tm_shape(shape) +
  tm_borders(lwd = 2) +
  tm_shape(points_sf) + 
  tm_dots(col="blue") +
  tm_layout(main.title="Zarejestrowane wykroczenia na terenie Krakowa",
            main.title.position = "center") +
  tm_scale_bar()

# ZAPISANIE PUNKTÓW DO PLIKU SHAPEFILE
st_write(points_sf, "D:/RStudio/projekt_analiza/points.shp")

# WCZYTANIE PUNKTÓW
points<-st_read("points.shp")

# DODATKOWO POBRANA MAPA DZIELNIC KRAKOWA ZE STRONY gis-support.pl
districts<-st_read("dzielnice_Krakowa.shp")  
districts<-st_transform(districts,crs=st_crs(shape))

shape_map<-tm_shape(shape) +
  tm_polygons() +
  tm_layout(main.title = "Wykroczenia na terenie Krakowa",
            main.title.position = "center",
            legend.outside = TRUE) +
  tm_scale_bar() 

tm_shape(districts) +
  tm_borders(lw=2, col = "blue") +
  tm_scale_bar() +
  tm_text("nazwa", size="AREA")


###############################


library(dbscan)

# PRZYGOTOWANIE MACIERZY KOORDYNATÓW DO WYKONANIA ALGORYTMÓW
points_matrix<-st_coordinates(points)
summary(points_matrix)

#ALGORYTM DBSCAN

dbscan_1 <- dbscan(points_matrix, eps = 300, minPts = 5)
points$dbscan_1 <- as.factor(dbscan_1$cluster)
shape_map + tm_shape(points) + tm_dots("dbscan_1", palette="Set1")
# KLASTER Z WARTOŚCIĄ '0' JEST SZUMEM
# WYSTĘPUJE ZBYT DUŻA LICZBA KLASTRÓW, ZWIĘKSZAMY eps

dbscan_2 <- dbscan(points_matrix, eps = 500, minPts = 5)
points$dbscan_2 <- as.factor(dbscan_2$cluster)
shape_map + tm_shape(points) + tm_dots("dbscan_2", palette="Set1")
# LICZBA KLASTRÓW SIE ZMNIEJSZYŁA

# SZUKAMY OPTYMALNEGO EPS PRZY UŻYCIU FUNKCJI WYZNACZAJĄCEJ ODLEGŁOŚCI DO K-NAJBLIŻSZYCH SĄSIADÓW
kNNdistplot(points_matrix, k = 5)
# WYZNACZENIE "KOLANA" NA WYKRESIE
abline(h = 700, lty = 2)

dbscan_3 <- dbscan(points_matrix, eps = 700, minPts = 5)
points$dbscan_3 <- as.factor(dbscan_3$cluster)
shape_map + tm_shape(points) + tm_dots("dbscan_3", palette="Set1")
# LICZBA KLASTRÓW ZNÓW ULEGŁA ZMNIEJSZENIU, SPRÓBUJMY ZWIĘKSZYĆ minPts

dbscan_4 <- dbscan(points_matrix, eps = 700, minPts = 10)
points$dbscan_4 <- as.factor(dbscan_4$cluster)
shape_map + tm_shape(points) + tm_dots("dbscan_4", palette="Set1")
# TERAZ ZMNIEJSZMAMY eps PRZY TAKIM SAMYM minPts

dbscan_5 <- dbscan(points_matrix, eps = 500, minPts = 10)
points$dbscan_5 <- as.factor(dbscan_5$cluster)
shape_map + tm_shape(points) + tm_dots("dbscan_5", palette="Set1")

# UWAŻAM, ŻE NAJLEPSZY WYNIK ZOSTAŁ OTRZYMANY PRZY 5 WYNIKU DLA eps = 500 i minPts = 10
dbscan_map <- tm_shape(points) + tm_dots("dbscan_5", palette="Set1")

districts_map<-tm_shape(districts) +
  tm_borders(col="black") +
  tm_scale_bar() +
  tm_text("nr_dzielni", size="AREA") +
  tm_layout(main.title="Zarejestrowane wykroczenia na terenie Krakowa",
            main.title.position = "center",
            legend.outside = TRUE)

# DWA RAZY POWTÓRZONE DISTRICT MAP, ABY BYŁ ZASIĘGIEM MAPY I OSTATNIĄ WARSTWĄ
districts_map + dbscan_map + districts_map 

#ALGORYTM OPTICS

optics_1 <- optics(points_matrix, eps = 300, minPts = 5)
points$optics_1<-as.factor(extractDBSCAN(optics_1, eps_cl = 300)$cluster)
shape_map + tm_shape(points) + tm_dots("optics_1", palette="Set1")
# WYSTĘPUJE ZBYT DUŻA LICZBA KLASTRÓW, ZWIĘKSZAMY eps

optics_2 <- optics(points_matrix, eps = 500, minPts = 5)
points$optics_2<-as.factor(extractDBSCAN(optics_2, eps_cl = 500)$cluster)
shape_map + tm_shape(points) + tm_dots("optics_2", palette="Set1")
# LICZBA KLASTRÓW SIĘ ZMNIEJSZYŁA

optics_3 <- optics(points_matrix, eps = 800, minPts = 5)
points$optics_3<-as.factor(extractDBSCAN(optics_3, eps_cl = 800)$cluster)
shape_map + tm_shape(points) + tm_dots("optics_3", palette="Set1")
# ZBYT MAŁA LICZBA KLASTROW, UTWORZYŁ SIE JEDEN DUŻY KLASTER W CENTRUM
# ZWIĘKSZAMY minPts

optics_4 <- optics(points_matrix, eps = 800, minPts = 10)
points$optics_4<-as.factor(extractDBSCAN(optics_4, eps_cl = 800)$cluster)
shape_map + tm_shape(points) + tm_dots("optics_4", palette="Set1")
# NADAL ZBYT MAŁO KLASTROW, ZMNIEJSZAMY ZATEM eps

optics_5 <- optics(points_matrix, eps = 500, minPts = 10)
points$optics_5<-as.factor(extractDBSCAN(optics_5, eps_cl = 500)$cluster)
shape_map + tm_shape(points) + tm_dots("optics_5", palette="Set1")

# UWAŻAM, ŻE NAJLEPSZY WYNIK ZOSTAŁ OTRZYMANY PRZY 5 WYNIKU DLA eps = 500 i minPts = 10 (TAK JAK PRZY DBSCAN)
optics_map<- tm_shape(points) + tm_dots("optics_5", palette="Set1")
districts_map + optics_map + districts_map


# ALGORYTM HDBSCAN 

hdbscan_1 <- hdbscan(points_matrix, minPts = 5)
points$hdbscan_1 <- as.factor(hdbscan_1$cluster)
shape_map + tm_shape(points) + tm_dots("hdbscan_1", palette="Set1")
# UTWORZYŁY SIE TYLKO DWA KLASTRY, SPRÓBUJEMY ZWIĘKSZYĆ minPTs

hdbscan_2 <- hdbscan(points_matrix, minPts = 10)
points$hdbscan_2 <- as.factor(hdbscan_2$cluster)
shape_map + tm_shape(points) + tm_dots("hdbscan_2", palette="Set1")
# LICZBA KLASTRÓW SIE ZNACZĄCO ZWIĘKSZYŁA, SPRÓBUJEMY ZWIĘKSZYĆ JESZCZE RAZ PARAMETR minPts

hdbscan_3 <- hdbscan(points_matrix, minPts = 15)
points$hdbscan_3 <- as.factor(hdbscan_3$cluster)
shape_map + tm_shape(points) + tm_dots("hdbscan_3", palette="Set1")
# LICZBA KLASTRÓW ULEGŁA ZBYT DUŻEMU ZMNIEJSZENIU, ZATEM ZMNIEJSZAMY minPts

hdbscan_4 <- hdbscan(points_matrix, minPts = 13)
points$hdbscan_4 <- as.factor(hdbscan_4$cluster)
shape_map + tm_shape(points) + tm_dots("hdbscan_4", palette="Set1")
# ZNÓW ZMNIEJSZYMY minPts

hdbscan_5 <- hdbscan(points_matrix, minPts = 8)
points$hdbscan_5 <- as.factor(hdbscan_5$cluster)
shape_map + tm_shape(points) + tm_dots("hdbscan_5", palette="Set1")

# UWAŻAM, ŻE NAJLEPSZY WYNIK ZOSTAŁ OTRZYMANY PRZY 4 WYNIKU DLA minPts = 13
hdbscan_map<-tm_shape(points) + tm_dots("hdbscan_4", palette="Set1") 
districts_map + hdbscan_map + districts_map


#ANIMOWANE MAPY
tmap_mode("view")
shape$NAZWA_JEDN<- iconv(shape$NAZWA_JEDN, "ASCII", "UTF-8", sub="byte")
shape_map+tm_text("NAZWA_JEDN", size="AREA")
