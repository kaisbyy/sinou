#NYC flights
library(readr)
library(dplyr)
install.packages("rmarkdown")


#Import des données
airlines <- read_delim("datasets/csv/airlines.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
airports <- read_delim("datasets/csv/airports.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
flights <- read_delim("datasets/csv/flights.csv",";", escape_double = FALSE, trim_ws = TRUE)
planes <- read_delim("datasets/csv/planes.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
states_airports <- read_delim("datasets/csv/states_airports.csv",  ";", escape_double = FALSE, trim_ws = TRUE)
weather <- read_delim("datasets/csv/weather.csv",  ";", escape_double = FALSE, trim_ws = TRUE)


colnames(airlines)
summary(airlines)
lapply(airlines,table)

colnames(airports)
summary(airports)
#lapply(airports,table)


colnames(flights)
summary(flights)
#lapply(flights,table)

colnames(planes)
summary(planes)
lapply(planes,table)



#1. Réaliser une jointure entre les différentes tables
df_departure = flights %>%
  left_join(airlines,by="carrier")%>%
  left_join(states_airports,by=c("origin"="faa"))%>%
  left_join(planes,by="tailnum")


#2. étudier les différents facteurs pouvant impliquer un retard au départ : + de 50% des vols partent légérement en avance, par contre, la moyenne des départs est égale à la valeur du 3ème quartile, ce qui indique que les vols en retard sont en général assez en retard
summary(df_departure$dep_delay)

#nom de l'aeroport ? type d'avion ? l'état ? la compagnie aérienne ?
df_departure_gb = df_departure %>%
  select(origin,dep_delay)%>%
  group_by(origin)%>%
  na.omit()%>%
  summarize(mean_dep_time = mean(dep_delay),
            median_dep_time = median(dep_delay))

View(df_departure_gb)
#EWR : Newark Liberty semble être l'aerport qui a le plus de retard avec 16 minutes de retard en moyenne juste devant EWR 
View(df_departure_gb)  

df_departure_gb_1 = df_departure %>%
  select(carrier,dep_delay)%>%
  group_by(carrier)%>%
  na.omit()%>%
  summarize(mean_dep_time = mean(dep_delay),
            median_dep_time = median(dep_delay))

#F9 Frontier Airlines , EV: Express Jet, YV : Mesa Airlines semblent être les compagnies avec le plus de retard avec une vingtaine de minutes de retard au départ
View(df_departure_gb_1)  

df_departure_gb_2 = df_departure %>%
  select(manufacturer,dep_delay)%>%
  group_by(manufacturer)%>%
  na.omit()%>%
  summarize(mean_dep_time = mean(dep_delay),
            median_dep_time = median(dep_delay))

#Agusta est le constructeur avec lequel les avions ont le plus de départ (à vérifier en terme de volume)
View(df_departure_gb_2)

df_departure_gb_3 = df_departure %>%
  select(month,dep_delay)%>%
  group_by(month)%>%
  na.omit()%>%
  summarize(mean_dep_time = mean(dep_delay),
            median_dep_time = median(dep_delay))
#Les périodes de grandes influences tel que le mois de juin et de juillet sont les mois avec les plus grand retard
View(df_departure_gb_3)

#Au final, la variable qui semble avoir le plus d'influence est le mois de départ : Agusta est également 

#2. Réaliser une jointure entre les différentes tables : Calculer les retards moyens et cumulés par Etat à l'arrivée des vols en provenance de NYC.
df_arrival_state = flights %>%
  left_join(states_airports,by=c("origin"="faa"))%>%
  select(STATE_NAME,arr_delay)%>%
  na.omit()%>%
  group_by(STATE_NAME)%>%
  summarize(mean_arr_delay = mean(arr_delay),
           sum_arr_delay = sum(arr_delay))
View(df_arrival_state)

#2 états : New Jersey et New York : NJ a un retard moyen plus élevé que NY
#3ème question carte


# Librairies 
library('rgdal')
library('classInt')
library("RColorBrewer")


# Contours
dep <- readOGR(dsn='maps/states_21basic',
               layer='states')
head(dep@data) #CODE_DEPT

# Merge
fusion <- merge(dep, df_arrival_state, by.x='STATE_NAME', by.y='STATE_NAME')
head(fusion@data)
fusion@data = fusion@data %>%
  na.omit()

liste = fusion@data$mean_arr_delay
liste=as.numeric(liste)
liste

colors <- c('#FFD1D1', '#FF8989')
plot(fusion, axes=F, col=colors[liste]) 
