# This script make some graphs

library(climaemet)
library(dplyr)
library(ggplot2)

out <- read.csv("data/SevillaClimate.csv")
head(out)

#Quick plots

#Dayly data----
out$year <- as.numeric(out$year)

scatter.smooth(out$tmin ~ out$year, las = 1, col = "red")
abline(h = 0, lty = 2)

scatter.smooth(out$tmax ~ out$year, las = 1, col = "red")
abline(h = 40, lty = 2)

#dia más caliente
out[which(out$tmax > 45),] #1995!!

#days above 40
out$a40 <- ifelse(out$tmax > 40, 1, 0) 
sevilla_temp <- out %>%
  group_by(year) %>%
  summarize(a40 = sum(a40, na.rm = TRUE))
scatter.smooth(sevilla_temp$a40 ~ sevilla_temp$year, las = 1, col = "red")

#nights above 20
out$a25 <- ifelse(out$tmin > 20, 1, 0) #noche tropicas >20; torrida >25
sevilla_temp <- out %>%
  group_by(year) %>%
  summarize(a25 = sum(a25, na.rm = TRUE))
scatter.smooth(sevilla_temp$a25 ~ sevilla_temp$year, las = 1, col = "red")

#mean per year
sevilla_temp <- out %>%
  group_by(year) %>%
  summarize(temp = mean(tmed, na.rm = TRUE))
sevilla_temp$year <- as.numeric(sevilla_temp$year)
ggstripes(sevilla_temp, plot_title = "Sevilla Airport") +
  labs(subtitle = "(1950-2025)")
par(mfrow = c(2,2))
scatter.smooth(sevilla_temp$temp ~ sevilla_temp$year, 
               las = 1, col = "red", 
               xlab = "Año", ylab = "Temperatura media anual")
#abline(lm(sevilla_temp$temp ~ sevilla_temp$year))

#Plots for infografic
#1) incremento medio
coef(lm(sevilla_temp$temp ~ sevilla_temp$year))[2]*length(sevilla_temp$year)
mean(sevilla_temp$temp[1:20]) - mean(sevilla_temp$temp[55:75])
#more conservative to use ~1.5 mean increase.

#2) extreme temperatures.
out$fecha2 <- as.Date(out$fecha)
out$month <- format(out$fecha2, "%m")
  
sevilla_temp <- out %>%
  group_by(year) %>%
  summarize(temp = mean(tmed, na.rm = TRUE))
sevilla_temp$year <- as.numeric(sevilla_temp$year)

sevilla_temp <- out %>%
  group_by(year) %>%
  filter(month %in% c("07", "08")) %>%
  summarize(temp = mean(tmax, na.rm = TRUE))
sevilla_temp$year <- as.numeric(sevilla_temp$year)
scatter.smooth(sevilla_temp$temp ~ sevilla_temp$year, las = 1, col = "red",
                 xlab = "Año", ylab = "media temperaturas máximas julio y agosto")
mean(sevilla_temp$temp[1:10]) - mean(sevilla_temp$temp[65:75])
#to plot the matrix in Canva...
export <- matrix(nrow = 75, ncol = 75)
diag(export) <- sevilla_temp$temp
export[row(export) < col(export)] <- diag(export)[row(export)[row(export) < col(export)]]
export <- as.data.frame(export)
colnames(export) <- 1951:2025
export$year <- 1951:2025
#write.csv(export, file = "SevillaClimate_export.csv")
sevilla_temp[order(sevilla_temp$temp, decreasing = T),]

#dias por encima de40 grados
#days above 40
out$a40 <- ifelse(out$tmax > 40, 1, 0) 
sevilla_temp <- out %>%
  group_by(year) %>%
  summarize(a40 = sum(a40, na.rm = TRUE))
scatter.smooth(sevilla_temp$a40 ~ sevilla_temp$year, las = 1, col = "red",
               xlab = "Año", ylab = "días por encima de 40 grados")
#write.csv(sevilla_temp, "sevilla_temp.csv")
mean(sevilla_temp$a40[1:10]); mean(sevilla_temp$a40[65:75])
#noches torridas
out$a25 <- ifelse(out$tmin > 20, 1, 0) #noche tropicas >20; torrida >25
sevilla_temp <- out %>%
  group_by(year) %>%
  summarize(a25 = sum(a25, na.rm = TRUE))
scatter.smooth(sevilla_temp$a25 ~ sevilla_temp$year, las = 1, col = "red",
               xlab = "Año", ylab = "noches por encima de 20 grados")
#write.csv(sevilla_temp, "sevilla_temp.csv")
mean(sevilla_temp$a25[1:10]); mean(sevilla_temp$a25[65:75])
par(mfrow = c(1,1))

#incremento en la madruga
#sacamos las fechas
years <- 1950:2026
#Calcular el Domingo de Resurrección para cada año y restar 2 días (Viernes Santo)
fechas_madruga <- timeDate::Easter(years) - 2
vector_madruga <- format(fechas_madruga, "%Y-%m-%d")
ss <- subset(out, fecha %in% vector_madruga)
scatter.smooth(ss$tmed ~ ss$year, las = 1, col = "red")
#write.csv(ss[c("year", "tmed")], "sevilla_temp.csv")
mean(ss$tmed[1:20]); mean(ss$tmed[65:75])
tail(ss$tmax)

#Ahora la noche del pescadito
years <- 1950:2026
# 2. Calcular el Domingo de Resurrección de cada año
domingos_resurreccion <- timeDate::Easter(years)
# 3. Aplicar las reglas históricas de Sevilla usando un bucle o ifelse vectorial
fechas_pescaito <- as.Date(character(length(years)))
for (i in seq_along(years)) {
  domingo <- domingos_resurreccion[i]
  if (years[i] < 2017) {
    # Regla antigua: El segundo lunes después de Semana Santa (8 días después)
    fechas_pescaito[i] <- domingo + 8
  } else {
    # Regla moderna: El segundo sábado después de Semana Santa (6 días después)
    fechas_pescaito[i] <- domingo + 6
  }
}
vector_pescaito <- format(fechas_pescaito, "%Y-%m-%d")
#calculo
feria <- subset(out, fecha %in% vector_pescaito)
scatter.smooth(feria$tmed ~ feria$year, las = 1, col = "red")
#write.csv(feria[c("year", "tmed")], "sevilla_temp.csv")
mean(feria$tmax[1:20]); mean(feria$tmax[65:75])
tail(feria$tmed)
