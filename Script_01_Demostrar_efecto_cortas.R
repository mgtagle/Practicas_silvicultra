# Demostrar el papel de la corta silvícola en la estructura 
# Silvicultura
# Dr. Marco A González Tagle

# Datos -------------------------------------------------------------------

library(repmis) 
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")


# Análisis datos ----------------------------------------------------------

library(dplyr)
# Determinar el área basal y volumen existente

# AB= área basal es la superficie medida en m2 de la sección transversal de un árbol
# a la altura de pecho y se calcula con la formula pi*dap2

conjunto <- conjunto %>% 
  mutate(
    AB= round(0.7854*(Diametro/100)^2,4),
    Vol= round(AB*Altura*0.65,4))

# Representación gráfica --------------------------------------------------
library(ggplot2)
p1 <- ggplot(conjunto, aes(x=Diametro))+
  geom_histogram(bins=9, fill="#69b3a2", color="#e9ecef")+
  scale_x_continuous(limits=c(6, 24), expand = c(0, 0)) + 
  geom_vline(data=conjunto, aes(xintercept=mean(Diametro)),
             linetype="dashed")+
  theme_light()
p1


# Antes de la corta -------------------------------------------------------

pdf("Figuras/Antes_de_la_Corta.pdf", width = 8, height = 6)
vol.h <- hist(conjunto$Diametro, xaxt = "n", col="lightgreen",
              xlab="Diámetro (cm)",
              ylab="Frecuencia",
              ylim=c(0,14),
              main="Antes de la corta")
axis(1, vol.h$mids)
abline(v=mean(conjunto$Diametro), col="red", lwd=3, lty=2)
text(18, 13.5, "Media = 15.3", col = "#1b98e0",
     cex = 1)
dev.off()




# Selección de aclareo bajo ------------------------------------------------------


# <ELegir los colores para las barras del histograma
colores=c("#e69900", "#e69900", "#e69900", "#e69900", "lightgreen", "lightgreen",
          "lightgreen", "lightgreen","lightgreen")

pdf("Figuras/Selección_de_la_Corta.pdf", width = 8, height = 6)
vol.h <- hist(conjunto$Diametro, xaxt = "n", col=colores,
              xlab="Diámetro (cm)",
              ylab="Frecuencia",
              ylim=c(0,14),
              main="Antes de la corta")
axis(1, vol.h$mids)
abline(v=mean(conjunto$Diametro), col="red", lwd=3, lty=2)
text(18, 13.5, "Media = 15.3", col = "#1b98e0",
     cex = 1)
dev.off()


# Calcular volúmen por cat diamétrica -------------------------------------

cat <- vol.h$mids                       # Cat diamétricas
num <- vol.h$counts                     # número de árboles
CD <- data.frame(cat, num)              # Crear data frame
CD$AB <- round(0.7854*(CD$cat/100)^2,4) # Calcular área basañ
CD$ABt <- CD$AB*CD$num 
CD$cumsumAB <- cumsum(CD$ABt)
CD$Altura <- c(10, 8.7, 12, 12.3, 14.8, 13.8, 14.3, 16.7, 19.2)
CD$Vol <- round(CD$ABt*CD$Altura*0.65,4)
CD$cumsumVol <- cumsum(CD$Vol)

# Filtra por categorias
df2 <- conjunto %>% 
  filter(Diametro >=22 & Diametro <=24)
mean(df2$Altura)


# Remover 1 m3 de volumen por lo bajo

# Gráfica después de eliminar el primer m3 de vol son 11 árboles

Cd.AF <- conjunto %>% 
  filter(
    Diametro >= 14
  )

# <ELegir los colores para las barras del histograma
colores.Af=c("lightgreen", "lightgreen",
          "lightgreen", "lightgreen","lightgreen")

pdf("Figuras/Corta_Por_lo_Bajo.pdf", width = 8, height = 6)
vol.af <- hist(Cd.AF$Diametro, xaxt = "n", col="lightgreen",
               breaks = c(6,8,10,12,14,16,18,20,22,24),
              xlab="Diámetro (cm)",
              ylab="Frecuencia",
              ylim=c(0,14),
              xlim = c(7,24),
              main="Después de la corta\nAclareo por lo bajo")
axis(1, vol.af$mids)
abline(v=mean(Cd.AF$Diametro), col="red", lwd=3, lty=2)
text(20, 13.5, "Media = 17.2", col = "#1b98e0",
     cex = 1.2)
# curve(dnorm(x, mean=mean(Cd.AF$Diametro), sd=sd(Cd.AF$Diametro)), add=TRUE, col="darkblue", lwd=2) 
dev.off()


# Selección aclareo alto --------------------------------------------------

# <ELegir los colores para las barras del histograma
coloresf2=c( "lightgreen", "lightgreen",
          "lightgreen", "lightgreen","lightgreen", "lightgreen", "lightgreen","#e69900", "#e69900")

pdf("Figuras/Selección_de_la_Corta_Alta.pdf", width = 8, height = 6)
vol.f2 <- hist(conjunto$Diametro, xaxt = "n", col=coloresf2,
              xlab="Diámetro (cm)",
              ylab="Frecuencia",
              ylim=c(0,14),
              main="Selección de la corta")
axis(1, vol.f2$mids)
abline(v=mean(conjunto$Diametro), col="red", lwd=3, lty=2)
text(18, 13.5, "Media = 15.1", col = "#1b98e0",
     cex = 1)
dev.off()


# Aclareo por lo alto ---------------------------------------------------
# Remover 2m3 de volumen por lo alto

filtro2 <- conjunto %>% 
  filter(Diametro <=20 )

mean(filtro2$Diametro)

# <ELegir los colores para las barras del histograma
colores.Af=c("lightgreen", "lightgreen",
             "lightgreen", "lightgreen","lightgreen")

pdf("Figuras/Corta_Por_lo_Alto.pdf", width = 8, height = 6)
vol.f2 <- hist(filtro2$Diametro, xaxt = "n", col="lightgreen",
               breaks = c(6,8,10,12,14,16,18,20,22,24),
               xlab="Diámetro (cm)",
               ylab="Frecuencia",
               ylim=c(0,14),
               #xlim = c(7,24),
               main="Después de la corta\nAclareo por lo alto")
axis(1, vol.f2$mids)
abline(v=mean(filtro2$Diametro), col="red", lwd=3, lty=2)
text(18, 13.7, "Media = 15.1", col = "#1b98e0",
     cex = 1.2)
# curve(dnorm(x, mean=mean(Cd.AF$Diametro), sd=sd(Cd.AF$Diametro)), add=TRUE, col="darkblue", lwd=2) 
dev.off()
