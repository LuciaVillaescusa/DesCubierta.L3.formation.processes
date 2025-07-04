# Title: Towards a formation model of the Neanderthal symbolic accumulation of herbivore crania in the Lozoya Valley: Spatial patterns shaped by rockfall dynamics in Level 3 of Des-Cubierta Cave

# Authors: Lucía Villaescusa, Enrique Baquedano, David M. Martín-Perea, Belén Márquez, M. Ángeles Galindo-Pellicena1, Lucía Cobo-Sánchez, Ana Isabel Ortega, Rosa Huguet, César Laplana, M. Cruz Ortega, Sandra Gómez-Soler, Abel Moclán, Nuria García, Diego J. Álvarez-Lao, Rebeca García-González, Laura Rodríguez, Alfredo Pérez-González, Juan Luis Arsuaga

# Rscript author: Lucía Villaescusa & Lucía Cobo-Sanchez

#--------------------------------------------------------------------------------
# SIXTH SECTION: CRANIA COMPLETENESS ----
#--------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#         Library uploading 
# ------------------------------------------------------------------------------

library(readxl)
library(spatstat)
library(sf) 

# 6.1. DATA UPLOADING ----
# Polygons in plan----

crania_shp <- st_read("09. CDC_crania_polygons.shp")

complet_colors <- c("#FF9999", "#99CCFF", "#99FF99", "#FFFF99")

crania_colors <- complet_colors[as.numeric(crania_shp$Complet.)]

# Points in section----

crania_complet <- read_excel("10. CDC_crania_points.xlsx")

m_crania <- as.factor(crania_complet$Completeness)


# Window in plan view

Window_plan <- read_excel("02. CDC_window_xy.xlsx")

WXY <- owin(poly=list(list(x=Window_plan$x, y=Window_plan$y)))

# Window in section view

Window_section <- read_excel("03. CDC_window_yz.xlsx")

WYZ <- owin(poly=list(list(x=Window_section$y, y=Window_section$z)))

# Crania PPP creation
sCRANIAm <- ppp(crania_complet$y, 
                crania_complet$z, 
                window = WYZ, 
                marks = m_crania)

# Overlay of crania polygons on cone geometry (plan)----
# upload middle boulders' data (if is not already in space) 

geo <- read_excel("05. CDC_clasts.xlsx")
mgeo <- as.factor(geo$`Size range 3`)

GEOm <- ppp(geo$x,
             geo$y, 
             window =WXY,
             marks = mgeo)

GEOm <- unique(GEOm)
GEOms <- split(GEOm)

MBt <- GEOms$`Medium boulder`

D1aMBt <- density(MBt, bw.ppl, positive=TRUE)

plot(D1aMBt, useRaster=FALSE, main="") 
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
plot(st_geometry(crania_shp), add=TRUE, col=crania_colors, border="black", lwd=0.5)
  grid(lty=2)
  plot(WXY, add=T, lwd=1) # fig.10.a

# Overlay of crania points on cone geometry (section)

sGEOm <- ppp(geo$y,
             geo$z, 
             window =WYZ,
             marks = mgeo)

sGEOm <- unique(sGEOm)
sGEOms <- split(sGEOm)

sMBt <- sGEOms$`Medium boulder`  
  
D1asMBt <- density(sMBt, bw.ppl, positive=TRUE)  

plot(D1asMBt, useRaster=FALSE, main="") 
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2)
plot(sCRANIAm, add=TRUE, 
     pch=21,            
     bg= c("#FF9999", "#99CCFF", "#99FF99", "#FFFF99"), 
     col="black",       
     lwd=1,             
     cex=1.5)  # fig.10.b



