# Title: Towards a formation model of the Neanderthal symbolic accumulation of herbivore crania in the Lozoya Valley: Spatial patterns shaped by rockfall dynamics in Level 3 of Des-Cubierta Cave

# Authors: Lucía Villaescusa, Enrique Baquedano, David M. Martín-Perea, Belén Márquez, M. Ángeles Galindo-Pellicena1, Lucía Cobo-Sánchez, Ana Isabel Ortega, Rosa Huguet, César Laplana, M. Cruz Ortega, Sandra Gómez-Soler, Abel Moclán, Nuria García, Diego J. Álvarez-Lao, Rebeca García-González, Laura Rodríguez, Alfredo Pérez-González, Juan Luis Arsuaga

# Rscript author: Lucía Villaescusa & Lucía Cobo-Sanchez

#-------------------------------------------------------------------------------
# 4th SECTION OF THE ANALYSES: BONE REFIS. DENSITY
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#         Library uploading 
# ------------------------------------------------------------------------------

library(spatstat)
library(readxl)


# 4.1. DATA UPLOADING----

refit_lines_section <- read_excel("06. CDC_refit_ lines_yz.xlsx")

refit_lines_plan <- read_excel("07. CDC_refit_ lines_xy.xlsx")

# Window in plan view

Window_plan <- read_excel("02. CDC_window_xy.xlsx")

WXY <- owin(poly=list(list(x=Window_plan$x, y=Window_plan$y)))

# Window in section view

Window_section <- read_excel("03. CDC_window_yz.xlsx")

WYZ <- owin(poly=list(list(x=Window_section$y, y=Window_section$z)))

# 4.2. PSP CREATION----

# 4.2.1. Section view 

as<-refit_lines_section$y0
as<-as.vector(as)

bs<-refit_lines_section$z0
bs<-as.vector(bs)

cs<-refit_lines_section$y1
cs<-as.vector(cs)

ds<-refit_lines_section$z1
ds<-as.vector(ds)

sREFITLINESm <- psp(as,bs,cs,ds, window=WYZ, marks = refit_lines_section$Nrefit)
sREFITLINES <- psp(as,bs,cs,ds, window=WYZ)

plot(sREFITLINES, main="")
  plot(WYZ, lwd=1,add=T)
  axis(1, 4530736:4530747)
  axis(2, 1108:1111)
  grid(lty=2) # fig. 8.c

# 4.2.2. Plan view

a<-refit_lines_plan$x0
a<-as.vector(a)

b<-refit_lines_plan$y0
b<-as.vector(b)

c<-refit_lines_plan$x1
c<-as.vector(c)

d<-refit_lines_plan$y1
d<-as.vector(d)

REFITLINESm <- psp(a,b,c,d, window=WXY, marks =as.factor(refit_lines_plan$Nrefit))
REFITLINES <- psp(a,b,c,d, window=WXY)

plot(REFITLINES, main="")
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) # fig. 8.a

# 4.3. INTENSITY REFIT PATTERN ----

# 4.3.1. Section view

den_lines_s <- density(sREFITLINES, 0.5)

plot(den_lines_s, main="", useRaster=F)
  plot(WYZ, lwd=1,add=T)
  plot(sREFITLINES, lwd=0.5, col ="white",add=T)
  axis(1, 4530736:4530747)
  axis(2, 1108:1111)
  grid(lty=2) # fig. 8.d

# 4.3.2. Plan view

den_lines <- density(REFITLINES, 0.75)

plot(den_lines, main="", useRaster=F)
  plot(WXY, lwd=1,add=T)
  plot(REFITLINES, lwd=0.5, col ="white",add=T)
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) # fig. 8.b
