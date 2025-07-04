# Title: Towards a formation model of the Neanderthal symbolic accumulation of herbivore crania in the Lozoya Valley: Spatial patterns shaped by rockfall dynamics in Level 3 of Des-Cubierta Cave

# Authors: Lucía Villaescusa, Enrique Baquedano, David M. Martín-Perea, Belén Márquez, M. Ángeles Galindo-Pellicena1, Lucía Cobo-Sánchez, Ana Isabel Ortega, Rosa Huguet, César Laplana, M. Cruz Ortega, Sandra Gómez-Soler, Abel Moclán, Nuria García, Diego J. Álvarez-Lao, Rebeca García-González, Laura Rodríguez, Alfredo Pérez-González, Juan Luis Arsuaga

# Rscript author: Lucía Villaescusa & Lucía Cobo-Sanchez

#--------------------------------------------------------------------------------
# 3rd SECTION OF THE ANALYSES: CLASTS DIVIDED BY SIZE 
#-------------------------------------------------------------------------------- 

# Libraries 

library(spatstat)
library(readxl)
library(ggplot2)
library(dplyr)
library(raster)
library(terra)

# 3.1. DATA UPLOADING ----

geo <- read_excel("05. CDC_clasts.xlsx")

mgeo <- as.factor(geo$`Size range 3`)

# Window in plan view

Window_plan <- read_excel("02. CDC_window_xy.xlsx")

WXY <- owin(poly=list(list(x=Window_plan$x, y=Window_plan$y)))

# Window in section view

Window_section <- read_excel("03. CDC_window_yz.xlsx")

WYZ <- owin(poly=list(list(x=Window_section$y, y=Window_section$z)))

# 3.2. PPP CREATION----
# 3.2.1. Section view----
sGEOm <- ppp(geo$y,
             geo$z, 
             window =WYZ,
             marks = mgeo)

sGEOm <- unique(sGEOm)
sGEOms <- split(sGEOm)

sLBt <- sGEOms$`Large boulder`
sMBt <- sGEOms$`Medium boulder`
sSBt <- sGEOms$`Small boulder`

layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = TRUE))
plot(sLBt, main = "Large boulder", pch= 18, cex= 3, cols = "black")
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2)
  plot(sMBt, main = "Medium boulder", pch= 18, cex= 1.5, cols = "tan")
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2)
  plot(sSBt, main = "Small boulder", pch= 16, cex= 0.75, cols = "tan4")
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2)
  par(mfrow=c(1,1)) #fig.5.a,b,c

# 3.2.2. Plan view----
GEOm <- ppp(geo$x,
            geo$y, 
            window =WXY,
            marks = mgeo)

GEOm <- unique(GEOm)
GEOms <- split(GEOm)

LBt <- GEOms$`Large boulder`
MBt <- GEOms$`Medium boulder`
SBt <- GEOms$`Small boulder`

layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = TRUE))
  plot(LBt, main = "Large boulder", pch= 18, cex= 3, cols = "black")
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2)
  plot(MBt, main = "Medium boulder", pch= 18, cex= 1.5, cols = "tan")
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2)
  plot(SBt, main = "Small boulder", pch= 16, cex= 0.75, cols = "tan4")
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2)
  par(mfrow=c(1,1)) # fig.6. a,b,c

# 3.3. MARKED PATTERN: CLASTS BY SIZE----

# 3.3.1. Section view----

# A. Intensity----

# A.1. Kernel density

#  Large boulders 

bsLBt <- bw.ppl(sLBt)
bsLBt*0.5
  
D1bsLBt <- density(sLBt, bw.ppl, adjust=0.5, positive=TRUE) 
 
plot(D1bsLBt, useRaster=FALSE, main="")
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2) # fig.5.d

# Medium boulders

bsMBt <- bw.ppl(sMBt)
bsMBt 

D1asMBt <- density(sMBt, bw.ppl, positive=TRUE)

plot(D1asMBt, useRaster=FALSE, main="")
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2) # fig.5.e

# Small boulders

bsSBt <- bw.ppl(sSBt)
bsSBt*0.5

D1asSBt <- density(sSBt, bw.ppl, adjust=0.5, positive=TRUE) 

plot(D1asSBt, useRaster=FALSE, main="")
  axis(1, 4530736:4530748)
  axis(2, 1108:1111)
  grid(lty=2) # fig.5.f

# A.2. Scan test 

# Large boulders

LR1sLBt<-scanLRTS(sLBt,r=bsLBt)
pvals1sLBt<-eval.im(pchisq(LR1sLBt,df=1,lower.tail=FALSE))

plot(pvals1sLBt<0.05, useRaster=F, main="", col= c(NA, "black"))
  axis(1, 4530736:4530748, cex.axis=1.2)
  axis(2, 1107:1111, cex.axis=1.2)
  plot(WYZ,add=T, border="black", lwd=1,25)
  grid(lty=2, lwd=0.5) # fig.5.g

# Medium boulders

LR1sMBt<-scanLRTS(sMBt,r=bsMBt)
pvals1sMBt<-eval.im(pchisq(LR1sMBt,df=1,lower.tail=FALSE))

plot(pvals1sMBt<0.05, useRaster=F, main="", col= c(NA, "tan"))
  axis(1, 4530736:4530748, cex.axis=1.2)
  axis(2, 1107:1111, cex.axis=1.2)
  plot(WYZ,add=T, border="black", lwd=1,25)
  grid(lty=2, lwd=0.5) # fig.5.h


# Small boulders

LR1sSBt<-scanLRTS(sSBt,r=bsSBt)
pvals1sSBt<-eval.im(pchisq(LR1sSBt,df=1,lower.tail=FALSE))

plot(pvals1sSBt<0.05, useRaster=F, main="", col= c(NA, "tan4"))
  axis(1, 4530736:4530748, cex.axis=1.2)
  axis(2, 1107:1111, cex.axis=1.2)
  plot(WYZ,add=T, border="black", lwd=1,25)
  grid(lty=2, lwd=0.5) # fig.5.i

# 3.3.2. Plan view----
# A. Intensity----
# A.1. Kernel density

# Large boulders
bLBt <- bw.ppl(LBt)
bLBt
  
D1aLBt <- density(LBt, bw.ppl, positive=TRUE)

plot(D1aLBt, useRaster=FALSE, main="") 
  axis(1, c(432076,432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736,4530738, 4530740,4530742, 4530744,4530746,4530747), cex.axis=0.75)
  grid(lty=2) # fig.6.d
  

# Medium boulders

bMBt <- bw.ppl(MBt)
bMBt

D1aMBt <- density(MBt, bw.ppl, positive=TRUE)
  
plot(D1aMBt, useRaster=FALSE, main="") 
  axis(1, c(432076, 432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736, 4530738, 4530740, 4530742, 4530744, 4530746, 4530747), cex.axis=0.75)
  grid(lty=2) # fig.6.e

# Small boulders

bSBt <- bw.ppl(SBt)
bSBt*0.5

D1aSBt <- density(SBt, bw.ppl, adjust=0.5, positive=TRUE)

plot(D1aSBt, useRaster=F, main="")  
  axis(1, c(432076, 432078, 432080, 432082), cex.axis=0.75)
  axis(2, c(4530736, 4530738, 4530740, 4530742, 4530744, 4530746, 4530747), cex.axis=0.75)
  grid(lty=2) # fig.6.f

# A.2. Scan test

# Large boulders

LR1LBt <- scanLRTS(LBt, r=bLBt)
pvals1LBt <- eval.im(pchisq(LR1LBt, df=1, lower.tail=FALSE))

plot(pvals1LBt < 0.05, useRaster=F, col= c(NA, "black"), main="")
  axis(1, c(432076,432078, 432080,432082), cex.axis=1.2)
  axis(2, c(4530736,4530738,4530740,4530742,4530744,4530746,4530747), cex.axis= 1.2)
  grid(lty=2)
  plot(WXY, add=T, border="black", lwd=1.25) # fig. 6.g

# Medium boulders

LR1MBt <- scanLRTS(MBt, r=bMBt)
pvals1MBt <- eval.im(pchisq(LR1MBt, df=1, lower.tail=FALSE))

plot(pvals1MBt < 0.05, useRaster=F,col= c(NA, "tan4") ,main="")
  axis(1, c(432076,432078, 432080,432082), cex.axis=1.2)
  axis(2, c(4530736,4530738,4530740,4530742,4530744,4530746,4530747), cex.axis=1.2)
  grid(lty=2)
  plot(WXY, add=T, border="black", lwd=1.25) # fig. 6.h

# Small boulders
LR1SBt <- scanLRTS(SBt, r=bSBt)
pvals1SBt <- eval.im(pchisq(LR1SBt, df=1, lower.tail=FALSE))

plot(pvals1SBt < 0.05, useRaster=F,col= c(NA, "tan"),  main="")
  axis(1, c(432076,432078, 432080,432082), cex.axis=1.2)
  axis(2, c(4530736,4530738,4530740,4530742,4530744,4530746,4530747), cex.axis=1.2)
  grid(lty=2)
  plot(WXY, add=T, border="black", lwd=1.25) # fig. 6.i

# KDE overlaid on the photogrammetry -----

plan_units_red <- "Plan_units_red.tif"
img_stack_red <- stack(plan_units_red)

def_col <- Kovesi$values[[29]]
def_col_trans <- to.transparent(def_col, fraction = 0.4) # defining transparency plot of kde medium boulder

plotRGB(img_stack_red, axes=F, colNA="white")
  plot(D1aMBt, col = def_col_trans, add=T)
  contour(D1aMBt, add=T, col="white", lwd=1.5) # fig. 6. j

# B. Density lines ----

ggplot(geo, aes(y=z, color = geo$`Size range 3`, fill = geo$`Size range 3`)) +
  geom_density(alpha=0.05) +
  scale_color_manual(values=c("black", "tan", "tan4"), labels=c("Large boulders", "Medium boulders", "Small boulders")) +
  scale_fill_manual(values=c("black", "tan", "tan4"), labels=c("Large boulders", "Medium boulders", "Small boulders")) +
  theme_bw() +
  labs(color=NULL, fill=NULL) # S.I. 4.b


