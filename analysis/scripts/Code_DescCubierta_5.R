# Title: Towards a formation model of the Neanderthal symbolic accumulation of herbivore crania in the Lozoya Valley: Spatial patterns shaped by rockfall dynamics in Level 3 of Des-Cubierta Cave

# Authors: Lucía Villaescusa, Enrique Baquedano, David M. Martín-Perea, Belén Márquez, M. Ángeles Galindo-Pellicena1, Lucía Cobo-Sánchez, Ana Isabel Ortega, Rosa Huguet, César Laplana, M. Cruz Ortega, Sandra Gómez-Soler, Abel Moclán, Nuria García, Diego J. Álvarez-Lao, Rebeca García-González, Laura Rodríguez, Alfredo Pérez-González, Juan Luis Arsuaga

# Rscript author: Lucía Villaescusa & Lucía Cobo-Sanchez

#-------------------------------------------------------------------------------
# FIFTH SECTION: BONE REFITS. CIRCULAR STATISTICS
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#         Library uploading 
# ------------------------------------------------------------------------------

library(readxl)
library(ggplot2)
library(circular)
library(CircStats)
library(stats)

# 5.1. DATA UPLOADING----
# 5.1.1. Total lines----

lines_refits<- read_excel("08. CDC_refits_circular_statistics.xlsx")
lines_refits <- as.data.frame(lines_refits)

angles<-lines_refits$Angle
angles_ct <- circular(angles, units="degrees", type="angles", zero=pi/2)

# 5.1.2. >= 20 cm lines 

long <- subset (lines_refits, lines_refits$Length>=0.20)
long <- as.data.frame(long)

angles_long <- long$Angle
angles_cl <- circular(angles_long, units="degrees", type="angles", zero=pi/2)

# 5.2. ROSE DIAGRAM----
# 5.2.1. Total lines 

ggplot(lines_refits, aes(x = Angle)) +
  geom_bar(stat='bin', bins=30, color ="black", fill = "grey") +
  scale_x_continuous(breaks = seq(0, 360-1, by=45), limits = c(0, 360), 
                     labels=c("N\n 0/360º","N-E\ 45º","E\n 90º","S-E\n 135º","S\n 180º","S-W\n 225º","W\n 270º","N-W\n 315º"))+
  scale_y_continuous(breaks = seq (0,8, by=1))+
  coord_polar(start = 0, direction = 1, clip = "on")+
  xlab("") + ylab("Count")+
  theme_bw() # fig. 8.e

# 5.2.2. Long lines

ggplot(long, aes(x = Angle)) +
  geom_bar(stat='bin', bins= 10, color ="black", fill = "grey") +
  scale_x_continuous(breaks = seq(0, 360-1, by=45), limits = c(0, 360), 
                     labels=c("N\n 0/360º","N-E\ 45º","E\n 90º","S-E\n 135º","S\n 180º","S-W\n 225º","W\n 270º","N-W\n 315º"))+
  scale_y_continuous(breaks = seq (0,8, by=1))+
  coord_polar(start = 0, direction = 1, clip = "on")+
  xlab("") + ylab("Count")+
  theme_bw() # fig. 8.g

# 5.3. KERNEL DENSITY ----
# 5.3.1. Total lines 

densitycirculart <- density(angles_ct, bw = 50, control.circular=list(units="degrees"))

plot(densitycirculart, xlim=c(-1.6,1), ylim=c(-1.6,1), 
     points.plot =T, points.pch=16, points.cex=0.5, 
     points.col="grey", col = 2,
     main="",
     template = "geographics") # fig.8.f

# 5.3.2. Long lines 

densitycircularl <- density(angles_cl, bw = 20, control.circular=list(units="degrees"))

plot(densitycircularl, xlim=c(-1.2,1.2), ylim=c(-1.2,1.2), 
     points.plot =T, points.pch=16, points.cex=0.5, 
     points.col="grey", col = 2,
     main="",
     template = "geographics")  # fig.8.h

# 5.4. STATISTICS ----
# 5.4.1. Total lines----

n_t <- length(angles) # number of observations

mean_vector_t <- mean.circular(angles_ct) # mean vector

r_t <- rho.circular(angles_ct) # length of mean vector

median_t <- median.circular(angles_ct) # circular median

kappa_t <- est.kappa(angles_ct) # concentration

cir_var_t <- 1 - r_t # circular variance

cir_std_dev_t <- sd.circular(angles_ct) # circular standar deviation

std_err_mean_t <- cir_std_dev_t / sqrt(n_t) # Standard Error of Mean

# Calculate 95% confidence interval for the mean direction
alpha_95_t <- 0.05
ci_95_radius_t <- qchisq(1 - alpha_95_t, df = 1) / (2 * n_t)
ci_95_lower_t <- mean_vector_t - ci_95_radius_t
ci_95_upper_t <- mean_vector_t + ci_95_radius_t

# Calculate 99% confidence interval for the mean direction
alpha_99_t <- 0.01
ci_99_radius_t <- qchisq(1 - alpha_99_t, df = 1) / (2 * n_t)
ci_99_lower_t <- mean_vector_t - ci_99_radius_t
ci_99_upper_t <- mean_vector_t + ci_99_radius_t

# Resultados
list(
  "Number of Observations" = n_t,
  "Mean Vector (µ)" = mean_vector_t,
  "Length of Mean Vector (r)" = r_t,
  "Median" = median_t,
  "Concentration (kappa)" = kappa_t,
  "Circular Variance" = cir_var_t,
  "Circular Standard Deviation" = cir_std_dev_t*180/pi,
  "Standard Error of Mean" = std_err_mean_t*180/pi,
  "95% Confidence Interval" = c(ci_95_lower_t %% 360, ci_95_upper_t %% 360),
  "99% Confidence Interval" = c(ci_99_lower_t %% 360, ci_99_upper_t %% 360)
)

# 5.4.2. Long lines ----

n_l <- length(angles_long) # number of observations

mean_vector_l <- mean.circular(angles_cl) # mean vector

r_l <- rho.circular(angles_cl) # length of mean vector

median_l <- median.circular(angles_cl) # circular median

kappa_l <- est.kappa(angles_cl) # concentration

kappa_l2 <- r_l * (2 - r_l^2) / (1 - r_l^2) # no me da igual 

cir_var_l <- 1 - r_l # circular variance

cir_std_dev_l <- sd.circular(angles_cl) # circular standard deviation

std_err_mean_l <- cir_std_dev_l / sqrt(n_l) # Standard Error of Mean

# Calculate 95% confidence interval for the mean direction
alpha_95_l <- 0.05
ci_95_radius_l <- qchisq(1 - alpha_95_l, df = 1) / (2 * n_l)
ci_95_lower_l <- mean_vector_l - ci_95_radius_l
ci_95_upper_l <- mean_vector_l + ci_95_radius_l

# Calculate 99% confidence interval for the mean direction
alpha_99_l <- 0.01
ci_99_radius_l <- qchisq(1 - alpha_99_l, df = 1) / (2 * n_l)
ci_99_lower_l <- mean_vector_l - ci_99_radius_l
ci_99_upper_l <- mean_vector_l + ci_99_radius_l

# Resultados
list(
  "Number of Observations" = n_l,
  "Mean Vector (µ)" = mean_vector_l,
  "Length of Mean Vector (r)" = r_l,
  "Median" = median_l,
  "Concentration (kappa)" = kappa_l,
  "Circular Variance" = cir_var_l,
  "Circular Standard Deviation" = cir_std_dev_l*180/pi,
  "Standard Error of Mean" = std_err_mean_l*180/pi,
  "95% Confidence Interval" = c(ci_95_lower_l %% 360, ci_95_upper_l %% 360),
  "99% Confidence Interval" = c(ci_99_lower_l %% 360, ci_99_upper_l %% 360)
)


# 5.5. TEST OF UNIFORMITY----
# 5.5.1. Total lines----

rayleigh.test(angles_ct)
kuiper.test(angles_ct)
watson.test(angles_ct)


# 5.5.2. Long lines ----

rayleigh.test(angles_cl)
kuiper.test(angles_cl)
watson.test(angles_cl)





