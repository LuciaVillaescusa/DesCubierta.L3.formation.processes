FROM rocker/binder:4.2.3
LABEL maintainer='Lucía Villaecsusa <lucia.villaescusa.fernandez@gmail.com>'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN wget https://github.com/LuciaVillaescusa/DesCubierta.L3.formation.processes/raw/master/DESCRIPTION && R --vanilla -s -e "options(repos = list(CRAN = 'http://cran.rstudio.com'));  install.packages('renv'); renv::restore()"

RUN rm DESCRIPTION.1; exit 0
