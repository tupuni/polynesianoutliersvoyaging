FROM rocker/binder:4.0.3
LABEL maintainer='Aymeric Hermann <aymeric.hermann@cnrs.fr>'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN wget https://github.com/tupuni/polynesianoutliersvoyaging/raw/main/DESCRIPTION && R --vanilla -s -e "options(repos = list(CRAN = 'http://cran.rstudio.com'));  install.packages('renv'); renv::restore()"

RUN rm DESCRIPTION.1; exit 0
