# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    vim \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev
    # libprotobuf-dev \
    # libgeos-dev \
    # libjq-dev \
    # libgdal-dev \
    # libudunits2-dev \
    # libv8-dev \
    # protobuf-compiler

# RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.7.907-amd64.deb  
# RUN dpkg -i shiny-server-1.5.7.907-amd64.deb || true

# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stringr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('openxlsx', repos='http://cran.rstudio.com/')"

# copy the app to the image
COPY ./app /srv/shiny-server/app
# COPY countries_small.geojson /srv/shiny-server/
# COPY country_cases.csv /srv/shiny-server/
# COPY PantherCoords.csv /srv/shiny-server/

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
# CMD ["/usr/bin/shiny-server.sh"]
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app', host = '0.0.0.0', port = 3838)"]
