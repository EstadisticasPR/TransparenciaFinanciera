# Pull shiny-server image
FROM rocker/shiny:latest

# Install R Packages dependencies
RUN apt update && apt install -y r-cran-curl \
    r-cran-openssl r-cran-xml2

# Install R Packages
RUN Rscript -e "install.packages(c('tidyverse','DT','plotly','data.table','rbokeh','lubridate','shinythemes'), repos='https://cran.rstudio.com/')"

# Shiny App in the current directory
COPY . /srv/shiny-server/TransparenciaFinanciera

# Config File
COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf
