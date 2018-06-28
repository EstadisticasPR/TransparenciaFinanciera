# Pull shiny-server image
FROM rocker/shiny:latest

# Install R Packages dependencies
RUN apt update && apt install -y libxml2-dev libcurl4-openssl-dev libssl-dev

# Install R Packages Dependencies
RUN Rscript -e "install.packages(c('Rcpp', 'stringi', 'htmltools', 'digest', 'glue'), dependencies=T)"

# Install R Packages
RUN Rscript -e "install.packages(c('shiny','tidyverse','DT','plotly','data.table','rbokeh','lubridate','shinythemes'), repos='https://cran.rstudio.com/', dependencies=T)"

# Shiny App in the current directory
RUN mkdir /srv/shiny-server/TransparenciaFinanciera/
COPY ./* /srv/shiny-server/TransparenciaFinanciera/

# Config File
COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf

#Start the server with the container
CMD ["/usr/bin/shiny-server.sh"]
