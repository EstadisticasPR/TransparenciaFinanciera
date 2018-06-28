FROM rocker/tidyverse
#RUN apt-get install r-base r-base-dev
RUN apt update && apt install -y libxml2-dev libcurl4-openssl-dev libssl-dev

RUN R -e "install.packages(c('shiny'), repos = 'https://cran.rstudio.com/', dependencies = T)"
RUN R -e "install.packages(c('tidyverse','DT','data.table','rbokeh','shinythemes'),repos = 'https://cran.rstudio.com/', dependencies=T)"

RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.7.907-amd64.deb
RUN dpkg -i shiny-server-1.5.7.907-amd64.deb
RUN chmod 777 /srv/shiny-server
RUN mkdir /srv/shiny-server/TransparenciaFinanciera/
RUN mkdir /srv/shiny-server/TransparenciaFinanciera/data/
RUN mkdir /srv/shiny-server/TransparenciaFinanciera/www/

COPY *.R /srv/shiny-server/TransparenciaFinanciera/
COPY ./data/*.csv /srv/shiny-server/TransparenciaFinanciera/data/
COPY ./www/*.png /srv/shiny-server/TransparenciaFinanciera/www/
COPY shiny-server.conf /etc/shiny-server/


#Start the server with the container
CMD ["/usr/bin/shiny-server"]

