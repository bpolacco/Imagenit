FROM rocker/r-ver:3.5.2

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget


# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown'), repos='$MRAN')" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ 

RUN R -e "install.packages(c('shiny','shinydashboard','data.table','DT','ggplot2','RColorBrewer', 'shinyjs', 'promises', 'future', 'config'), repos='http://cran.rstudio.com/')"

#additional needed for map
RUN R -e "install.packages(c('leaflet', 'shinycssloaders'), repos='http://cran.rstudio.com/')"


RUN groupadd -g 5242 m342 && \
  useradd -u 75132 -g 5242 bpolacco


#set up files with access by limited permission server must run in    
RUN mkdir -p /var/log/shiny-server && chown bpolacco.m342 /var/log/shiny-server && \
    mkdir -p /etc/shiny-server && chown bpolacco.m342 /etc/shiny-server && \
    mkdir -p /srv/shiny-server && chown bpolacco.m342 /srv/shiny-server && \
    mkdir -p /var/lib/shiny-server && chown bpolacco.m342 /var/lib/shiny-server 

USER bpolacco

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /app /srv/shiny-server/
COPY /map /srv/shiny-server/map/

EXPOSE 5000

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]

