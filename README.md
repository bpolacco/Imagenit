# Imagenit

# version 0.3.1
In brief: an IMG/M metagenome analysis and exploration tool. [Pfam](https://pfam.xfam.org/) and [SFLD](http://sfld.rbvi.ucsf.edu) HMMs were matched across ~8000 metagenomes in [IMG/M](https://img.jgi.doe.gov/).  The results can be explored using Imagenit tools.

Visit https://imagenit.jgi.doe.gov/ for live interactive tools.  [There  is a tutorial available on Google Docs](https://docs.google.com/document/d/1k6VjmhIgy6v9NJ3PKgAUi8aJdAa0k62cKZSZROUE0Sc/edit?usp=sharing)

### Running on your own computer
Imagenit is written in [R](https://www.r-project.org/) using [Shiny](https://shiny.rstudio.com/). You can run it using a Docker image which contains all you need, or from within your local R environment.

#### Running in Docker ####

Tools are packaged in a [docker container with](https://hub.docker.com/r/bpolacco/imagenit-wdata) or [without](https://hub.docker.com/r/bpolacco/imagenit) embedded data (see the *tar.gz files).

To run on your own machine, you must have a recent [Docker](https://docs.docker.com/) installed.  The easiest is to use the docker image with embedded data with the following single command.

```
docker run --rm --detach -p 5000:5000 bpolacco/imagenit-wdata
```
Then visit http://localhost:5000 in your browser. If you see an error about the port not being available, change 5000:5000 to something else, such as 5001:5000, then update the url to http://localhost:5001 or whatever number you settled on.  Old versions of Docker may require that you connect to http://192.168.99.100:5000 instead.  

Advanced git and docker users could configure as they wish, and use local copies of the data files by cloning the repository, 
expanding the data directories, then use docker-compose to run:

```
git lfs install #confirm you have git-lfs ready, so that the download of the data files works
git clone https://github.com/bpolacco/Imagenit
cd Imagenit
tar xzf data.tar.gz
tar xzf data-map.tar.gz
mv docker-compose.yml.local docker.compose.yml
docker-compose up
```

The above container stack includes an nginx web server at port 80 as a front end to the Shiny Server in the Imagenit container. There are many different ways to run just the bpolacco/imagenit image, or even to use and modify the included Dockerfiles to build your own images.

#### Running in R ####

First download the required code and data.
```
git lfs install #confirm you have git-lfs ready, so that the download of the data files works
git clone https://github.com/bpolacco/Imagenit
cd Imagenit
tar xzf data.tar.gz
tar xzf data-map.tar.gz
```
Then start up your R environment from the parent Imagenit directory.
```
getwd()
# "/Path/to/where/I/git/cloned/Imagenit/"
library (shiny)
runApp(appDir="./imagenit/app")
#or
runApp(appDir="./imagenit/map")
```

Chances are good you will get errors about missing required packages. You can view the Dockerfiles for the required installations, or just copy and paste these lines from the Dockerfiles
```
install.packages(c('shiny','shinydashboard','data.table','DT','ggplot2','RColorBrewer', 'shinyjs', 'promises', 'future', 'config'), repos='http://cran.rstudio.com/')
install.packages('BiocManager'); BiocManager::install('qvalue', version = '3.8')
install.packages(c('leaflet', 'shinycssloaders'), repos='http://cran.rstudio.com/')
```
