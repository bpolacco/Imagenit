# Imagenit

# version 0.3
In brief: an IMG/M metagenome analysis and exploration tool. [Pfam](https://pfam.xfam.org/) and [SFLD](http://sfld.rbvi.ucsf.edu) HMMs were matched across ~8000 metagenomes in [IMG/M](https://img.jgi.doe.gov/).  The results can be explored using Imagenit tools.

Visit https://imagenit.jgi.doe.gov/ for live interactive tools.  [There  is a tutorial available on Google Docs](https://docs.google.com/document/d/1k6VjmhIgy6v9NJ3PKgAUi8aJdAa0k62cKZSZROUE0Sc/edit?usp=sharing)

### Running on your own computer
Tools are packaged in a [docker container](https://hub.docker.com/r/bpolacco/imagenit) with separate data (see the *tar.gz files).

To run on your own machine, you must have a recent [Docker](https://docs.docker.com/) installed.  The easiest is to use the docker image with embedded data with the following single command.

```
docker run --rm --detach -p 5000:5000 bpolacco/imagenit-wdata
```
Then visit http://localhost:5000 in your browser. If you see an error about the port not being available, change 5000:5000 to something else, such as 5001:5000, then update the url to http://localhost:5001 or whatever number you settled.  Old versions of Docker may require that you connect to http://192.168.99.100:5000 instead.  

Advanced git and docker users could configure as they wish, and use local copies of the data files by cloning the repository, 
expanding the data directories, then use docker-compose to run:

```
git lfs install #confirm you have git-lfs ready to go
git clone https://github.com/bpolacco/Imagenit
cd Imagenit
tar xzf data.tar.gz
tar xzf data-map.tar.gz
mv docker-compose.yml.local docker.compose.yml
docker-compose up
```

The above container stack includes an nginx web server at port 80 as a front end to the Shiny Server in the Imagenit container. There are many options to run the just the bpolacco/imagenit image, or even to use and modify the included Dockerfiles to build your own images.
