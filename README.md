# Imagenit

# version 0.3
In brief: an IMG/M metagenome analysis and exploration tool. [Pfam](https://pfam.xfam.org/) and [SFLD](http://sfld.rbvi.ucsf.edu) HMMs were matched across ~8000 metagenomes in [IMG/M](https://img.jgi.doe.gov/).  The results can be explored using Imagenit tools.

Visit https://imagenit.jgi.doe.gov/ for live interactive tools.  [There  is a tutorial available on Google Docs](https://docs.google.com/document/d/1k6VjmhIgy6v9NJ3PKgAUi8aJdAa0k62cKZSZROUE0Sc/edit?usp=sharing)

Tools are packaged in a [docker container](https://hub.docker.com/r/bpolacco/imagenit) with separate data (see the *tar.gz files).

To run on your own machine, you must have [Docker](https://docs.docker.com/) installed and local copies of the data files.  Easiest if you have git and git-lfs installed is to clone the repository, expand the data directories, then use docker-compose to run

```
git lfs clone https://github.com/bpolacco/Imagenit
cd Imagenit
tar xzf data.tar.gz
tar xzf data-map.tar.gz
ln -s docker-compose.yml.local docker.compose.yml
docker-compose up
```

Then visit http://localhost in your browser. The above container stack includes an nginx web server at port 80 as a front end to the Shiny Server in the Imagenit container.  

Without git, or if you don't want to run the full stack above, download and expand just the *tar.gz files via the browser, then issue the following command.

```
docker run --rm --detach -p 5000:5000 -v /full/path/to/data:/srv/shiny-server/data:ro -v /full/path/to/data-map:/srv/shiny-server/map/data:ro  bpolacco/imagenit:0.3.0
```
Then visit http://localhost:5000
This requires port 5000 be available on your computer (which it probably is). If not, and you see an error about the port already used/allocated/bound, try a different port:

```
docker run --rm --detach -p 5050:5000 -v /full/path/to/data:/srv/shiny-server/data:ro -v /full/path/to/data-map:/srv/shiny-server/map/data:ro  bpolacco/imagenit:0.3.0
```
Then visit http://localhost:5050

Of course, you'll also need to replace /full/path/to/data with the actual path.  Something like the following on Mac OSX if your username is *benn*, and you leave the data files right where the browser downloads and expands them.

```
docker run --rm --detach -p 5050:5000 -v /Users/benn/Downloads/data:/srv/shiny-server/data:ro -v /Users/benn/Downloads/data-map:/srv/shiny-server/map/data:ro  bpolacco/imagenit:0.3.0
```

