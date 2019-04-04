# Imagenit

# version 0.3
In brief: an IMG/M metagenome analysis and exploration tool. PFAM and SFLD HMMs were matched across ~8000 metagenomes in IMG.  The results can be explored using these tools.

Tools are packaged in a docker container with separate data (see the *tar.gz files).  Visit https://imagenit.jgi.doe.gov/ for live interactive tools.  [There  is a tutorial available on Google Docs](https://docs.google.com/document/d/1k6VjmhIgy6v9NJ3PKgAUi8aJdAa0k62cKZSZROUE0Sc/edit?usp=sharing)

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

This requires port 5000 be available on your computer (which it probably is). If not, try a different port:

```
docker run --rm --detach -p 5050:5000 -v /full/path/to/data:/srv/shiny-server/data:ro -v /full/path/to/data-map:/srv/shiny-server/map/data:ro  bpolacco/imagenit:0.3.0
```


# version 0.1
 

The tools and hosting are in development.  For now, access docker images at:

[https://hub.docker.com/r/bpolacco/imagenit](https://hub.docker.com/r/bpolacco/imagenit)

[https://hub.docker.com/r/bpolacco/imagenit_map](https://hub.docker.com/r/bpolacco/imagenit_map)

If you have [Docker](https://docs.docker.com/) installed, you can run these by<sup>[1](#myfootnote1)</sup>:

```
docker run --rm --detach -p 80:80 bpolacco/imagenit:version0.11
docker run --rm --detach -p 1717:80 bpolacco/imagenit_map:version0.1

```


Then point your browser to 

[http://localhost](http://localhost)
and
[http://localhost:1717](http://localhost:1717)



# version 0.2

This version, the first commit/push to github, has been built for hosting on NERSC's spin. To run locally, clone the repository, expand the data directories, then use docker-compose to run

```
git clone https://github.com/bpolacco/Imagenit
cd Imagenit
tar xzf data.tar.gz
tar xzf data-map.tar.gz
docker-compose up
```

I use git lfs to manage the large data files. You may need to install this extension to make this work--I haven't tested it without.

Then point your browser to<sup>[1](#myfootnote1)</sup> 
[http://localhost](http://localhost)
and
[http://localhost/map](http://localhost/map)


<a name="myfootnote1">1</a>: If you have a web server or other service using ports 80 or 1717 the above commands will fail.  Simply change the 80: or 1717: to another random number and modify the urls below appropriately.
