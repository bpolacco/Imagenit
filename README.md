# Imagenit

In brief: an IMG/M metagenome analysis and exploration tool. PFAM and SFLD HMMs were matched across ~8000 metagenomes in HMM.  The results can be explored using these tools. 

For now, access docker images at:

[https://hub.docker.com/r/bpolacco/imagenit](https://hub.docker.com/r/bpolacco/imagenit)

[https://hub.docker.com/r/bpolacco/imagenit_map](https://hub.docker.com/r/bpolacco/imagenit_map)

If you have Docker installed, you can run these by<sup>[1](#myfootnote1)</sup>:

```
docker run --rm --detach -p 80:80 bpolacco/imagenit:version0.11
docker run --rm --detach -p 1717:80 bpolacco/imagenit_map:version0.1

```


Then point your browser to 

[http://localhost](http://localhost)
and
[http://localhost:1717](http://localhost:1717)


<a name="myfootnote1">1</a>: If you have a web server or other service using ports 80 or 1717 the above commands will fail.  Simply change the 80: or 1717: to another random number and modify the urls below appropriately.
