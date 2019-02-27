# Imagenit

In brief: an IMG/M metagenome analysis and exploration tool. PFAM and SFLD HMMs were matched across ~8000 metagenomes in HMM.  The results can be explored using these tools. 

For now, access docker images at:

[https://hub.docker.com/r/bpolacco/imagenit](https://hub.docker.com/r/bpolacco/imagenit)

[https://hub.docker.com/r/bpolacco/imagenit_map](https://hub.docker.com/r/bpolacco/imagenit_map)

If you have Docker installed, you can run these by:

```
docker run --rm --detach -p 80:80 bpolacco/imagenit:version0.11
docker run --rm --detach -p 8080:80 bpolacco/imagenit_map:version0.11

```

Then point your browser to 

[http://localhost](http://localhost)
and
[http://localhost:8080](http://localhost:8080)
