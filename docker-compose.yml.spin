version: '2'
services:
  app:
    user: 75132:5242
    group_add:
    - shiny
    cap_drop:
    - ALL
    retain_ip: true
    image: registry.spin.nersc.gov/bpolacco/imagenit:0.3.1
    volumes:
    # mount data directories
    - /global/project/projectdirs/jgimg/babbitt/imagenit//data:/srv/shiny-server/data:ro
    - /global/project/projectdirs/jgimg/babbitt/imagenit//data-map:/srv/shiny-server/map/data:ro
    # mounting config files is optional, fully working config files are in images,
    # but we do it to allow for easy changing the config from the defaults
    - /global/project/projectdirs/jgimg/babbitt/imagenit//imagenit/app/config.yml:/srv/shiny-server/config.yml:ro
    - /global/project/projectdirs/jgimg/babbitt/imagenit//imagenit/map/config.yml:/srv/shiny-server/map/config.yml:ro
  web:
    image: registry.spin.nersc.gov/bpolacco/imagenit-nginx:latest
    ports:
    - "60000:8080"
    volumes:
    - /global/project/projectdirs/jgimg/babbitt/imagenit/web/nginx-proxy.conf:/etc/nginx/conf.d/default.conf:ro
    user: 75132:5242 
    group_add:
    - nginx
    cap_drop:
    - ALL

