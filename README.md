# Simple restaurant service app
## Created with Elm

Requirements to run the project:
- `docker` 19.03.0+
- `docker-compose` 3.8+

### Development

Run project in dev environment by:

```
cd docker/development
docker-compose up
```

Add this entry to `/etc/hosts`

```
127.0.0.1	bolec.local
```

Then go to `http://bolec.local:5000`

Fill missing initial data by executing:

```
cd docker/development
docker-compose exec gastro-db sh
./db-init.sh
exit
```

### Production

Run project in prod environment by:

```
cd docker/production
docker-compose up
```

Fill missing initial data by executing:

```
cd docker/production
docker-compose exec gastro-db sh
./db-init.sh
exit
```
