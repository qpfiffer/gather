gather
=====

Installation
------------
CD into the directory, then:
```cabal install```

Running
-------
The cabal file will put gather in your path. To run it you need to specify a database file that it will use, like so. It should work with any of the supported KyotoCabinet file extensions, but I use the B+ Tree.
```gather -db /srv/data/links.kct```

Testing submission
------------------
This is designed to work with https://github.com/kyleterry/tenyks running the tenykslinkscraper service, but you can test it will good old curl.
```
curl -X POST -H "Content-Type: application/json" -d '{"url": "http://omfgdogs.com", "person":"December"}' http://localhost:8000
```
