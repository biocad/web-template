# web-template

This library encapsulates settings and error-catching for REST-services.

Convention, that are inside:

* every route has the following structure: `HOST:PORT/v{PATH VERSION}/PATH`;
* every path can be under authorization. Currently, there are cookie-based auth, OpenID-Connect auth and role-based auth on top of OpenID-Connect.

## Example

Look for example in `add/Main.hs` file. To run simple server on port 5000 just run:
```
stack build
stack exec web-template
```

Then you can ask server with curl requests:

* not processing path:
```
>>> curl localhost:5000/abracadabra
<h1>404: File Not Found!</h1>
```

* processing path with no authorization needed:
```
>>> curl localhost:5000/v1/ping
Pong!
Current environment: 0.%
```

* processing path with authorization without authorization:
```
>>> curl localhost:5000/v1/pong
{"error":"Authorization failed"}
```

* processing path with authorization with authorization:
```
>>> curl localhost:5000/v1/pong --cookie "id=0000-0000-0000-000000000000"
Ping!
Authorised: 0000-0000-0000-000000000000.
```

* example of throwing custom JSON error
```
>>> curl localhost:5000/v1/throw
["error",42]
```
