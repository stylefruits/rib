# Introduction to Ri͜b

Rib exists in order to minimise number of queries sent to JSON HTTP services.
It exposes an HTTP service providing a façade for another service. Rib executes
batches of requests against the underlying service on client's behalf.

## Interface

Rib's API consists of a single endpoint: `POST /v1/batch`. Requests to the
endpoint must contain a JSON-encoded body matching a following schema.

```json
[
  {
    "uri": Uri,
    "method": Method,
    "name": Name
  }
]
```

where

 - `Uri` is a string with a path and query string,
 - `Method` is a lower case string with an arbitrary HTTP method,
    e.g. `"get"` or `"delete"`, and
 - `Name` is a string with a name identifying the particular request.

The response matches a following schema.

```json
{
  "time_taken": TimeTotal,
  "results": [
    {
      "request": {
        "uri": Uri,
        "method": Get,
        "name": Name
      },
      "response": {
        "time_taken": Time,
        "status": Status,
        "headers": Headers,
        "body": Body
      }
    }
  ]
}
```

where

 - `TimeTotal` is the number of milliseconds of wall time it took
   to execute the entire batch,
 - `Uri` is the path sent in the request after applying dependency rules
    (see _Dependant requests_ below),
 - `Method` is the method sent in the request,
 - `Name` is the name of the request,
 - `Time` is the number of milliseconds of wall time it took
   to execute the request,
 - `Status` is a number representing the HTTP status of the response,
 - `Headers` is an object with headers of the response, and
 - `Body` is an object with the body of the response.

Rib can compress its responses with gzip. Responses to batched requests contain
a lot of duplicate information, e.g. identical keys in JSON objects or
reappearing headers. This makes compression an effective way to reduce amount of
transfered data.

To enable compression send your `POST /v1/batch` request with the
`Accept-Encoding` header set to `gzip`.

### Dependant requests

Rib allows you to execute batches with requests which depend on some
other requests in the same batch. Specifically, `Uri` of any request sent in
a batch can reference a value in a JSON document fetched in a prior request in
the same batch.

To introduce such a relation `Uri` of the dependant request must contain a
substring matching the following pattern:

    {result=NAME:JSONPATH}

where

  - `NAME` is a name identifying another request in the batch, and
  - `JSONPATH` is a [JSONPath][jp] expression.

The JSONPath expression will be evaluated in context of the JSON document
fetched by the request with the given name.

If the JSONPath expression returns a string or a numeric value the
`{result=...}` substring in the dependant request will be replaced with
this value.

If the JSONPath expression returns an array of strings or numeric values
the dependant request will be executed for every element of the array, with the
same string replacement rule as above. All resulting responses will be returned.

If the JSONPath expression returns a value which doesn't match rules above
the behaviour is undefined (but it should return a 500).

[jp]: http://goessner.net/articles/JsonPath/

## Tutorial

In this tutorial we'll assume that Rib is running on the local host on port
3000 and it's configured to run against the [GitHub API][gh]. Note that some
responses were redacted for the sake of brevity.

Let's start with a trivial example. We'll send an empty batch of queries.
The response is an empty array of results.

```
curl -s localhost:3000/v1/batch -d '[]'
```

```json
{
  "time_taken": 0.035,
  "results": []
}
```

Let's send a single query fetching a user profile.

```
curl -s localhost:3000/v1/batch -d '[{"uri": "/users/xsc", "method": "get"}]'
```

```json
{
  "time_taken": 126.93,
  "results": [
    {
      "response": {
        "time_taken": 126.514,
        "status": 200,
        "headers": {
          "cache-control": "public, max-age=60, s-maxage=60",
          "date": "Fri, 09 Oct 2015 14:57:26 GMT",
        },
        "body": {
          "login": "xsc",
          "id": 198072,
          "avatar_url": "https://avatars.githubusercontent.com/u/198072?v=3",
          "repos_url": "https://api.github.com/users/xsc/repos",
          "created_at": "2010-02-05T22:29:31Z",
          "updated_at": "2015-09-22T10:31:26Z"
        }
      },
      "request": {
        "uri": "/users/xsc",
        "name": "",
        "method": "get"
      }
    }
  ]
}
```

Formulating Rib requests in a command line becomes cumbersome quite quickly.
We can make it easier by putting the batch in a file. Let's call it `batch.js`.

```json
[
  {"uri": "/users/xsc", "method": "get"}
]
```

We execute the request in a following fashion.

```
curl -s localhost:3000/v1/batch -d @batch.js
```

Let's fetch more than a single user in a single request

```json
[
  {"uri": "/users/xsc", "method": "get"},
  {"uri": "/users/Leonidas-from-XIV", "method": "get"}
]
```

The result is

```json
{
  "time_taken": 124.601,
  "results": [
    {
      "response": {
        "time_taken": 121.747,
        "status": 200,
        "headers": { ... },
        "body": {
          "login": "Leonidas-from-XIV",
          "id": 121531,
          "repos_url": "https://api.github.com/users/Leonidas-from-XIV/repos",
          ...
        }
      },
      "request": {
        "uri": "/users/Leonidas-from-XIV",
        "name": "",
        "method": "get"
      }
    },
    {
      "response": {
        "time_taken": 124.018,
        "status": 200,
        "headers": { ... },
        "body": {
          "login": "xsc",
          "id": 198072,
          "repos_url": "https://api.github.com/users/xsc/repos",
          ...
        }
      },
      "request": {
        "uri": "/users/xsc",
        "name": "",
        "method": "get"
      }
    }
  ]
}
```

Notice that the total execution time is shorter than the sum of execution time
of all requests; Independent requests are executed in parallel.

Let's fetch xsc's repositories in order to show how dependant requests work in
Rib.

```json
[
  {"uri": "/users/xsc", "method": "get", "name": "user"},
  {"uri": "{result=user:$.repos_url}", "method": "get"}
]
```

This returns

```json
{
  "time_taken": 421.332,
  "results": [
    {
      "response": { ... },
      "request": {
        "uri": "/users/xsc",
        "name": "user",
        "method": "get"
      }
    },
    {
      "response": {
        "time_taken": 287.773,
        "status": 200,
        "headers": { ... },
        "body": [
          {
            "id": 10091725,
            "name": "pandect",
            "full_name": "xsc/pandect",
            "owner": {
              "login": "xsc",
              "id": 198072,
              ...
            },
            "languages_url": "https://api.github.com/repos/xsc/pandect/languages",
            "merges_url": "https://api.github.com/repos/xsc/pandect/merges",
            "size": 1805,
            "stargazers_count": 133,
            "watchers_count": 133,
            "default_branch": "master"
          },
          ...
        ]
      },
      "request": {
        "uri": "/",
        "name": "",
        "method": "get"
      }
    }
  ]
}
```

In the body of the second response we get an array of repositories.
Let's fetch a list of languages for all these repositories. We'll use
Rib's dependant requests.

```json
[
  {"uri": "/users/phalphalak", "method": "get", "name": "user"},
  {"uri": "{result=user:$.repos_url}", "method": "get", "name": "repos"},
  {"uri": "{result=repos:$[*].languages_url}", "method": "get", "name": "lang"}
]
```

which results in

```json
{
  "time_taken": 1950.13,
  "results": [
    {
      "response": { ... },
      "request": {
        "uri": "/users/phalphalak",
        "name": "user",
        "method": "get"
      }
    },
    {
      "response": { ... },
      "request": {
        "uri": "/users/phalphalak/repos",
        "name": "repos",
        "method": "get"
      }
    },
    {
      "response": {
        "time_taken": 135.29,
        "status": 200,
        "headers": { ... },
        "body": {
          "Clojure": 1504
        }
      },
      "request": {
        "uri": "/repos/phalphalak/.lein/languages",
        "name": "lang",
        "method": "get"
      }
    },
    {
      "response": {
        "time_taken": 604.615,
        "status": 200,
        "headers": { ... },
        "body": {
          "Clojure": 7209,
          "Shell": 982
        }
      },
      "request": {
        "uri": "/repos/phalphalak/Minecraft-Cartographer/languages",
        "name": "lang",
        "method": "get"
      }
    },
    {
      "response": {
        "time_taken": 740.691,
        "status": 200,
        "headers": { ... },
        "body": {
          "Clojure": 1609608,
          "Java": 7284,
          "Emacs Lisp": 2080
        }
      },
      "request": {
        "uri": "/repos/phalphalak/OpenEngine/languages",
        "name": "lang",
        "method": "get"
      }
    },
    ...
  ]
}
```

[gh]: https://developer.github.com/v3/
