# Ri͜b

**Requests In Batches**

A service acting as a proxy in front of an HTTP JSON API.
It allows sending batches of requests against the underlying API.
This leads to less roundtrips between clients and the cloud.

## Try it out!

    docker run -e RIB_BACKEND=https://api.github.com \
      -d -p 3000:3000 --name rib stylefruits/rib
    curl -i http://docker:3000/v1/batch \
      -d '[{"uri": "/", "method": "get"}]'

## Running

Install `rebar` and

    rebar get-deps compile
    erl -pa ebin -pa deps/*/ebin -noshell -s rib start

If the service is running give it a try using

    curl -s http://localhost:3000/v1/batch -d '[
      {"uri": "/", "method": "get"}
    ]'

Run tests with

    rebar compile eunit

## Coverage analysis

```erlang
cover:start().
Modules = cover:compile_directory("src").
[eunit:test(Mod) || {ok, Mod} <- cover:compile_directory("test")].
[cover:analyse_to_file(M, atom_to_list(M) ++ ".html", [html])
    || {ok, M} <- Modules].
```

## License

    Copyright 2016 stylefruits GmbH

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
