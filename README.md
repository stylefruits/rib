# Ri͜b: Requests in Batches

**tl;dr**: Rib executes batches of inter-dependent HTTP requests from your
clients to your JSON API, with automagical parallelisation built in.

## Problem

![Requests over a flaky network](https://zugspitzbahn.stylefruits.de/v1/images/058b363f-7a44-4a25-bcd4-31b7014a1c6f)

The functionality which your mobile applications offer to your users depends on
communication with a JSON API running in the cloud.
There's a lot going on on the wire; to render some views or offer some features
mobile clients need to send multiple requests to the API.
Some of those requests depend on successful execution of requests sent earlier.
Your mobile clients access the API over unreliable networks with high latency.
API responses can take a long time to reach your clients or just fail to arrive
at all.
This leads to unpredictability and bad user experience.

## Solution

![Requests through Rib](https://zugspitzbahn.stylefruits.de/v1/images/b8336bc7-6d29-41f2-959f-485b2da045fe)

Rib acts as a proxy between your mobile clients and the underlying JSON API.
It accepts batches of requests from clients and executes them on their behalf
as efficiently as possible.
Independent requests are executed concurrently.
Requests which depend on successful completion of other requests in the batch
are executed as soon as their requirements are met.

Rib improves reliability and decreases latency thanks to having a far better
connection to your API in comparison to your remote clients. In the best case
scenario it runs in the same network as the API.

Additionally, Rib decreases the overall network traffic to your mobile clients.
Rib sends batched responses to several requests in a single HTTP body.
This allows HTTP compression to achieve better ratio and in turn result
in leaner responses.

## Try it out!

Giving Rib a try will take less than five minutes.
It's as simple as pulling and running our lightweight Docker image.
Run it in front of your favourite JSON API and use curl to test it.

    docker run -e RIB_BACKEND=https://api.github.com \
      -d -p 3000:3000 --name rib stylefruits/rib
    curl -i http://docker:3000/v1/batch \
      -d '[{"uri": "/", "method": "get"}]'

Do you want to learn more about Rib's API?
[Take a look at the introductory guide.][doc]

[doc]: https://github.com/stylefruits/rib/blob/master/doc/intro.md

## Compiling and running

Install `rebar3` and

    rebar3 compile
    erl -pa ebin -pa _build/default/lib/*/ebin -noshell -s rib start

If the service is running give it a try using

    curl -s http://localhost:3000/v1/batch -d '[
      {"uri": "/", "method": "get"}
    ]'

Run tests with

    rebar3 eunit

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
