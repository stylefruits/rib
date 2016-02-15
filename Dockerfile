FROM msaraiva/erlang:18.1
RUN \
  hash=6b293837b430360feace5232edd582e0b979882b93e36c3de60c69b403d841ac && \
  apk --update add curl && \
  curl -L https://github.com/rebar/rebar/releases/download/2.6.0/rebar > /usr/bin/rebar && \
  echo $hash\ \ /usr/bin/rebar | sha256sum -c && \
  apk del curl && \
  rm -rf /var/cache/apk/* && \
  chmod +x /usr/bin/rebar
ADD ./ /opt/rib
WORKDIR /opt/rib
RUN \
  mkdir -p config && chown daemon config && \
  apk --update add git gcc g++ libstdc++ \
    erlang-inets erlang-syntax-tools erlang-dev erlang-parsetools \
    erlang-crypto  erlang-erts erlang-erl-interface erlang-eunit \
    erlang-ssl erlang-public-key erlang-asn1 && \
  (test -d ebin || mkdir ebin) && \
  rebar -r get-deps clean compile && \
  apk del git gcc g++ erlang-dev && \
  rm -rf /var/cache/apk/*
USER daemon
EXPOSE 3000
CMD \
  if [ -z "$RIB_BACKEND" ]; then \
    RIB_BACKEND=https://api.github.com; \
  fi && \
  echo "[{rib, [{port, 3000}, {backend, \"$RIB_BACKEND\"}]}]." \
    > config/demo.config && \
  exec erl -pa ebin -pa deps/*/ebin -noshell -config config/demo -s rib start
