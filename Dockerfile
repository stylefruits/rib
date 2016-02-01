FROM msaraiva/erlang:17.5
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
  apk --update add git gcc g++ libstdc++ \
    erlang17-inets erlang17-syntax-tools erlang17-ssl erlang17-parsetools \
    erlang17-crypto  erlang17-erts erlang17-erl-interface erlang17-eunit \
    erlang17-dev erlang17-public-key erlang17-asn1 && \
  mkdir ebin && \
  rebar -r get-deps clean compile && \
  apk del git gcc g++ erlang17-dev && \
  rm -rf /var/cache/apk/* && \
  mkdir /opt/rib/config && \
  chown daemon /opt/rib/config
USER daemon
EXPOSE 3000
CMD \
  if [ -z "$RIB_BACKEND" ]; then \
    RIB_BACKEND=https://api.github.com; \
  fi && \
  echo "[{rib, [{port, 3000}, {backend, \"$RIB_BACKEND\"}]}]." \
    > config/demo.config && \
  exec erl -pa ebin -pa deps/*/ebin -noshell -config config/demo -s rib start
