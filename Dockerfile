FROM msaraiva/erlang:18.1
RUN \
  hash=2d527b2a42e7c3a0c236de92d5ef951a5a27b462a1e8364e77928505e760bfd1 && \
  apk --update add curl && \
  curl -L https://s3.amazonaws.com/rebar3/rebar3 > /usr/bin/rebar3 && \
  echo $hash\ \ /usr/bin/rebar3 | sha256sum -c && \
  apk del curl && \
  rm -rf /var/cache/apk/* && \
  chmod +x /usr/bin/rebar3
ADD ./ /opt/rib
WORKDIR /opt/rib
RUN \
  mkdir -p config && chown daemon config && \
  apk --update add git gcc g++ libstdc++ \
    erlang-inets erlang-syntax-tools erlang-dev erlang-parsetools \
    erlang-crypto  erlang-erts erlang-erl-interface erlang-eunit \
    erlang-ssl erlang-public-key erlang-asn1 && \
  (test -d ebin || mkdir ebin) && \
  rebar3 compile && \
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
  erl -pa _build/default/lib/*/ebin -noshell -config config/demo -s rib start
