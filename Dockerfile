FROM msaraiva/erlang:18.1
RUN \
  hash=de8846e03bd35f4987de2121401851224e1e7270bf93dbd77d92466b1006451d && \
  apk --update add curl && \
  curl -L https://github.com/erlang/rebar3/releases/download/3.1.0/rebar3 -o /usr/bin/rebar3 && \
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
