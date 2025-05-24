# ---- builder ----
FROM haskell:9.4.8 AS builder
WORKDIR /src

# 依存だけ先に解決してキャッシュを効かせる
COPY *.cabal cabal.project* /src/
RUN cabal update && cabal build --only-dependencies

# ソース投入 & ビルド + インストール
COPY . /src
RUN cabal install \
      --installdir=/opt/bin \
      --install-method=copy \
      --overwrite-policy=always \
      --enable-executable-static \
      -O2

# ---- runtime ----
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libgmp10 && rm -rf /var/lib/apt/lists/*
COPY --from=builder /opt/bin/haskell-cicd /usr/local/bin/haskell-cicd
CMD ["/usr/local/bin/haskell-cicd"]
