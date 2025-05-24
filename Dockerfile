# ---- builder ----
FROM alpine:3.19 AS builder

RUN apk update
RUN apk add --no-cache curl gcc g++ git gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl tar xz zlib-dev zlib-static

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh

ENV PATH="/root/.ghcup/bin:/root/.cabal/bin:${PATH}"

WORKDIR /src/

# 依存だけ先に解決してキャッシュを効かせる
COPY *.cabal cabal.project* /src/
RUN cabal update && cabal build --only-dependencies

# ソース投入 & ビルド + インストール
COPY . /src
RUN cabal build --enable-executable-static -O2
RUN mkdir out/
RUN cp $(cabal -v0 list-bin exe:haskell-cicd --enable-executable-static -O2) out/

# ---- runtime ----
FROM scratch
COPY --from=builder /src/out/haskell-cicd /usr/local/bin/haskell-cicd
CMD ["/usr/local/bin/haskell-cicd"]
