# ---- builder ----
FROM haskell:9.4.8 AS builder
WORKDIR /src
COPY *.cabal /src/
RUN cabal update
COPY . /src
RUN cabal build --enable-executable-static -O2 all

# ---- runtime ----
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libgmp10
COPY --from=builder /src/dist-newstyle/build/x86_64-linux/ghc-9.4.8/haskell-cicd-0.1.0.0/x/haskell-cicd/build/haskell-cicd/haskell-cicd /usr/local/bin/haskell-cicd
CMD ["/usr/local/bin/haskell-cicd"]
