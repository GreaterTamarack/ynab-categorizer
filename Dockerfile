FROM haskell:9.8 AS build
WORKDIR /app
COPY receipt-crud.cabal Main.hs Handler.hs ./
RUN cabal update && cabal build exe:receipt-crud && cp "$(cabal list-bin exe:receipt-crud)" /receipt-crud

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends libffi8 libgmp10 libnuma1 && rm -rf /var/lib/apt/lists/*
COPY --from=build /receipt-crud /usr/local/bin/receipt-crud
EXPOSE 3000
CMD ["receipt-crud"]
