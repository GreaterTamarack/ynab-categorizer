FROM haskell:9.8 AS build
WORKDIR /app
COPY ynab-categorizer.cabal Main.hs Handler.hs ./
RUN cabal update && cabal build exe:ynab-categorizer && cp "$(cabal list-bin exe:ynab-categorizer)" /ynab-categorizer

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends libffi8 libgmp10 libnuma1 && rm -rf /var/lib/apt/lists/*
COPY --from=build /ynab-categorizer /usr/local/bin/ynab-categorizer
EXPOSE 3000
CMD ["ynab-categorizer"]
