# foreword

The mission behind this project is to experiment with a couple things:
1. Letting an LLM Agent drive writing the code for a project to get a workable API server
2. Familiarize myself with Haskell syntax within a familiar subject matter

I do not necessarily endorse the quality of this code! 

# ynab-categorizer

Categorizes unapproved YNAB transactions. Claude picks a category for each one, using `config/vendor-rules.yaml` as a prior for known payees. A run reports the choices. Pass `apply=true` to write the category, memo tag, and flag back to YNAB.

## Run locally

Requires GHC and Cabal. Copy `.env.example` to `.env` and set `YNAB_ACCESS_TOKEN`.

```sh
cabal run ynab-plans
```

Copy the plan id into `YNAB_PLAN_ID`, then set `ANTHROPIC_API_KEY` and `API_KEY`.

```sh
cabal run ynab-categorizer
```

The server listens on `127.0.0.1:3000`.

```sh
curl -s localhost:3000/health

curl -s -X POST -H "X-Api-Key: $API_KEY" localhost:3000/runs

curl -s -X POST -H "X-Api-Key: $API_KEY" "localhost:3000/runs?apply=true"
```

`POST /runs` is a dry run unless `apply=true`. Optional `sinceDate=YYYY-MM-DD` limits which unapproved transactions are included. Category names in the rules file must match the plan. Restart the server after editing that file.
