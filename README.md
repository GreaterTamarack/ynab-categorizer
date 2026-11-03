

# ynab-categorizer

Categorizes uncategorized YNAB transactions. Claude picks a category for each one, using `config/vendor-rules.yaml` as a prior for known payees. A run reports the choices. Pass `apply=true` to write the category, memo tag, and flag back to YNAB.

## Foreword

The mission behind this project is to experiment with a couple things:
1. Letting an LLM Agent drive writing the code for a project to get a workable API server
2. Familiarize myself with Haskell syntax within a familiar subject matter

## Sample Result
Uncategorized transactions before:

<img width="620" height="137" alt="autocat-Test001" src="https://github.com/user-attachments/assets/c358fbcb-9ddc-40e2-b31b-a3dbb804292f" />

And after processed by a this tool, categorized by claude-haiku:

<img width="906" height="132" alt="image" src="https://github.com/user-attachments/assets/bbe9bbec-814a-44b1-92ae-d31e33590fb1" />


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

`POST /runs` is a dry run unless `apply=true`. Optional `sinceDate=YYYY-MM-DD` limits which uncategorized transactions are included. Category names in the rules file must match the plan. Restart the server after editing that file.
