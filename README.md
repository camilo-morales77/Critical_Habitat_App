# Critical Habitat Assessment

## Run locally

Open this folder as an RStudio project and run `app.R`, or run
`shiny::runApp()` with this folder as the working directory.

## Deploy to shinyapps.io with GitHub Actions

The workflow at `.github/workflows/deploy-shinyapps.yml` deploys the app when
code is pushed to `main`. It can also be run manually from the repository's
**Actions** tab.

1. Create a shinyapps.io account and configure the app there once, or register
   an account through `rsconnect::setAccountInfo()` locally.
2. In the GitHub repository, open **Settings > Secrets and variables >
   Actions** and add these repository secrets:
   - `SHINYAPPS_ACCOUNT`: the shinyapps.io account name.
   - `SHINYAPPS_TOKEN` and `SHINYAPPS_SECRET`: the deployment credentials from
     shinyapps.io.
   - `SHINYAPPS_APP_NAME`: the app name to create/update on shinyapps.io.
3. Push this project to a GitHub repository on its `main` branch, then check
   the **Actions** tab for the deployment result.

The workflow installs the app's CRAN packages and spatial system libraries.
The Migratory Species panel also requires the optional `ebirdst` package and
an eBird Status & Trends access key; the access key is entered in the app and
is not needed for deployment.
