# HIV IT

RStudio project for pulling ANC/PMTCT HIV testing data from KHIS/DHIS2 and estimating HIV incidence among ANC clients using HIV recency testing data.

The project is organized as two main scripts:

- `KHIS_AUTO_DATA_PULL.R` pulls and prepares KHIS/DHIS2 ANC HIV testing data.
- `HIV_INCIDENCE.R` combines KHIS ANC denominators with recency/LAg data and produces incidence reports.

## Project Structure

```text
.
├── HIV_IT.Rproj
├── KHIS_AUTO_DATA_PULL.R
├── HIV_INCIDENCE.R
├── output/
├── data_source/
├── documents/
└── logs/
```

## Workflow

Run the scripts in this order:

1. `KHIS_AUTO_DATA_PULL.R`
2. `HIV_INCIDENCE.R`

The KHIS script creates the intermediate analytics dataset used by the incidence script:

```text
output/analytics_latest.rds
```

The incidence script expects this file to exist unless an `analytics` object is already loaded in the R session.

## Script 1: KHIS Data Pull

`KHIS_AUTO_DATA_PULL.R` connects to KHIS/DHIS2, pulls ANC/PMTCT HIV testing indicators, reshapes them, calculates summary indicators, and exports downstream files.

It pulls both old and new MOH 731/711 data elements, including:

- ANC1 attendance
- known HIV-positive at first ANC
- ANC initial tests
- ANC positive results
- labour and delivery HIV testing
- postnatal care HIV testing

Key outputs:

```text
output/analytics_latest.rds
data_source/np_monthly.xlsx
logs/KHIS_LOG_<timestamp>.log
```

## Script 2: HIV Incidence Analysis

`HIV_INCIDENCE.R` pulls individual-level recency data from the recency API, standardizes key columns, classifies infections using the RITA algorithm, joins to KHIS ANC denominators, and estimates HIV incidence by county.

The implemented RITA logic is:

- LAg recent or ODn <= 1.5 and viral load >= 1000 copies/mL: `RECENT`
- LAg recent or ODn <= 1.5 and viral load < 1000 copies/mL: `LONGTERM`
- reported long-term labels: `LONGTERM`
- pending labels: `PENDING`

Incidence estimation uses `inctools::incprops()` with:

- MDRI: 130 days
- MDRI 95% CI: 118-142
- FRR: 0.00
- BigT: 365 days
- bootstrap iterations: 10,000
- high RSE threshold: 25%

Key outputs:

```text
documents/report_table_new_<timestamp>.xlsx
data_source/hivit_incidence.xlsx
```

The detailed workbook includes county results, a data dictionary, matched KHIS pre-ART VL sites, unmatched site checks, pooled recency-tested results, high-RSE summaries, and run metadata.

## Required Environment Variables

Create or update `.Renviron` in the project root with the required credentials and paths.

Minimum variables:

```text
DHIS2_USERNAME=your_khis_username
DHIS2_PASSWORD=your_khis_password
DHIS3_URL=https://your-dhis2-url/
RECENCY_API_KEY=your_recency_api_key
```

Optional variables:

```text
RECENCY_API_URL=https://eiddash.nascop.org/api/recency/lag
KHIS_BASE_DIR=.
EXTERNAL_DIR=./data_source
MFL_LOOKUP_PATH=documents/MFLCODE_FacilityExportMaterialList_26022026_Recent.xlsx
MFL_LOOKUP_CSV=documents/MFLCODE_KHIS_latest_26022026.xlsx.csv
INCIDENCE_REPORT_XLSX=documents/report_table_new.xlsx
PROJECT_RENVIRON=.Renviron
```

Optional SQL Server export variables:

```text
SQLSERVER_ENABLED=true
SQLSERVER_DRIVER=ODBC Driver 18 for SQL Server
SQLSERVER_SERVER=your_server_name_or_ip
SQLSERVER_DATABASE=your_database
SQLSERVER_UID=your_username
SQLSERVER_PWD=your_password
SQLSERVER_SCHEMA=dbo
SQLSERVER_ANALYTICS_TABLE=khis_analytics
SQLSERVER_NP_MONTHLY_TABLE=khis_np_monthly
SQLSERVER_TRUST_SERVER_CERTIFICATE=yes
```

When `SQLSERVER_ENABLED=true`, `KHIS_AUTO_DATA_PULL.R` writes the refreshed KHIS data to SQL Server only after the KHIS pull and transformations complete successfully. It opens a transaction, truncates the configured target tables, appends the new `analytics` and `np_monthly` data, then commits. If the data frames are empty or any database step fails, the transaction is rolled back.

Do not commit real credentials or API keys.

## R Packages

The scripts use packages including:

- `tidyverse`
- `dplyr`
- `tidyr`
- `purrr`
- `readxl`
- `readr`
- `writexl`
- `jsonlite`
- `httr`
- `stringr`
- `lubridate`
- `inctools`
- `pacman`

`KHIS_AUTO_DATA_PULL.R` uses `pacman::p_load()` to install/load many dependencies. `HIV_INCIDENCE.R` checks required packages and stops with a clear install message if one is missing.

## Notes

- The project depends on live KHIS/DHIS2 and recency API access.
- `HIV_INCIDENCE.R` restricts the HIV-negative incidence denominator to KHIS ANC sites that match pre-ART viral-load-contributing recency facilities by MFL code.
- County-level ANC1 HIV prevalence is calculated using all ANC1 KHIS sites.
- The simplified external incidence output contains only `County_Results` and `Data_Dictionary`.
- The `.RData` file is not required to understand the workflow, but it may contain saved objects from prior interactive R sessions.

## Typical Run

Open `HIV_IT.Rproj` in RStudio, confirm `.Renviron` is configured, restart R, then run:

```r
source("KHIS_AUTO_DATA_PULL.R")
source("HIV_INCIDENCE.R")
```

After completion, check `data_source/hivit_incidence.xlsx` for the main incidence report.
