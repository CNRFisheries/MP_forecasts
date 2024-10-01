
# Short term forecast for Italian management plan 2024

This repo contains the code used to produce the biological results for
the scientific advice given for the redaction of the Italian management
plan in 2024.

## Data needed

- FDI catches (not provided)
- Summary of reference points by stock (provided: stocks_info.xlsx)
- stock objects (not provided)

## Type of stock assessment models used

- a4a

- ss3 (annual time step, seasonal timestep, ensemble of annual timestep)

- spict

## Scripts and functions: how to replicate analysis

### Pre analysis 1: catch share

The script “catch_share.R” it is used to aggregate the FDI data and to
estimate the average proportion of the catches attributed to Italian
trawlers over the period 2020-2022. The output is the data file
“catch_share.csv” (provided).

### Pre analysis 2: forecasting ss3 models

Forecasts of ss3 models are done starting from the main ss3 objects (not
provided). In the script “forecast_creation_ss3” are reported all the
steps taken to create and to run the scenarios forecasts.

### Main analysis: global workflow

The script “global workflow” iterates over all the elements contained in
the stocks_info file. This code import and format data and stock
specific information, then it call the appropriate functions (depending
on the model used) to create, run and summarise the forecast. Finally it
creates images and tables.

### Model-specific functions (run within the global workflow)

The codes “forecast_a4a.R” and “forecast_spict.R” contains the code
needed to create, run and summarise the scenarios for the a4a and spict
models. The script “forecast_ss3.R” only contains the code to summarise
scenario results for the ss3 model.
