# EMTCT Investment Case Shiny App

## How to Run

1. Install R (≥ 4.1) from https://cran.r-project.org
2. Install required packages in R:

```r
install.packages(c("shiny", "shinydashboard", "DT", "ggplot2", 
                   "dplyr", "tidyr", "scales", "shinyWidgets"))
```

3. Run the app:

```r
shiny::runApp("path/to/emtct_shiny_app/app.R")
```

Or from RStudio: open `app.R` and click **Run App**.

## Model Description

Faithfully recreates the non-dynamic compartmental model from:
> *Zambia PMTCT investment case, Lancet Global Health, 2024*

The model tracks women of childbearing age through three reproductive 
compartments (Pregnant, Delivery, Breastfeeding) × five HIV health states 
(HIV-negative, HIV-infected/undiagnosed, Diagnosed, On ART, Virally Suppressed).

## Countries Included
- Zambia (base case, matches original paper)
- Kenya
- Mozambique
- Malawi
- Zimbabwe
- South Africa

All country parameters are sourced from UNAIDS 2022/2023 estimates, 
PEPFAR data, and national DHS reports. All inputs are editable in the UI.

## Interventions Modeled
1. Infant prophylaxis
2. ANC retesting (3 strategies)
3. PrEP for HIV-negative mothers
4. Mother support groups
5. Regimen shift (DTG / EFZ)
6. LTFU-minimizing interventions
7. Point-of-care viral load testing
