# CarbonIMTA data and analysis

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24455744.svg)](https://doi.org/10.5281/zenodo.24455744)

Data and R script for the manuscript:

> **Greenhouse Gas Emissions and Carbon Budgets in Freshwater Asian Sea Bass and Hybrid Tilapia Ponds With and Without Duckweed Integration**  
> Zhao-Jun Yong, Wei-Jen Lin, Yao-Chen Lee, Shuenn-Der Yang, Hsing-Juh Lin  

## What's here

| Path | Description |
|------|-------------|
| `CarbonIMTA.R` | One script that builds every table and figure |
| `data/*.rds` | Processed data for the freshwater integrated and monoculture ponds, 2023 and 2024 |
| `output/` | Figures and tables written by the script (not tracked in git) |

## Running it

You need R (tested with 4.6.1) and:

- `aelab`, from GitHub: `remotes::install_github("yangshaojun/aelab")`
- CRAN packages: `tidyverse`, `openxlsx`, `ggpubr`, `patchwork`, `Hmisc`, `corrplot` (0.95), `see`, `extrafont`, `suncalc`, `lme4`, `lmerTest`, `vegan`, `cowplot`, `scales`
- Century Gothic registered with `extrafont` (run `extrafont::font_import()` once)

From the repository root:

```r
source("CarbonIMTA.R")
```

Everything lands in `output/`, named after the manuscript item:

| Files | Item |
|-------|------|
| `Table2_*`, `Table3_*`, `Fig1_*` to `Fig4_*` | Main text. Table 1 is made by hand from `info.rds`. |
| `TableS1_*` to `TableS6_*`, `FigS2_*` to `FigS6_*` | Supplement. Fig. S1 is a satellite image and diagram, so it isn't produced here. |

The partial RDA uses permutation tests. The script sets the seed, so the selected terms and Fig. 3 are reproducible.

## Data files

| File | Description |
|------|-------------|
| `do.rds` | Dissolved oxygen metabolism (GPP, ER, NEP) |
| `ghg.rds` | GHG fluxes (CO2, CH4, N2O) per closure |
| `ghg_co2e.rds` | GHG fluxes as CO2 equivalents |
| `ghg_zb_flux.rds` | Chamber closure table (slope, window, temperature, flux, CH4 ebullition split) behind Table S3 and the bubble-free CO2 flux |
| `doc.rds`, `poc.rds` | Dissolved and particulate organic carbon, DIC, nitrogen |
| `sa.rds`, `soc.rds` | Sediment accumulation; sediment organic carbon |
| `envr.rds` | Water quality (temperature, DO, pH, EC, turbidity, SS, chl a) |
| `nutrient.rds` | NH4, NO2, NO3, PO4 concentrations |
| `raw_budget.rds`, `budget.rds` | Carbon budget per sampling and per pond |
| `feed.rds`, `info.rds` | Feed carbon content; pond information (area, stocking, harvest, feed) |
| `pco2.rds` | One row per pond and sampling (82 rows): pCO2, air pCO2, ΔpCO2, wind, bubble-free CO2 flux, time-matched NEP |
| `pco2_air.rds` | Air CO2 (ppm, dry) per campaign and LI-COR analyzer |

Two steps happened before these files and are not in the repository:

- **GHG slopes** (`ghg`, `ghg_co2e`, `raw_budget`, `budget`, `ghg_zb_flux`) were fitted from the raw LI-COR LI-7810 (CH4/CO2) and LI-7820 (N2O) files, using air temperature for each closure. The raw files are not shared here.
- **pCO2** (`pco2.rds`) comes from PyCO2SYS (Humphreys et al. 2022) using pH, DIC and the freshwater carbonic acid constants of Millero (1979) at zero salinity. Air CO2 is the analyzers' stable readings between closures (30-s rolling SD within 3.5 ppm), with the medians of all analyzers averaged per sampling. It is converted to pCO2 with air pressure from the nearby weather station and water vapor pressure at water temperature (Weiss and Price 1980).

## Citation

Please cite the manuscript (not yet published) and this dataset (https://doi.org/10.5281/zenodo.24455744).

## License

CC BY 4.0. See `LICENSE`.
