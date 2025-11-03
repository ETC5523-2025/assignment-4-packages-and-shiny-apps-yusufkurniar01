#Package: yusufHAIGermany

<!-- badges: start -->
[![R-CMD-check](https://github.com/ETC5523-2025/assignment-4-packages-and-shiny-apps-yusufkurniar01/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ETC5523-2025/assignment-4-packages-and-shiny-apps-yusufkurniar01/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The **`yusufHAIGermany`** R package provides a reproducible simulation and visualization framework for exploring **Healthcare-Associated Infections (HAIs)** in Germany.  
It is inspired by the *Eurosurveillance* study by Zacher et al. (2019),  
*“Application of a new methodology and R package reveals a high burden of healthcare-associated infections (HAI) in Germany compared to the average in the European Union/European Economic Area, 2011–2012.”*  
This package creates **daily**, **weekly**, and **monthly** simulated datasets calibrated from the 2011–2012 ECDC PPS (Point Prevalence Survey) data.

---

## Installation

You can install the latest development version from GitHub:

```r
# install.packages("pak")
pak::pak("ETC5523-2025/assignment-4-packages-and-shiny-apps-yusufkurniar01")
```

Or from source:

```r
remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-yusufkurniar01")
```

---

## Run the App

Launch the built-in **Germany HAI Explorer** directly from your R console:

```r
library(yusufHAIGermany)
launch_app()
```

### App Features
- Choose data frequency (**Monthly**, **Weekly**, or **Daily**)
- Explore **time-series trends** of simulated infections
- View **animated weekly bar race**, **daily heatmap**, and **monthly line chart**
- Download results for reproducible analysis

---

## Datasets

| Dataset | Description | Frequency | Data Type |
|----------|-------------|------------|-----------|
| `sim_daily` | Daily-level simulated infections and DALYs | Daily | `data.frame` |
| `sim_weekly` | Weekly aggregated simulations | Weekly | `data.frame` |
| `sim_monthly` | Monthly aggregated simulations | Monthly | `data.frame` |

### Variable Descriptions

| Variable | Description | Type | Used in Visualisation |
|-----------|-------------|------|------------------------|
| `date` | Observation date | Date | X-axis for time-based plots |
| `hai` | Infection type (UTI, HAP, SSI, BSI, CDI) | Factor | Color grouping |
| `cases` | Estimated cases | Numeric | Bar & line charts |
| `deaths` | Estimated deaths | Numeric | Bar charts & annotations |
| `dalys` | Disability-adjusted life years | Numeric | Heatmap or cumulative chart |
| `freq` | Frequency level | Character | Filter selector |

These simulations introduce mild seasonality and random variation to reflect uncertainty in real-world HAI burdens.

---

## Data Source

Original data reference:

> Zacher, B., Haller, S., Willrich, N., Walter, J., Abu Sin, M., Cassini, A., Plachouras, D., Suetens, C., Behnke, M., Gastmeier, P., Wieler, L. H., & Eckmanns, T. (2019).  
> *Application of a new methodology and R package reveals a high burden of healthcare-associated infections (HAI) in Germany compared to the average in the European Union/European Economic Area, 2011–2012.*  
> *Eurosurveillance, 24*(46). [https://doi.org/10.2807/1560-7917.ES.2019.24.46.1900135](https://doi.org/10.2807/1560-7917.ES.2019.24.46.1900135)

Data licensed under **Creative Commons Attribution 4.0 (CC BY 4.0)**.

---

## Example Usage

```r
library(yusufHAIGermany)

# Explore simulated monthly data
head(sim_monthly)

# Summarise DALYs by infection type
library(dplyr)
sim_monthly |>
  group_by(hai) |>
  summarise(total_dalys = sum(dalys))
```

---

## Vignette and Documentation

To generate the full vignette explaining simulation and visualisation methods:

```r
usethis::use_vignette("germany-hai-explorer")
devtools::build_vignettes()
```

Once built, view with:

```r
browseVignettes("yusufHAIGermany")
```

The vignette demonstrates:
- Simulation process  
- Professional time-series and animated charts  
- Shiny app walkthrough  

---

## Changelog

All updates are documented in [**NEWS.md**](./NEWS.md).  
Example format:

```markdown
# yusufHAIGermany 0.1.0
- Initial release: simulation datasets and Shiny dashboard

# Development version
- Added vignette “Germany HAI Explorer”
- Improved plot consistency and app layout
```

---

## License

- Package code © 2025 **Yusuf Romadhon**, licensed under **MIT License**.  
- Data source: *ECDC PPS 2011–2012*, licensed under **CC BY 4.0**.

Please cite both the original ECDC dataset and Zacher et al. (2019) when using derived results.

---

## Credits

Developed for **ETC5523 – Communicating with Data** (Monash University, 2025).  
Instructor: **Michael Lydeamore**  
Built with `tidyverse`, `lubridate`, `usethis`, and `shiny`.

---

## Recommended Workflow

1. Install package → `pak::pak("ETC5523-2025/...")`  
2. Launch app → `launch_app()`  
3. View vignette → `browseVignettes("yusufHAIGermany")`  
4. Check changelog → `NEWS.md`  
5. View documentation via pkgdown site

---

*Note: The dataset is combination of orginal source from the pdf and the simulated time series*
