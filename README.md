## Package: yusufHAIGermany

<!-- badges: start -->
[![R-CMD-check](https://github.com/ETC5523-2025/assignment-4-packages-and-shiny-apps-yusufkurniar01/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ETC5523-2025/assignment-4-packages-and-shiny-apps-yusufkurniar01/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The **`yusufHAIGermany`** R package offers a reproducible simulation and visualisation framework for examining **Healthcare-Associated Infections (HAIs)** in Germany.  
It is inspired by the *Eurosurveillance* study by Zacher et al. (2019).  
*“Application of a new methodology and R package reveals a high burden of healthcare-associated infections (HAI) in Germany compared to the average in the European Union/European Economic Area, 2011 – 2012.”*  

This package generates **daily**, **weekly**, and **monthly** simulated datasets calibrated from the 2011 – 2012 ECDC PPS (Point Prevalence Survey) data.

This package can be accessed online, with the website explaining it, and the **shiny dashboard** can be reached via the **App** menu. Here is the link to access it:
https://etc5523-2025.github.io/assignment-4-packages-and-shiny-apps-yusufkurniar01/

---

## Installation

The package can be installed by running the latest version from GitHub:

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
- View **distribution analysis metric**, **heatmap analysis metric**, and **animated monthly analysis** 

---

## Datasets

The dataset is combination of orginal source and the simulated time series.
See the detail in: [articles/data-description.html](articles/data-description.html)

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

## Vignettes

Full documentation is provided in four articles:

- **Data description** — original PPS source and the three simulated tables; variables, units, and structure.  
  See: [articles/data-description.html](articles/data-description.html)
- **Get started**: required libraries, cleaning and simulation overview, and a quick tour of the data.  
  See: [articles/get-started.html](articles/get-started.html)
- **Examples**: practical recipes to filter, aggregate, and plot the data for analysis.  
  See: [articles/examples.html](articles/examples.html)
- **Germany HAI explorer**: a short analytical summary of the HAI Germany project and a link to the Shiny app.  
  See: [articles/germany-hai-explorer.html](articles/germany-hai-explorer.html)

To browse locally in R:

```r
browseVignettes("yusufHAIGermany")

# or open directly:
vignette("data-description", package = "yusufHAIGermany")
vignette("examples", package = "yusufHAIGermany")
vignette("germany-HAI-explorer", package = "yusufHAIGermany")
vignette("get-started", package = "yusufHAIGermany")
```

---

## Changelog

All updates are documented in [**NEWS.md**](news/index.html). 
The latest version is 0.1.3, which includes the most recent integrated dataset and an updated Shiny dashboard.

---

## License

- Package code © 2025 **Yusuf Romadhon**, licensed under **MIT License**.  
- Data source: *ECDC PPS 2011–2012*, licensed under **CC BY 4.0**.


---

## Credits

Developed for **ETC5523 – Communicating with Data** (Monash University, 2025).  
Instructor: **Michael Lydeamore** & **Maliny Po** 

**Built with:** `shiny`, `bslib`, `ggplot2`, `plotly`, `dplyr`, `tidyr`, `lubridate`, `scales`, `glue`, `yusufHAIGermany`.

**Developed with:** `devtools`, `usethis`, `roxygen2`, `pkgdown`, `knitr`, `rmarkdown`, `rsconnect`.

---

## Recommended Workflow

1. Install package: `pak::pak("ETC5523-2025/...")`  
2. Launch app: `launch_app()`  
3. View vignette: `browseVignettes("yusufHAIGermany")`  
4. Check changelog: `NEWS.md`  
5. View documentation via pkgdown site

---
