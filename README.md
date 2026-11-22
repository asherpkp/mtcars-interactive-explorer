# 1974 Motor Trend: Horsepower vs. Fuel Efficiency

An interactive exploratory data visualization built in **R** using `tidyverse`, `ggplot2`, `plotly`, and `htmlwidgets`.

**[Launch Live Interactive Explorer](https://<your-username>.github.io/mtcars-interactive-explorer/)**

## Features
- **Multi-variable encoding:** Maps horsepower (`hp`), fuel economy (`mpg`), engine cylinders (`cyl`), and curb weight (`wt`) across all 32 vehicles in the `mtcars` dataset.
- **Interactive inspection:** Hover for exact road-test metrics or click any data point to fetch period-accurate 1974 vehicle photos and archival summaries via the Wikipedia API.
- **Zero-server deployment:** Compiled directly from R (`mtcars_app.R`) into a self-contained static web application (`index.html`).
