#GPU Analytics - Manufacturing Design Advisor


An interactive R Shiny dashboard for exploring a historical GPU dataset, predicting GPU specifications with machine learning, and sketching cooling and size requirements for a hypothetical GPU, including a 3D hologram of three cooling designs.

It has three areas: a **Home** page, a **Manufacturer** view for analysis and design, and a **Consumer** view for choosing a GPU.

## Features

**Manufacturer**
- **Visualizations:** trends by release year and manufacturer, distributions, and a correlation matrix.
- **Hypothetical Prediction:** a Random Forest model predicts a spec from the others, with test-set RMSE, MAE and R², feature importance, and a prediction range.
- **GPU Simulator:** enter core clock, memory, bus and power. The app predicts boost clock, bandwidth, cache and fill rates, gives a feasible / not-feasible verdict, and shows a rotating 3D hologram of three cooling systems (axial fans, vapor chamber + fans, liquid AIO).
- **Cooling & GPU Size:** recommends the smallest cooler that fits your power, card length, chassis and priority, plus the power connector and PSU size.

**Consumer**
- **Find by Needs:** filter GPUs by your requirements.
- **Recommender:** ranked GPU suggestions.
- **Compare GPUs:** side-by-side comparison.

## Getting started

Requires R and the packages below.

```r
install.packages(c("shiny", "tidyverse", "randomForest", "DT", "plotly", "htmlwidgets", "jsonlite"))
```

1. Put `All_GPUs.csv` in the same folder as the script.
2. Open the folder as your working directory, then run:

```r
shiny::runApp("project.R")
```

If the CSV is somewhere else, change `DATA_PATH` near the top of the script.

## Dataset

`All_GPUs.csv` has 3,406 GPUs with specs such as core clock, memory size, type and bus, power, connectors, display outputs and release date. It has **no** cooler, card dimension, fan or thermal data.

## Methods

- **Descriptive statistics:** grouped means and medians, density plots, a linear trend line, and a Pearson correlation matrix.
- **Performance index:** a 0 to 100 percentile score from memory size and core clock. It is relative, not a benchmark.
- **Prediction:** Random Forest regression (`randomForest`) with a random train/test split and a fixed seed. The "likely range" is the 10th to 90th percentile of the individual trees' predictions, not a formal confidence interval.

## Limitations

- The cooling recommendations, feasibility checks and hologram are **engineering rules of thumb**, not trained models or thermal simulations. The cooler sizes, capacities, noise levels and card shapes are illustrative. Validate any real design with proper thermal testing.
- Predictions are only as good as the data. Many fields are missing in the CSV, and the model can't extrapolate beyond the specs it has seen. The simulator warns when your inputs fall outside the dataset range.

## Built with

[Shiny](https://shiny.posit.co/), [tidyverse](https://www.tidyverse.org/), [randomForest](https://cran.r-project.org/package=randomForest), [DT](https://rstudio.github.io/DT/), [plotly](https://plotly.com/r/)
