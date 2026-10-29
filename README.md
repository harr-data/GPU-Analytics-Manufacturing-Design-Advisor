#GPU Analytics - Manufacturing Design Advisor

Files:
- app.R: updated Shiny application
- All_GPUs.csv: source dataset
- www/: supplied GPU images (used by homepage if referenced there)

Run:
1. Open this folder as the RStudio working directory.
2. Install packages if needed: install.packages(c("shiny","tidyverse","randomForest","DT","plotly"))
3. Open app.R and click Run App.

New feature:
Manufacturer > Cooling & GPU Size lets you enter estimated board power, maximum card length, chassis size, and design priority. It recommends a preliminary cooling approach, fan count, and size class.

Important limitation:
The supplied All_GPUs.csv does not contain cooler-type labels, physical card dimensions, measured thermal data, or fan counts. Therefore this is a transparent heuristic advisor, not a trained ML prediction. To train a real classifier/regressor, add labeled data from GPU product specifications and validate recommendations with thermal simulations and physical prototypes. The output is a design starting point, not a manufacturing-ready specification.
