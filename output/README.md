 # Summary
This folder includes simulation results and codes to produce the figures and tables.

## Simulations:
Each subfolder contains a `data/` directory and an R script, `collect.R`. The `data/` directory contains simulation results (generated from the `../code/` folder) for each model. Run `collect.R` within each subfolder to generate the corresponding figures used in the paper. We summary the output as follows.

| Subfolder | Output |
|:------|:-----|
|  Model1  |  Figure 3-(1), Figure 5-(1)  |
|  Model2  |  Figure 3-(2), Figure 5-(2)  |
|  Model3  |  Figure 3-(3), Figure 5-(3)  |
|  Model4  |  Figure 3-(4), Figure 5-(4)  |
|  Model5  |  Figure 4-(1), Figure 6  |
|  Model6  |  Figure 4-(1), Figure 7  |
|  Model7  |  Figure 4-(1), Figure 8  |
|  Model8  |  Figure 4-(1), Figure 9  |
|  Additional_Simulation/Model1  |  Figure 12-(1)  |
|  Additional_Simulation/Model2  |  Figure 12-(2)  |
|  Additional_Simulation/Model3  |  Figure 12-(3)  |
|  Additional_Simulation/Model4  |  Figure 12-(4)  |
|  Additional_Simulation/Model5  |  Figure 13  |
|  Additional_Simulation/Model6  |  Figure 14  |
|  Additional_Simulation/Model7  |  Figure 15  |
|  Additional_Simulation/Model8  |  Figure 16  |


## Empirical Application
The subfolders `Realdata_ols` and `Realdata_lad` contain the analysis results (generated from the `../code/` folder) for the empirical application in the paper. The R scripts inside generate the figures and tables in the paper, which are summarized as follows.

| Script | Output |
|:------|:-----|
|  Realdata_ols/collect.R  |  Table 1  |
|  Realdata_ols/hist_bootstrap.R  |  Figure 10  |
|  Realdata_lad/collect.R  |  Table 2  |
|  Realdata_lad/hist_bootstrap.R  |  Figure 11  |
