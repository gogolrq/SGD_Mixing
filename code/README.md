 # Summary
This folder includes codes to reproduce the simulation results. The R script `myfun.R` defines the functions used in the experiments.

## Simulations:
Each subfolder contains an (empty) `data/` directory and several R scripts. `R_main_Modelx.R` or `R_main_Add_Modelx.R` is the main script, where `x` denotes the model number. `Loop_run.R` implements the for-loops to repeat the simulations. `collect.R` produces the corresponding figures (see `../output/README.md` for more details). We summarize the correspondence as follows.

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
The subfolders `Realdata_ols` and `Realdata_lad` contain an (empty) `data/` directory and several R scripts. `Realdata_ols/R_main_ols.R` and `Realdata_lad/R_main_lad.R` are the main scripts. `Loop_run.R` implements the for-loops to run the analysis under different settings. `collect.R` and `hist_bootstrap.R` are used to produce figures and tables (see `../output/README.md` for more details).


| Subfolder | Output |
|:------|:-----|
|  Realdata_ols |  Table 1, Figure 10   |
|  Realdata_lad  |  Table 2, Figure 11   |
