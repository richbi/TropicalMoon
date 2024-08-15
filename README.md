# TropicalMoon

This repository contains data and code accompanying article "The moon’s influence on the activity of tropical forest mammals" (Bischof et al. 2024, DOI: 10.1098/rspb.2024.0683). The data and code are also available on at https://doi.org/10.5061/dryad.kkwh70sbz.

The file vignette_LunarDielAnalysis.html provides step-by-step explanations and code for performing the analyses described in the article. The following additional files are provided in this repository and are required to run the code in the vignette:

functions.R

InputData1.RData

InputData2.RData

InputData3.RData

The data have been processed and are set up as inputs for 3 Bayesian multinomial models described in the article. Each InputData file (in native RData format) contains data objects (lists) required by the NIMBLE models run in the study. These objects are a data object (nimData), a constants objects (nimConstants), an initial values objects (nimInits), and a species information object (species.info). The core elements in the nimData object are counts of 15-minute intervals with unique detections (species at a camera trap). These are aggregated by protected area and species. Detections are further segregated into the diel and lunar periods they are associated with, depending on the analysis. The nimConstants objects contain values of constants (e.g., covariate values, dimensions, etc.), organized as list elements. The nimInits objects are empty: the models sample initial values from the prior distributions. The species.info objects include a data frame with taxonomic information for each species, alligned (rows) with the observation data in nimData objects associated with the respective analysis (1, 2, and 3).



