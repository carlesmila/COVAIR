# Estimating daily air temperature and pollution in Catalonia: a comprehensive spatiotemporal modelling of multiple exposures

This repository contains the code of the paper "Estimating daily air temperature and pollution in Catalonia: a comprehensive spatiotemporal modelling of multiple exposures" by Carles Milà, Joan Ballester, Xavier Basagaña, Mark J. Nieuwenhuijsen and Cathryn Tonne. 

The main bulk of the analysis is structured in a `targets` pipeline which can be found in [this file](_targets.R). Therein, a number of scripts containing the functions implementing the different parts of the workflow are sourced at the beginning of the file and used within the pipeline. Packages needed to run the workflow can be checked in the pipeline file as well. 

The workflow is structured in three main parts:
1. Data pre-processing (find related code [here](R/prep/))
    1. Prepare boundaries and grids
    2. Prepare station data and report
    3. Prepare spatial predictors
    4. Prepare temporal predictors
    5. Prepare spatiotemporal predictors
    6. Prepare AOD/NO2/O3/LST remote sensing predictors and report
    7. Predictor report
2. Remote sensing imputation (find related code [here](R/RSimp/))
    1. Test/train splits 
    2. MODIS MAIAC and LST products: resample, merge, split
    3. TROPOMI products: resample, merge, split
    4. OMI: resample, merge, split
    5. Fit and validate RS imputation models (HPC)
    6. Imputation reports
3. Exposure modelling and prediction (find related code [here](R/prediction/))
    1. Focal predictors 
    2. Resample coarse predictors to target resolution  (HPC)
    3. Extract spatiotemporal predictor data at station locations (HPC)
    4. Temperature modelling: prepare data, explore, fit (HPC), predict (HPC)
    5. AP modelling: prepare data, explore, fit (HPC), predict (HPC)

The parts of the workflow that were done in a High Performance Computing (HPC) cluster can be found in a separate [folder](R/HPC/). Automatic reports summarising the results of each step can be found [here](reports/). The code to generate the tables and figures presented in the article and its appendix can be found in their respective [folder](R/tables&figures/).