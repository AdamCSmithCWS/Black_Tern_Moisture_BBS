# Black Tern trends and annual fluctuations

## Separating annual fluctuations in Black Tern bird abundance from longer-term non-linear patterns in population change

Using data from the North American Breeding Bird Survey for Black Tern and annual covariates of moisture/drought, we modeled annual fluctuations and long-term population change. 

We designed the model to estimate the effects of annual climate patterns on abundance of Black Terns observed during BBS surveys, while accounting for medium- and long-term population trends that were not associated with these climate patterns. We used two annual climate covariates: one that represented the local moisture conditions using the Standardized Precipitation Evapotranspiration Index (SPEI [@beguería]); and, a second that represented the North Atlantic Oscillation Index (NAOI, [@hurrell1995], accessed through https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/nao.shtml) from the preceding winter (January - June, on a 0-6 months before the BBS surveys were conducted). We fit three models: a base model that did not include climate data; a climate model that included the effects of SPEI and NAOI on the annual abundance; and a climate-plus-core model that also included the annual SPEI in the core of the species' range as a predictor on the annual abundance outside of the core. All of the models included spatially varying effects of the climate predictors and spatially varying population trends, so that these effects could vary across the species' range and so that information could be shared in a way that respects the geographic structure of the data [@thorson2023].

## Repo structure

The 5 R-scripts are ordered and provide the full code and data-download functions necessary to reproduce the analyses, tables, and figures in the paper. 

The Stan models are in the models folder
The covariate data are saved in the data folder
The cross_validation folder includes scripts necessary to run the k-fold cross validation summarized in supplemental table S1.




