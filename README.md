# migration_gravity

Code, data, presentation and paper for a within migration study in the Netherlands using a mixed effects multilevel model. 
The preliminary abstract of the paper is as follows:

## Housing market and migration revisited: a Bayesian multilevel gravity model for Dutch municipalities

By applying a Bayesian multilevel gravity model, this paper
  revisits the impact of home-ownership and social renting rates on
  intercity migration. Where most of the extant literatures focuses on
  using fixed effects for cities of origin and destination, I adopt a
  Bayesian multilevel approach. This approach has two main
  advantages. First, it allows for simultaneous estimation of city
  specific effects and the effects of city specific home-ownership and
  social renting rates on migration flows, where the impact is not
  necessarily symmetrical for cities of origin and
  destination. Second, it allows for prediction of migration flows
  between cities both in- and out-of-sample. The results show that
  home-ownership rates decrease migration flows significantly with an
  elasticity below $-1$. Municipal social renting rate has a negative
  impact as well, but its elasticity is close to zero. I use these
  estimates to predict changes in all in- and out-going migration flows in
  Amsterdam caused by a change in the home-ownership rate.

## Reproduction

To reproduce the results of the paper the following files have to run in order. Note, however that `analysis.brms` requires a considerable amount of time. Moreover, [Stan](https://mc-stan.org/) has to be installed to be able to run the commands from the wonderful `R`-package `brms`

1. `read_data.R`
2. `analysis_brms.R`
3. `make_maps.R`
4. `process_results.R`