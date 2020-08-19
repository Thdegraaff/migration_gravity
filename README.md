# migration_gravity

Code, data, presentation and paper for a within migration study in the Netherlands using a mixed effects multilevel model. 
The preliminary abstract of the paper is as follows:

## Housing market and migration revisited: a Bayesian multilevel gravity model for Dutch regions 

By applying a Bayesian multilevel gravity model, this paper revisits the impact
  of home-ownership and social renting rates on interregional migration. Where
  most of the extant literatures focuses on using fixed effects for regions of
  origin and destination, I adopt a Bayesian multilevel approach. This approach
  has two main advantages. First, it allows for simultaneous estimation of
  regional specific effects and the effects of regional specific home-ownership
  and social renting rates on migration flows, where the impact is not
  necessarily symmetrical for regions of origin and destination. Second, it
  allows for prediction of migration flows between regions both in- and
  out-of-sample. The results show that home-ownership rates decrease
  in-migration flows significantly with an elasticity aroud $-0.5$. However,
  both home-ownership and social renting increase regional out-migration flows
  to a large extent. The latter result might point to population been driven out
  of popular regions because of tight housing markets.

## Reproduction

To reproduce the results of the paper the following files have to run in order.
Note, however that `analysis.brms` requires a considerable amount of time.
Moreover, [Stan](https://mc-stan.org/) has to be installed to be able to run the
commands from the wonderful `R`-package `brms`

1. `read_corop.R`
2. `analysis_COROP.R`
3. `make_maps.R`
4. `process_results.R`

Note that there are more code files that are used for earlier analyses
(typically on a municipality level)
