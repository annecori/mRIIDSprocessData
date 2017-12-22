---
bibliography: 'ms6.bib'
date: '27 November, 2017'
subtitle: |
    A project funded through USAID’s “Combating Zika and Future Threats: A
    Grand Challenge for Development” program\
    Milestone 6: Increased complexity of the simple model to include data
    from Milestone 5
title: 'Mapping the Risk of International Infectious Disease Spread (MRIIDS)'
---

Milestone Description
=====================

Increased complexity of the simple model to include data from Milestone
5. Automated testing and validating procedures implemented where
possible. Explore procedures for multiple models comparison and
accounting for uncertainty.

General Approach
================

In Milestone 4, we presented a simple transmission model that made us of
historical case counts (data stream 1), information about the
transmissibility of the pathogen (data stream 2) and geographical
characetrisation (data stream 3) as a sole means to predict future risk.
We built on the simple model presented in Milestone 4 (ML4) to a more
complex model that integrates the information on multiple data streams
to provide:

-   estimate model parameters,

-   predict the regional/international spread of Ebola.

We have also established the procedure for the validation process using
historical data and are exploring various possibilities for multi-model
comparison.

Presentation of the model {#sec:model}
=========================

The number of cases at a location $j$ at time $t$ is given by the
equation
$$I_{j, t} \sim Pois\left( \sum_{i = 1}^{n} {\left( p_{i \rightarrow j}
  R_{t, i} \sum_{s = 1}^{t}{I_{i, t - s} w_{s}}\right)} \right),$$

where $R_{t, i}$ is the reproduction number at location $i$ at time $t$
and $p_{i \rightarrow j}$ is the probability of moving from location $i$
to location $j$. The quantity $R_{t, i}$ is the reproduction number at
time $t$ at location $i$. $R_{t, i}$ is affected by a number of other
factors e.g., the intrinsic transmissibility of a pathogen, the
healthcare capacity at location $i$ etc. Its dependance on these factors
is formalised as $$R_{t, i} := f(haq_i, R_0, t),$$ where $haq_i$ is an
index/score quantifying the healthcare capacity at $i$, $f$ denotes a
function and other symbols have their usual meaning.

The probability of moving between locations is derived from the relative
flow of populations between locations. This latter quantity is estimated
using a population flow model such as gravity model. Under gravity
model, the flow of individuals from area $i$ to area $j$,
$\phi_{i \rightarrow j}$, is proportional to the product of the
populations of the two areas, $N_i$ and $N_j$, and inversely
proportional to the distance between them $d_{i, j}$ raised to some
power $\gamma$. The probability of movement from location $i$ to
location $j$ is given by
$$p_{i \rightarrow j} = (1 - p_{stay}^i) r_{i \rightarrow j}^{spread},$$

where $p_{stay}^i$ is the probability of staying at location $i$ and
$r_{i \rightarrow j}^{spread}$ is the relative risk of spread to a
location $j$ from a focal location $i$. $r_{i \rightarrow j}^{spread}$
is quantified as
$$r_{i \rightarrow j}^{spread} = \frac{\phi_{i \rightarrow
  j}}{\sum_{x}{\phi_{i \rightarrow
  j}}}.$$

Statistical inference of model parameters
-----------------------------------------

The parameters of the full model as presented in Section \[sec:model\]
are: $R_{t, i}$, $p_{stay}$ and $\gamma$. The reproduction number
$R_{t, i}$ at time $t$ can be estimated from the incidence data for the
previous 3 to 4 weeks using the package EpiEstim ([@cori2013new]).

The other parameters can be estimated using maximum likelihood
estimation or estimating the posterior distribution of the parameters
using MCMC. Let the observed incidence time series be $o_1, o_2, \dots
o_t \dots $. Then the likelihood of the model parameters given the
observations is proportional to the probability of the data given model
parameters. The probability of $o_t$ at time $t$ given the model
parameters is: $$P(o_t \mid p_{stay}, \gamma, R_t) = e^-{\lambda_t}
  \frac{o_t^{\lambda_t}}{\lambda_t !},$$ where $\lambda_t$ is given by
$$\lambda_t = R_t \sum_{s = 1}^t{I_{s}w_{t - s}}.$$

Thus assuming that each observation is independent, the likelihood of
the parameters is proportional to
$$\prod_{t = 1}^{t}{e^-{\lambda_t} \frac{o_t^{\lambda_t}}{\lambda_t !}}.$$
In practice, we use past 2 to 3 weeks of incidence data to compute the
likelihood.

Multi-model Comparison
----------------------

We are exploring the use of well established statistical measures
ranging from simple such as likelihood ratio to the more sophisticated
information-theoretic measures such as AIC or DIC for multi-model
comparison. Another line of investigation for multi-model inference is
model avaeraging i.e., the predicted values are the weighted means of
the predictions from each model under consideration where the weights
are the model probabilities (see [@burnham2011aic]).

Predicting Future Incidence Pattern and Geographical Spread
-----------------------------------------------------------

Implementation Details
======================

Software Package mRIIDS
-----------------------

The general approach outlined above relies on several data streams, an
inference framework, and a framework for projection. The code developed
as part of the project will be made available as an open source R
package that will provide functions for pre-processing and collating the
various data streams as well as plug the data into modules that will do
the inference and the projection. The software package will eventually
be published on the R packages repository (CRAN). At the moment is
available on GitHub (github.com/annecori/mRIIDSprocessData).

\[fig:github\]

The package will include extensive documentation in the form of
user-friendly help files and vignettes.

Collating data for each data stream
-----------------------------------

\(a) \[on chain=1,join\] [Extract case counts]{}; (b) \[on chain=1,join\]
[Merge Duplicate Alerts]{}; (c) \[on chain=1,join\] [Remove outliers]{};
(d) \[on chain=1,join\] [Discard inconsistent data]{}; (e) \[on chain=2,
below = of d,join\] [Interpolate missing data]{}; (f) \[on
chain=2,join\] [Estimate Reproduction Number]{}; (g) \[on chain=2,join\]
[Determine probability of movement between locations]{}; (h) \[on
chain=2,join\] [Predict future incidence]{}; (d) – (e);

Figure \[fig:workflow\] summarises the steps involved in collating the
different data streams and in going from raw data to predictions. In
Milestone 6, a step was added to the data pre-processing workflow to
remove outliers from data. The removal of outliers was done using
Chebyshev Inequality with sample mean (see [@saw1984chebyshev]).
Figure \[fig:wf\_example\] illustrates the results of each step in the
pre-processing steps in the workflow.

![Illustration of the pre-processing steps on HealthMap incidence data
for
Liberia.[]{data-label="fig:wf_example"}](ms6-figures/liberia-preprocessing.png)

Model training and validation using data from WHO
-------------------------------------------------

In the current iteration, the model was trained and validated the data
on cases officially reported to the WHO during the 2013–2016 Ebola
outbreak in Guinea, Liberia and Sierra Leone. This dataset was cleaned
and published in [@garske20160308] and it is this cleaned version of the
data that were used in this work. This dataset consists of incidence
reports at ADM2 level. Thus in using it, we were able to better validate
the model at a finer spatial resolution than available with
HealthMap/ProMed data. We refer to this dataset as WHO data throughout
the rest of this document.

### Incidence trends from different data sources

We aggregated the WHO data to national level to compare the incidence
trends derived from the three different data sources (WHO, HealthMap and
ProMed). As can be seen in Figures \[fig:incid\_comp\] and
\[fig:r\_comp\], the three data sources correlate well both in the
incidence time series as well as the reproduction numbers estimated from
the incidence data.

[0.8]{} ![Comparison of the reproduction numbers estimated from the
different sources of the incidence
data.[]{data-label="fig:r_comp"}](ms6-figures/who_vs_hm_vs_pm-incid.png "fig:"){width="\textwidth"}

 

[0.8]{} ![Comparison of the reproduction numbers estimated from the
different sources of the incidence
data.[]{data-label="fig:r_comp"}](ms6-figures/who_vs_hm_vs_pm-R.png "fig:"){width="\textwidth"}

Inference of parameters
-----------------------

The parameters of the full model detailed in Section \[sec:\] are
$\alpha$, $\beta$, $\gamma$, $p_{stay}$ and $R_{i, t}$. The estimation
os $R_{i, t}$ uses incidence data and is done using the EpiEstim
package. In the interest of simplicity, we assume both $\alpha$ and
$\beta$ to be $1$. Thus the set of parameters is now $p_{stay}$ and
$\gamma$. We explored the influence of these two parameters on the
quality of fit of the model at various points in the epidemic. To assess
the goodness-of-fit, we used the normalised root mean squared error
(rms), which is the sum of squares of the differences between observed
and predicted values. That is,
$$rms := \sum_{i = 1}^n{\left(o_i - p_i\right)^2},$$ where $o_i$ is
$i$th observation, $p_i$ is the corresponding value predicted by the
model and $n$ is the total number of observations.

[0.4]{} ![Normalised root mean squared values as a function of the model
parameters. The fit is assessed for prediction of 5 weeks at 100 and 300
days from the start of the epidemic. The fit is better for smaller
values of the root mean sqaure error. In the early phase of the
epidemic, a better fit is obtained at a smaller value of $p_{stay}$
while at the 300 days mark, a much higher value of $p_{stay}$ is needed
to obtain a good
fit.[]{data-label="fig:rms"}](ms6-figures/rms-100-2.png "fig:")

 

[0.4]{} ![Normalised root mean squared values as a function of the model
parameters. The fit is assessed for prediction of 5 weeks at 100 and 300
days from the start of the epidemic. The fit is better for smaller
values of the root mean sqaure error. In the early phase of the
epidemic, a better fit is obtained at a smaller value of $p_{stay}$
while at the 300 days mark, a much higher value of $p_{stay}$ is needed
to obtain a good
fit.[]{data-label="fig:rms"}](ms6-figures/rms-300-2.png "fig:")

Output for Ebola in West Africa
===============================

Estimating Transmissibility
---------------------------

Predicting Future Cases
-----------------------

Connectivity and Risk of International Spread
---------------------------------------------

Parameters Inference
--------------------

\[initial exploration of how $p_{stay}$ and power, using rms\]

\[Formalization of the likelihood for joint estimation of R, $p_{stay}$,
power\]

This section should include figures

Predicting future incidence and geographical spread
---------------------------------------------------

\[present some early results – map of predictions/ incidence curve/
table at country level? \].

Conclusions and next steps {#sec:conclusions}
==========================

References {#references .unnumbered}
==========
