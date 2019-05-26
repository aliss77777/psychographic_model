# psychographic_model
This technique uses outputs from the Watson Personality Insights API to derive a predictive model for brand preference based on psychographic characteristics. This is done via dimensionality reduction on the full list of Watson characteristics and using those dimensions as features for a predictive model. This has the benefit of translating the rich, complex data of the Watson API into a succinct set of features that can be communicated more intuitively than the full data set. The output data can also be used for dynamic personalization of messaging and digital experiences (e.g. website functionality and content).

The data file consists of row-level data where most of the columns are features from the Watson API. In the course of running the model, you execute dimensionality reduction to reduce the API outputs down to 10 dimensions, then uses a logisticic regression and feature importance scores to identify what propensities are most significant to each brand. This model should be thought of as additive to other modeling techniques (e.g. behavioral, customer journey mapping, etc), because on its own it does not capture all the factors that would lead to a purchase decision.

d

This is a nuance technique with many steps - here's a breakdown of how to proceed:

Pre-processing: Data Collection via Watson Personality Insight API (not shown)
-- the tutorial script starts with a data file that has merged insights from a couple sources:
  - Personality Insights Watson API -- all data sources except Consumption Preferences (which have been remove)
  - For stability and reproducability, the data has been transformed to logical vectors. Any value of '1' indicates a percentile score of 90 or higher in the raw API output. And value of '0' indicates a percentile score below 90
  - this data can be pulled via the Watson API shown in the Aspiration Model repo and transformed to this format
  - there is also a column called 'Brand Flown' which is the target variable to predict psychographic propensities for. There are many ways to obtain this including via survey results or other source.

This repo has two branches, to account for the two ways this model can be run:

Approach A: Quick Route
- Loading the data set, appending psychographic dimensions via a pre-trained model, and running the logistic regression for brand preference

Approach B: Exploratory Route
- Loading the data set, running the factor analysis to explore the meanings of each dimension then running the logistic regression for brand preference
