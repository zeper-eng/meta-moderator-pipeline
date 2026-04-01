# meta-moderator-pipeline

## Intro and disclaimer

The idea is that this repository contains a generalized and self-contained version of a pipeline I developed while working on a systematic review in an academic clinical research setting. All datasets and results have been removed or replaced, and no proprietary or unpublished research data is included!

The core of the pipeline is implemented in```code/Analysis.R``` and example execution can be found inside of ```code/Runner.R```. Also contained inside ```code/Convenience.R``` are examples of how reusable configuration objects were structured for the various iterations of our analysis.

## Background

This project stems from the analysis for one of the projects my current group is working on where we are exploring the effects of a range of interventions on early behavioral outcomes.

We conducted a systematic review using standard screening tools (Covidence) to identify and extract data from a broad set of studies. These studies were then synthesized using established meta-analytic methods and software (Cochrane). The included studies varied in design, population characteristics, and intervention delivery, allowing us to examine patterns across diverse contexts. 

This inherent variability led us to design our meta-analysis under a random-effects model. The results indicated substantial heterogeneity, suggesting that effect sizes (Hedges’ g/SMD) varied meaningfully across studies.

To better understand this heterogeneity, we selected a set of moderators (predictors) relevant to study design and intervention characteristics. This motivated the development of a reusable pipeline for systematically evaluating moderator effects across multiple model specifications.

This repository contains a generalized version of that pipeline and is the result of that work. Feel free to adapt and use it as you please! It is intended both as a reusable tool for similar analyses and as a demonstration of my ongoing work in building reproducible, research-oriented data workflows in R. 
## 