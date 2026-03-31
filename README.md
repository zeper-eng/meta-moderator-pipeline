# meta-moderator-pipeline

## Intro and disclaimer

The idea is that this repository contains a generalized and self-contained version of a pipeline I developed while working on a systematic review in an academic clinical research setting. All datasets and results have been removed or replaced, and no proprietary or unpublished research data is included!

The core of the pipeline is implemented inside of ```code/Analysis.R``` but, I also include an example execution inside of ```code/Runner.R``` and ```code/Convenience.R``` includes an examples of how reusable configuration objects were structured for the various iterations of our analysis.

## Background

This project stems from the analysis for one of the projects my current group is working on where we are exploring the effects of a range of interventions on early behavioral outcomes.

We conducted a systematic review using standard screening tools (Covidence) to identify and extract data from a broad set of studies. These studies were then synthesized using established meta-analytic methods and software (Cochrane). The included studies varied in design, population characteristics, and intervention delivery, allowing us to examine patterns across diverse contexts.

The results of our preliminary meta-analysis hinted that our studies had high heterogneity implying our effect sizes (Hedge's G/SMD) were significantly different across studies.

The meta regression is 


## 