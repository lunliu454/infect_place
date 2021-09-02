# Data and codes for *Infectiousness of places: The impact of human settlement and activity space in the transmission of COVID-19*
This repository stores the data and codes to replicate the analysis in *Infectiousness of places: The impact of human settlement and activity space in the transmission of COVID-19*
## Abstract
Places are fundamental factors in the spread of epidemics, as they are where people agglomerate and interact. This paper explores how different types of places—activity spaces at micro-level and human settlements at macro-level—impact the transmission of infections using evidences from COVID-19. We examine eleven types of activity spaces and find heterogeneous impacts across countries, yet we also find that non-essential activity spaces tend to have larger impacts than essential ones. Contrary to common beliefs, settlement size and density are not positively associated with reproduction numbers. Further, the impacts of closing activity spaces vary with settlement types and are consistently lower in larger settlements in all sample countries, suggesting more complex pattern of virus transmission in large settlements. This work takes first steps in systematically evaluating the epistemological risks of places at multiple scales, which contributes to knowledge in urban resilience, health and livability.
## Files recording the joint effects of activity spaces closures
jp_jointeffect_full.csv, uk_jointeffect_full.csv, us_jointeffect_full.csv, br_jointeffect_full.csv provide the estimated joint effects of closing any combination of activity spaces in our four sample countries.
## Codes
The codes for the analysis on the four sample countries are stored in four separate folders. In each folder\
***_basic_model.R** runs the basic two-way fixed-effect model on the causal impact of closing eleven types of activity spaces, and estimates the joint effects of closing multiple types of activity spaces, which produces *_jointeffect_full.csv.\
***_settlement_model.R** runs the regression model on the relationship between settlement characteristics and unit fixed effects.\
***_pretrend.R** checks the parallel trend assumption.
***_density_size.R** runs separate models on large and small as well as high-density and low-density settlements, examining the interaction between activity space closure and settlement type.\
***_robust_sample.R** checks the sensitivity of results to sample selection.\
***_robust_var.R** checks the sensitivity of results to including or excluding more independent variables.\
***_robust_density_size.R** checks the sensitivity of results to alternative indicators of settlement size and density (for Japan and US only).\
