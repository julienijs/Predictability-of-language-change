# Predictability of language change project
This repository contains the script and 2 datasets for the "Predictability of language change" project. As is already clear from the title, this project tries to answer the question *how predictable is language change?*

## General idea
It is most likely impossible to foresee when and where a particular language change will emerge (Weinreich et al.’s 1968 problem of actuation), but the propagation of a language change follows a fairly predictable trajectory. Language change follows an S-cure through time, that is best modelled by the logistisc equation. We can conclude that a change is real when the residuals of the logistic curve (the difference between the observed and predicted values) are small. In this project we take a closer look at these residuals and ask the following questions:

1. Are S-curves robust enough to be used for prediction and forecasting? 
2. Which variables will make the (standardized) residuals larger in the positive or negative direction in case of an ongoing change? 

In order to answer these questions we have build different models for 15 known binary language changes (new vs old variant), with the change (new or old variant) as the dependent variable and the decade (total of 15 decades) as the independent variable. For each change there are four models: one with all the data and three other models, each with some part of the data left out (first 5 decades, middle 5 decades or last 5 decades). We compare the residuals of these 3 partly blinded models to the residuals of the model with all the data to see how well the blind models are able to predict the missing decades.

## Data & code
Contains 2 datasets to try out the code. The other datasets can be downloaded from Zenodo. C-CLAMP corpus (Piersoul, De Troij & Van de Velde 2021).

WIP

## Results
WIP

![The hortative alternation_final](https://user-images.githubusercontent.com/107923146/212959306-be672e37-ef2f-44d2-aea2-fd0ad0d27d62.png)

![Interaction effect for window position and part of the curve](https://user-images.githubusercontent.com/107923146/213192154-970b962d-f9ec-4b46-abaf-44238d11ba94.png)


## References
- Piersoul, Jozefien, Robbert De Troij & Freek Van de Velde. 2021. 150 years of written Dutch: The construction of the Dutch Corpus of Contemporary and Late Modern Periodicals. Nederlandse Taalkunde 26(2). 171-194.
- Weinreich, Uriel, William Labov & Marvin Herzog. 1968. Empirical foundations for a theory of language change. In: Winfred P. Lehmann & Yakov Malkiel (eds.), Directions for historical linguistics, 95-188. Austin: University of Texas Press.
