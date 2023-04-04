# Predictability of language change project
This repository contains the script and 2 datasets for the "Predictability of language change" project. As is already clear from the title, this project tries to answer the question *how predictable is language change?*

## General idea
It is most likely impossible to foresee when and where a particular language change will emerge (Weinreich et al.’s 1968 problem of actuation), but the propagation of a language change follows a fairly predictable trajectory. Language change follows an S-cure through time, that is best modelled by the logistisc equation. We can conclude that a change is real when the residuals of the logistic curve (the difference between the observed and predicted values) are small. In this project we take a closer look at these residuals and ask the following questions:

1. Are S-curves robust enough to be used for prediction and forecasting? 
2. Which variables will make the (standardized) residuals larger in the positive or negative direction in case of an ongoing change? 

In order to answer these questions we have build different models for 15 known binary language changes (new vs old variant), with the change (new or old variant) as the dependent variable and the decade (total of 15 decades) as the independent variable. For each change there are four models: one with all the data and three other models, each with some part of the data left out (first 5 decades, middle 5 decades or last 5 decades) to create a sliding window effect. We compare the residuals of these 3 partly blinded models to the residuals of the model with all the data to see how well the blind models are able to predict the missing decades.

## Data
Included in this repository are 2 datasets to try out the code. The other 13 datasets can be downloaded from Zenodo. Each dataset contains a binary alternation between an old and a new variant, tracked over 15 decades from 1850 to 1999. All changes were extracted from C-CLAMP (Piersoul, De Troij & Van de Velde 2021), a 200 million token corpus of excerpts from cultural magazines written in Dutch. The datasets include the following changes:
- the *dat/wat* shift in indefinite numerals
- the *dat/wat* shift in the superlative
- the demise of predeterminer *al* 
- the deflection of *veel*
- the rise of the auxiliary *zijn* for the perfect tense
- the demise of the inflection on the attributive first person singular possessive 
- the rise of the close apposition with *soort* 
- the hortative alternation
- the rise of experiencer-subject construal with psych verbs
- the loss of the article in quantifier *tal van* 
- plural ending in *-s* vs. plural ending in *-en*
- the loss of the article in complex prepositions 
- the rise of the periphrastic superlative
- *niet meer* ADJ vs. *niet* ADJ *meer*
- Mass noun van Noun vs. Mass noun Noun

## Code

The R script process_datasets.R builds models (as described above) and creates graphs based on the datasets. As an example, take a look at the models for the hortative alternation:

![The hortative alternation_final](https://user-images.githubusercontent.com/107923146/212959306-be672e37-ef2f-44d2-aea2-fd0ad0d27d62.png)

The red line is the complete model. The other colored lines are the partly blinded models. The black dots represent the real observed values for each decade. This graph allows for an easy visual inspection of the performance of the different models. In order to perform a quantitative analysis of the residuals, the script also collected the residuals of each model in a new dataset: residuals_analysis.xlsx. This dataset was further annotated for significance (2 changes were not significant, i.e. the rise of experiencer-subject construal with psych verbs and the *dat/wat* shift in the superlative), the type of change (did the change involve inflectional morphology or not?) and the part of the curve (do the 15 decades show the whole curve or only part of it: start, middle or end?), creating residuals_analysis_annotated.xlsx. 

This new dataset was analyzed in residuals_analysis.R. The residuals of the insignificant datasets were dropped. It was found that the interaction between the position of the sliding window and the part of the curve for which the data is omitted is significant. With respect to the position of the window, it is easier to reconstruct the middle of the S-curve than the begin or the end. The type of change did not have a significant influence on the residuals.

![Interaction effect for window position and part of the curve](https://user-images.githubusercontent.com/107923146/213192154-970b962d-f9ec-4b46-abaf-44238d11ba94.png)


## References
- Piersoul, Jozefien, Robbert De Troij & Freek Van de Velde. 2021. 150 years of written Dutch: The construction of the Dutch Corpus of Contemporary and Late Modern Periodicals. Nederlandse Taalkunde 26(2). 171-194.
- Weinreich, Uriel, William Labov & Marvin Herzog. 1968. Empirical foundations for a theory of language change. In: Winfred P. Lehmann & Yakov Malkiel (eds.), Directions for historical linguistics, 95-188. Austin: University of Texas Press.
