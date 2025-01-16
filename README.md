# ds1_team_project
Data Science in Health 1 - Team Project 7/31/2024

BTC 1859H: Data Science in Health I

Summer term 2024

 

Team Project - Description

 

Topic: Sleep Disturbance in Patients with liver transplants and its relationship to Quality of Life

 

 

It is perceived that patients with liver transplant have problems with sleep. In addition, it is hypothesized that sleep disturbance has a direct effect on Quality of Life. The objective of this project is the investigation of these hypotheses with the use of an observational dataset of 268 patients with liver transplant. You can download the data here Download here. You will need to answer the following two main research questions:

What is the prevalence of subjective sleep disturbance and what are its predictors in post liver transplant recipients
What is the impact and relationship of sleep disturbance with health related quality of life in these patients
 

In the data set you are receiving you will find a large number of variables. You will only need to focus on the following ones:

Variables measuring sleep disturbance: they correspond to scores from different patient-response questionnaires and they include:
Pittsburgh Sleep Quality Index (PSQI), the Epworth sleepiness scale (ESS), the Berlin sleepiness scale (BSS) and the Athens insomnia scale (AIS). BSS is binary (value of 1 indicates disturbance), and the rest are numeric with higher values indicating worse sleep. In the data, there are some other variables related to measuring sleep disturbance but you can ignore them.

Measures of Health Related Quality of Life. This “construct” is measured by two separate variables, SF36 PCS and SF36 MCS. The former variable measures the physical component of quality of life and the latter the mental component. They both take numeric values, with larger values indicating better quality of life.
Demographic and clinical variables. These include:
- Age

- Gender

- Body Mass Index (BMI)

- time elapsed between transplant and questionnaire completion

- liver diagnosis

- recurrence of original disease process (“recurrence of disease”)

- evidence of rejection/graft dysfunction

- evidence of any fibrosis (“Any fibrosis”)

- renal failure

- depression

- corticosteroid

 

These variables are categorical with the exception of Age, BMI and time elapsed, which are numeric.

 

Information on these variables and the meaning of their values can be found in the attached Data Dictionary document Download Data Dictionary document. The variables you need to focus on are highlighted in yellow. The first column corresponds to the column letter if you open the data in Excel.

 

In your analysis you will need to use any of the methods taught in this course to accomplish the following goals:

Description of relevant data
Estimation of the prevalence of sleep disturbance.
Identify predictors that are associated with the sleep disturbance.
Evaluate the relationship between sleep disturbance and quality of life (physical and mental).
 

Note here that in these data sleep disturbance is measured by the included 4 “instruments”. You could use the measurements as continuous or/and convert them to binary by using clinically accepted thresholds: for ESS, values above 10 indicate sleep disturbance; for PSQI, values above 4 indicate disturbed sleep; for AIS, values above 5 indicate insomnia (i.e. sleep disturbance). You may find that some predictors are better associated with some of these measures than with others (which is expected as these instruments measure different aspects or “facets” of sleep quality). Make sure that you make your analysis as comprehensive as possible.

 

As in previous assignments, you need to state clearly your assumptions, justify your choices, explain clearly the advantages and limitations of the strategies you choose, and also discuss alternatives (and if you like, discuss also scenarios where these alternative would have been preferred choices).
