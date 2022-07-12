# Tallest-Buildings

This repository contains a codebase to generate an R shiny application that can be used to visualise the 
tallest buildings in the world using the following three kaggle datasets:

1. [tallest buildings](https://www.kaggle.com/datasets/stpeteishii/world-tallest-buildings)
2. [largest cities](https://www.kaggle.com/datasets/mathurinache/themostlargestcitiesintheworld19502035)
3. [countries of the world](https://www.kaggle.com/datasets/fernandol/countries-of-the-world)

The codebase is composed of the file that is sourced to read and prepare the data `read_and_prepare_data.R` 
and a file to run the app `app.R`.

The app is composed of three pages that can used to visualise the following:
* Information regarding the worlds top 100 tallest buildings. This includes a map and a chart of ordered 
building heights.
* Country information: a map with an drop down menu containing economic and similar indicators (e.g., GDP per 
capita) which is represented by the density in the map.
* City population information: a time-series of population totals where data is available.

