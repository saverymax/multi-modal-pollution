# Models

## ARIMA

The ARIMA (SARIMA and VAR) models can be ran from the modeling directory. These can be run either as the original scripts, or in the RMarkdown (.Rmd) files, which will generate the results shown in the report. 

The model forecasts can be found in the data/model_output/arima directory. These files are produced from the modelling/*.R scripts and are provided for users to rerun the evaluation without rerunning the models.

The following VAR model is used:

\begin{gather*}
    \overrightarrow{y_t}
     =
     \begin{pmatrix}
     a_{1,1} x_{t-1} + a_{1,2} y_{t-1} a_{1,3}z_{t-1} + \cdots + a_{1,22}x_{t-8} + a_{1,23}y_{t-8} + a_{1,24}z_{t-8}+ \mu_{x,t} \\
     a_{2,1} x_{t-1} + a_{2,2} y_{t-1} a_{2,3}z_{t-1} + \cdots + a_{2,22}x_{t-8} + a_{2,23}y_{t-8} + a_{2,24}z_{t-8} + \mu_{y,t} \\
     a_{3,1} x_{t-1} + a_{3,2} y_{t-1} a_{3,3}z_{t-1} + \cdots + a_{3,22}x_{t-8} + a_{3,23}y_{t-8} + a_{3,24}z_{t-8} + \mu_{z,t} \end{pmatrix}
\end{gather*}


## Transformer

The transformer code can be found in the repository https://github.com/saverymax/mvts_transformer, forked from https://github.com/gzerveas/mvts_transformer. In this code you can specify the options for pretraining, regression, or classification, as in the original repository, with any of the original datasets. But now, it is also possible to implement multivariate forecasting by specifying the forecasting option.


