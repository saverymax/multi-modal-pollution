# Experiments

We now describe how to run the experiments used in this work and how to evaluate the forecasts. All training runs of the transformer can be [visualized here](https://wandb.ai/mix/mvts-forecasting?workspace=user-mix).

To compare the transformer and the VAR model, you can run [this script](https://github.com/saverymax/multi-modal-pollution/blob/main/src/evaluation/evaluation_learning_rate.R).

4 experiments with the transformer were conducted:
1. Learning rate effect  
2. Forecast mask effect  
3. Effect of air measuring station   
4. Ablation of variables from input   

The commands to train/test the appropriate transformer models can be found in the [aforementioned location](https://github.com/saverymax/mvts_transformer/tree/master/experiments/generated_experiments).

The same script that compares the transformer and the VAR model also generates the learning rate experiment.

All tables and figures found generated by these scripts can be found in the thesis document itself.

If you would like to look at an interactive report detailing the training and validation loss from experiments 1, 2, and 4, we have made [a report available here](https://wandb.ai/mix/mvts-forecasting/reports/Forecasting-Air-Pollution-During-COVID-19--VmlldzoxOTA1Mzc0).

# Evaluation

The evaluation of the models can be conducted using [these scripts](https://github.com/saverymax/multi-modal-pollution/tree/main/src/evaluation).

You can generate the results for each experiment using the standalone R scripts. Furthermore, you can generate all experimental results in [the RNarkdown script](https://github.com/saverymax/multi-modal-pollution/blob/main/src/evaluation/evaluation.RMD). This also includes all text found in the results
section of the thesis.