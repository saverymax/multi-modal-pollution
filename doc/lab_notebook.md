# Lab notebook for thesis

## Intro

This is bit of an experiment in making lab notebook public and open source. But I think it is valuable to show progress and as a resource
for later work.

## HPC 

Submitting interactive jobs:
gpu
qsub -I -l walltime=2:00:00 -l nodes=1:ppn=9:gpus=1:skylake -l partition=gpu -l pmem=5gb -A default_project

To run with just debugging, add 
-l qos=debugging

cpu
qsub -I -l walltime=01:00:00 -l nodes=1:ppn=36 -A default_project

p100s are on skylake, 9 cores each. v100s are on cascadlake, 4 cores each.
-A is for account
Credit system:
https://docs.vscentrum.be/en/latest/leuven/credits.html#credits-to-use-ku-leuven-infrastructure
https://vlaams-supercomputing-centrum-vscdocumentation.readthedocs-hosted.com/en/latest/jobs/credit_system_basics.html#credit-system-basics
default_project is for introductory credits.

To move all files but one:
add shopt -s extglob to .bashrc
mv !(somefile) ~/newFolder

Setting up mvts environment. Using miniconda so didn't.
Took a bit of trickery, as usual, particularly sktime.
conda create -n mvts
conda install pytorch torchvision torchaudio cudatoolkit=10.2 -c pytorch

Downloaded data with 
```
wget https://zenodo.org/record/3902651/files/Monash_UEA_UCR_Regression_Archive.zip?download=1
```

For info about datasets:
http://tseregression.org/

Pretraining mvts with Beijing data:
python src/main.py --output_dir $VSC_DATA/thesis/mvts_output --comment "pretraining through imputation" --name BeijingPM25Quality_pretraining --records_file Imputation_records.xls --data_dir $VSC_DATA/thesis/data/monash_data/BeijingPM25Quality/ --data_class tsra --pattern TRAIN --val_ratio 0.2 --epochs 700 --lr 0.001 --optimizer RAdam --batch_size 32 --pos_encoding learnable --d_model 128

Pretraining mvts with Brussels data:
python src/main.py --output_dir $VSC_DATA/thesis/mvts_output --comment "pretraining through imputation" --name Brussels_pretraining --records_file Imputation_records.xls --data_dir $VSC_DATA/thesis/data/brussels_data/ --data_class bxl --pattern TRAIN --val_ratio 0.2 --epochs 5 --lr 0.001 --optimizer RAdam --batch_size 32 --pos_encoding learnable --d_model 128

Numpy version is too new or something

Github organization:
    forked mvts_transformer: https://gist.github.com/jpierson/b6c0815e9dd7078f6b8cc3cb9076dd4
    need to push my windows data processing code somewhere

To cache public token:
https://docs.github.com/en/get-started/getting-started-with-git/caching-your-github-credentials-in-git
```
conda install gh --channel conda-forge
```
Then run 
```
gh auth login
```
and follow protocol.
You know, I had to generate a Personal access key to do this, with the correct scope. You can do this in GitHub Developer settings in settings. https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token

## Sentinel Data

### Installation of s5p:
conda create --override-channels -c conda-forge --file requirements.txt --name sentinel_env
pip install sentinelsat

I followed instructions here https://github.com/bilelomrani1/s5p-tools
but I didn't use -c stcorp and I deleted harp from the requirements file. It might not be necessary except for L3 products.

Well I still can't get harp to install. I am running 
conda install -c conda-forge after doing
```
conda config --add channels conda-forge
conda config --set channel_priority strict
conda update conda
```
Not sure if that will work
Well, it really fucked things up, because it will now use conda-forge as the channel to update conda with (this is what setting strict does)
so it changed all my environment packages and messed with the pytorch/cuda libs, changing them to cpu :(

TODO: Retry procedure
```
conda install -n sentinel_env harp
didnt work below:
conda install -n sentinel_env -c conda-forge harp
```
The issue is that there wsa not the harp=1.11 version anymore, so it wouldnt install in the req.txt. 
Then once running the above conda command, it took a looong time (45 mins) to resolve conflicts.
I also had to do 
```
conda config --set channel_priority false
```
and reinstall my mvts environment after messing it up.

## Running 
To request
python s5p-request.py L2__NO2___ --aoi $VSC_DATA/thesis/multi-modal-pollution/data/geojson/Brussels_tunnels_polygon.json --date 20200301 20210601

And to compress
python s5p-compress.py $VSC_DATA/thesis/s5p-tools/processed/processed__NO2___/NO2___26-8-2021__26-8-2021.nc tropospheric_NO2_column_number_density


## Code progress:

### Forecasting

Finished modifying ForecastRunner. Need to test.

good advice: doing all handling outside of the classes, and once you select the class, no options are handled.

Need to deal with self.IDS now that I am handling forecasting in a realistic way.

Go through all todos!

Get bxl data in good shape, add measuring stations and somethingg.. sentinel..
limit_size in bxl data??


## Documentation

Running sphinx documentation. Need to publish GitHub pages from gh-pages branch, but have the github actions file in master. I can update docs in the github pages branch? Im not totall sure. 
Oh, I just push docs to the main branch, but its just that github actions will build docs in the gh-pages branch...
