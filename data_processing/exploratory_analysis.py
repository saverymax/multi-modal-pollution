"""
Analysis of netCDF data from sentinel following the jupyter notebook
https://github.com/bilelomrani1/s5p-analysis/blob/master/s5p-analysis.ipynb
"""
import xarray as xr
import numpy as np
import pandas as pd
from itertools import product, cycle

import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.dates as mdates
import matplotlib.ticker as mticker
import matplotlib.font_manager as fm
import seaborn as sns;sns.set();sns.set_style("white")

xr.set_options(keep_attrs=True)
DS = xr.open_dataset('../../s5p-tools/processed/processed__NO2___/NO2___14-9-2021__14-9-2021.nc')

VARIABLE = 'tropospheric_NO2_column_number_density'

print(DS[VARIABLE].attrs)
print(DS[VARIABLE])


# Monday=0, Sunday=6
boxes_weekday = [group[VARIABLE].values for _,group in DS.mean(dim='longitude').mean(dim='latitude').groupby('time.weekday')]

WEEKDAYS_LABEL = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']

fig = plt.figure(figsize=(6,2))
ax = plt.axes()
ax.yaxis.set_major_formatter(mticker.ScalarFormatter(useMathText=True, useOffset=True))

boxplot = sns.boxplot(data=boxes_weekday,
                      linewidth=0.5, width=0.5, saturation=0.5, showfliers=False, color="slategrey")
boxplot.set_xticklabels(WEEKDAYS_LABEL)
plt.ylabel(LABEL, labelpad=10)
plt.show()

#plt.savefig("{folder_plots}/box_plot_weekdays_NO2.{format}".format(folder_plots=folder_plots,
#                                                                 format=FORMAT_EXPORT),
#            bbox_inches='tight', dpi=300)
