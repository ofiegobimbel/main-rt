import rpy2
import rpy2.robjects as robjects
import rpy2.robjects.packages as pkgs

base = pkgs.importr("base")
utils = pkgs.importr("utils")

# utils.install_packages("fortunes")

ftns = pkgs.importr("fortunes")
ftn7 = ftns.fortune(7)

print(ftn7)

import pandas

fiction = pandas.read_csv("data-sampel.csv", sep=";", low_memory=False, index_col=False)
fiction.head()
print(fiction[0:5])
print(fiction['2'])

print(fiction.loc[0,'3'])
print(fiction.loc[1,'3'])
print(fiction.loc[2,'3'])

import pyarrow
import rpy2_arrow.pyarrow_rarrow as pyra

fiction2 = pyarrow.Table.from_pandas(fiction)
print('')

fiction3 = pyra.pyarrow_table_to_r_table(fiction2)
print('')

%load_ext rpy2.ipython
%%R
suppressMessages({
    library(dplyr)
    library(arrow)
})

