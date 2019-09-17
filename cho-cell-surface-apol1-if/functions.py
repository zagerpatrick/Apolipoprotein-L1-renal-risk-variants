# Import packages and modules
import random
import re
import glob
import collections
import math
import numpy as np
import pandas as pd
import scipy as sp
import skimage
import mahotas as mh
import matplotlib.pyplot as plt
import seaborn as sns

from skimage import io
from skimage import filters
from skimage.filters import gaussian
from skimage.filters import threshold_local, threshold_otsu
from skimage.morphology import disk
from skimage.segmentation import clear_border

from matplotlib import colors as c
import matplotlib.gridspec as gridspec

# Define convenience functions

def imread_rgb(f):
    '''
    Function used to read in rgb images properly through
    skimage ImageCollection.
    '''
    return skimage.io.imread(f)

def factor_maker(factortype, filenamelist, steps):
    '''
    Extract relevant factors from filenames and convert them into
    Pandas Series'.

    Parameters
    factortype : list
        List of the factor of interest.
    filenamelist : list
        List of image filenames
    steps: int
        Number of duplicates per each image name.
    '''

    output = []

    for f in range(len(filenamelist)):
        for s in range(len(factortype)):
            if factortype[s] in filenamelist[f]:
                output.extend([factortype[s] for i in range(steps)])
            else:
                pass

    return pd.Series(output)

def atoi(text):
    return int(text) if text.isdigit() else text

def natural_keys(text):
    '''
    alist.sort(key=natural_keys) sorts in human order
    http://nedbatchelder.com/blog/200712/human_sorting.html
    (See Toothy's implementation in the comments)
    '''
    return [ atoi(c) for c in re.split('(\d+)', text) ]

# Setup colormap for watershed images

colors = list(map(plt.cm.jet, range(0, 256, 1)))
random.shuffle(colors)
colors[0] = (0.,0.,0.,1.)
rmap = c.ListedColormap(colors)

# Define smoothing kernel size and other stuff for later
Bc = np.ones((9,9))
selem = np.ones((9,9))
