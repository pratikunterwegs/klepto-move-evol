# code for landscape gradients and moran i

#### importing libraries and paths
# check python path
import sys

# should yield python 3.7 file path
for p in sys.path:
    print(p)

import pandas as pd  # similar to dplyr! yay!
import os  # has list dir functions etc
import numpy as np  # some matrix functions
import re
from scipy import misc
import matplotlib.pyplot as plt
import imageio  # library to read images

# check the current working directory
os.getcwd()
# gather image output
output_folder = os.path.join(os.getcwd(), "data_sim/")  # os.path.abspath("output")
# check for the right folder
# if "images" not in output_folder:
#     raise Exception('seems like the wrong output folder...')

#### list files and filter by name
# gather contents of the folder
img_files = list()
for root, directories, filenames in os.walk(output_folder):
    for filename in filenames:
        img_files.append(os.path.join(root, filename))

# filter filenames to match foodlandscape
img_files = list(filter(lambda x: "png" in x and "rep" in x, img_files))


# function to get image generation
def get_image_generation (x):
    assert "str" in str(type(x)), "input doesn't seem to be a filepath"
    generation = int(re.findall(r'(\d{5})', x)[0])
    return generation


# get sim type
def get_image_data (x):
    assert "str" in str(type(x)), "input doesn't seem to be a filepath"
    sim_type = re.findall(r'(obligate|facultative|forager|random)', x)[0]
    sim_gen = int(re.findall(r'(\d{5})', x)[1])
    sim_rep = int(re.findall(r'rep\_(\d{3})', x)[0])
    sim_rmax = float(re.findall(r'\d+\.\d+', x)[0]) # unfortunately hardcoded
    return [sim_type, sim_gen, sim_rep, sim_rmax, x]

# get the image identity to match to parameters later
img_data = list(map(get_image_data, img_files))
# make data frame
img_data = pd.DataFrame.from_records(img_data, columns=['sim_type','gen', 'replicate', 'regrowth','path'])


# function to read images, get gradient, and count non zero
# takes a 2d array
def get_prop_plateau (x, dim, layer):
    # assert "landscape" in x, "input is not a landscape"
    image = imageio.imread(x)[:,:,layer]
    assert image.ndim == 2, "get_prop_plateau: not a 2d array"
    gradient = np.gradient(image)
    mag = np.sqrt(gradient[0]**2 + gradient[1]**2)
    mag = mag[mag == 0]
    p_plateau = len(mag) / (dim**2)
    return p_plateau


# run over files
img_data['p_clueless'] = img_data['path'].apply(get_prop_plateau, dim=512, layer=3)

img_data.to_csv("data_sim/results/data_p_clueless.csv")

# ends here