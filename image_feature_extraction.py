# This notebook is using concepts/code from these links.
# 
# https://www.kaggle.com/shivamb/ideas-for-image-features-and-image-quality/notebook
# 
# https://www.pyimagesearch.com/2017/06/05/computing-image-colorfulness-with-opencv-and-python/

from collections import defaultdict
from scipy.stats import itemfreq
from scipy import ndimage as ndi
import matplotlib.pyplot as plt
from skimage import feature
from PIL import Image as IMG
import numpy as np
import pandas as pd 
import operator
import cv2
import os 

from IPython.core.display import HTML 
from IPython.display import Image

from tqdm import tqdm
import time

import imutils
from imutils import build_montages, paths
import argparse

images_path = 'data/train_images/'
imgs = os.listdir(images_path)

features = pd.DataFrame()
features['image'] = imgs

features = features.loc[['-1.' in x for x in features.image]]

def color_analysis(img):
    # obtain the color palatte of the image 
    palatte = defaultdict(int)
    for pixel in img.getdata():
        palatte[pixel] += 1
    
    # sort the colors present in the image 
    sorted_x = sorted(palatte.items(), key=operator.itemgetter(1), reverse = True)
    
    light_shade, dark_shade, shade_count, pixel_limit = 0, 0, 0, 1000
    for i, x in enumerate(sorted_x[:pixel_limit]):
        if all(xx <= 20 for xx in x[0][:3]): ## dull : too much darkness 
            dark_shade += x[1]
        if all(xx >= 240 for xx in x[0][:3]): ## bright : too much whiteness 
            light_shade += x[1]
        shade_count += x[1]
        
    light_percent = round((float(light_shade)/shade_count)*100, 2)
    dark_percent = round((float(dark_shade)/shade_count)*100, 2)
    return light_percent, dark_percent

def perform_color_analysis(img):

    path = images_path + img 
    im = IMG.open(path) #.convert("RGB")
    
    # cut the images into two halves as complete average may give bias results
    size = im.size
    halves = (size[0]/2, size[1]/2)
    im1 = im.crop((0, 0, size[0], halves[1]))
    im2 = im.crop((0, halves[1], size[0], size[1]))

    try:
        light_percent1, dark_percent1 = color_analysis(im1)
        light_percent2, dark_percent2 = color_analysis(im2)
    except Exception as e:
        light_percent1, dark_percent1 = -1, -1
        light_percent2, dark_percent2 = -1, -1

    light_percent = (light_percent1 + light_percent2)/2 
    dark_percent = (dark_percent1 + dark_percent2)/2 
    
    return dark_percent, light_percent

def average_pixel_width(img):
    path = images_path + img 
    im = IMG.open(path)    
    im_array = np.asarray(im.convert(mode='L'))
    edges_sigma1 = feature.canny(im_array, sigma=3)
    apw = (float(np.sum(edges_sigma1)) / (im.size[0]*im.size[1]))
    return apw*100

def getSize(filename):
    filename = images_path + filename
    st = os.stat(filename)
    return st.st_size

def getDimensions(filename):
    filename = images_path + filename
    img_size = IMG.open(filename).size
    return img_size 

def get_blurrness_score(image):
    path =  images_path + image 
    image = cv2.imread(path)
    image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    fm = cv2.Laplacian(image, cv2.CV_64F).var()
    return fm

def image_colorfulness(image, folder):
    # split the image into its respective RGB components
    img = cv2.UMat(cv2.imread('{}/{}'.format(folder, image), cv2.IMREAD_COLOR))
    (B, G, R) = cv2.split(img)#.astype("float")
    # compute rg = R - G
    rg = np.absolute(R - G)
    
    # compute yb = 0.5 * (R + G) - B
    yb = np.absolute(0.5 * (R + G) - B)
 
    # compute the mean and standard deviation of both `rg` and `yb`
    (rbMean, rbStd) = (np.mean(rg), np.std(rg))
    (ybMean, ybStd) = (np.mean(yb), np.std(yb))
 
    # combine the mean and standard deviations
    stdRoot = np.sqrt((rbStd ** 2) + (ybStd ** 2))
    meanRoot = np.sqrt((rbMean ** 2) + (ybMean ** 2))
    
    # derive the "colorfulness" metric and return it
    return stdRoot + (0.3 * meanRoot)

tqdm.pandas()

start=time.time()
features['dullness_whiteness'] = features['image'].apply(lambda x : perform_color_analysis(x))
print(time.time()-start)


features['dullness'] = features.dullness_whiteness.map(lambda x: x[0])
features['whiteness'] = features.dullness_whiteness.map(lambda x: x[1])

features['average_pixel_width'] = features['image'].apply(average_pixel_width)

features['image_size'] = features['image'].apply(getSize)
features['temp_size'] = features['image'].apply(getDimensions)
features['width'] = features['temp_size'].apply(lambda x : x[0])
features['height'] = features['temp_size'].apply(lambda x : x[1])
features['blurrness'] = features['image'].apply(get_blurrness_score)

start=time.time()
features['colorfulness'] = features['image'].apply(lambda x : image_colorfulness(x, images_path))
print(time.time()-start)

features.to_csv('data/train_image.csv',index=False)

