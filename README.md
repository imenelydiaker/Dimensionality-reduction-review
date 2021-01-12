# Manifold Learning Project

In this project, we implement some of the methods presented in [Dimensionality Reduciton Review by Maaten et al.](https://lvdmaaten.github.io/publications/papers/TR_Dimensionality_Reduction_Review_2009.pdf)

### Authors : 
* Mouad LARIBIA *mouad.laribia@univ-lyon2.fr*
* Imene KERBOUA *imene.kerboua@univ-lyon2.fr*

## The project directory :
You will find in *src* directory : 
* *artificial_datasets.R* : the source code to genrate the artificial datasets.
* *metrics.R* :  the source code of both metrics trustworthiness and continuity.
* *cord_dim.R* : the source code to calculate the correlation dimension.
* *int_dim_estim.R* : the source code to estimate intrinsic diemnsion fo datasets (tested on 5000 datapoints of each dataset). Runing this file will generate an image *"dim_estim.png"* that you will find in the main directory.
* *main.R* : the source code of the whole experience and DR methods testing. By runing this file, you will run the 6 DR methods on 6 datsets, it may take a little long to run everything according to your CPU capacity. Each experience will genrate images by the name *"nameOfDataset_.png"* where you will find the projections of the datapoints in the low-dimensional space. It will also genrate a 3D plot of 4 artificial datsets used for the experiments.

You will find in *data* directory :
* Copies of generated datapoints of artificial datasets we used for our experiments.
* minimnist dataset for the real dataset we used.

You will find in *results* directory :
* Trustworthiness and continuity metrics values of our experiments.
* A directory named *plot* with plots we used in our project report.

## Getting started : 

* Open the .Rproj file in Rstudio.
* Uncomment the lines of code in *main.R* to install required packages.
* Run *main.R* with : 
    > source("main.R") 

PS : the experiences may take a little long to run because of the calculations of the metrics values.
