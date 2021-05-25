rm(list = ls())
prjName = "test"
source(here::here("E:/04. TalentPlatform/Github/TalentPlatform-R/src", "InitConfig.R"), encoding = "UTF-8")

# install.packages("devtools")
install.packages("dlib")

devtools::install_github("bnosac/image", subdir = "image.CornerDetectionF9",build_vignettes = TRUE)
devtools::install_github("bnosac/image", subdir = "image.LineSegmentDetector", build_vignettes = TRUE)
devtools::install_github("bnosac/image", subdir = "image.ContourDetector", build_vignettes = TRUE)

# Reqiring libpng and fftw3 to be installed
# devtools::install_github("bnosac/image", subdir = "image.CannyEdges", build_vignettes = TRUE)

devtools::install_github("bnosac/image", subdir = "image.dlib", build_vignettes = TRUE)
devtools::install_github("bnosac/image", subdir = "image.darknet", build_vignettes = TRUE)


remotes::install_github("bnosac/image", subdir = "image.CornerDetectionF9")
remotes::install_github("bnosac/image", subdir = "image.CornerDetectionHarris")
remotes::install_github("bnosac/image", subdir = "image.LineSegmentDetector")
remotes::install_github("bnosac/image", subdir = "image.ContourDetector")
remotes::install_github("bnosac/image", subdir = "image.CannyEdges")
remotes::install_github("bnosac/image", subdir = "image.Otsu")
remotes::install_github("bnosac/image", subdir = "image.dlib")
remotes::install_github("bnosac/image", subdir = "image.darknet")
remotes::install_github("bnosac/image", subdir = "image.DenoiseNLMeans")
remotes::install_github("bnosac/image", subdir = "image.libfacedetection")
remotes::install_github("bnosac/image", subdir = "image.OpenPano")







library(image.darknet)

## Define the model 
yolo_tiny_voc <- image_darknet_model(
  type = "detect",
  model = "tiny-yolo-voc.cfg",
  weights = system.file("models", "tiny-yolo-voc.weights", package="image.darknet"),
  labels = c("aeroplane", "bicycle", "bird", "boat", "bottle", "bus", "car", "cat", "chair", "cow", "diningtable", "dog", "horse", "motorbike", "person", "pottedplant", "sheep", "sofa", "train", "tvmonitor")
)


## Find objects inside the image (reqire full path)
f <- paste0(getwd(),"/sp.jpg")
# f <- system.file("include", "darknet", "data", "sp.jpg", package="image.darknet")


#image_darknet_detect(file=normalizePath("sp.jpg"), object = yolo_tiny_voc)
image_darknet_detect(file=f, object = yolo_tiny_voc)

im_predictions <- image_read(path = "predictions.png") 
plot(im_predictions)



library(image.darknet)
tiny_yolo_pretrain_weight = system.file(package="image.darknet","models","tiny-yolo-voc.weights")
pre_labels = system.file(package="image.darknet","include","darknet","data","voc.names")

R_yolo =image_darknet_model(type="detect", model = "tiny-yolo-voc.cfg", 
  weights= tiny_yolo_pretrain_weight,
  labels= pre_labels )

image_darknet_detect(file = testimgfile,object = R_yolo, threshold = 0.15)


# 10-3. Classification with Darknet
## Define DarkNet model

library(magick)

model <- system.file("include", "darknet", "cfg", "tiny.cfg", package="image.darknet")
weights <- system.file("models", "tiny.weights", package="image.darknet") 
labels <- system.file("include", "darknet", "data", "imagenet.shortnames.list", package="image.darknet")
labels <- readLines(labels)

darknet_tiny <- image_darknet_model(type = "classify", model = model, weights = weights, labels = labels)
darknet_tiny

f <- system.file("include", "darknet", "data", "dog.jpg", package="image.darknet")

#library(magick)
im_dog <- image_read(path = f) 
plot(im_dog)


x <- image_darknet_classify(file = f, object = darknet_tiny)
x











# Object detection on images on Windows.
# Adaptation of the original code by Duncan Golicher: https://rpubs.com/dgolicher/yolo

library(image.darknet)
library(Rcpp)
library(dplyr)
library(tidyr)
library(here)


## define the detection model (YOLO) 
detect_model <- image_darknet_model(
  type = "detect",
  model = "tiny-yolo-voc.cfg",
  weights = system.file(package = "image.darknet", "models", "tiny-yolo-voc.weights"),
  labels = system.file(package = "image.darknet", "include", "darknet", "data", "voc.names")
)


# folder with input images
path <- here("img")

# get all pngs and jpgs
images <- dir(path = path, pattern = "\\.png|\\.jpg|\\.jpeg")

# folder for output images with predictions
dir.create('pred')


# function to be applied to images
detect_objects <- function(x) {
  
  filename <- paste(path, x, sep = "/")
  
  prediction <- image_darknet_detect(
    file = filename,
    object = detect_model,
    threshold = 0.19
  )
  
  file.rename("predictions.png", paste0("pred/", x))
  return(prediction)
}


cppFunction('void redir(){FILE* F=freopen("capture.txt","w+",stdout);}')
cppFunction('void resetredir(){FILE* F=freopen("CON","w+",stdout);}')

writeLines(readLines('capture.txt'))





# Read in the output file
d <- data.frame(txt = unlist(readLines("capture.txt"))) 

## Take out all the lines that we don't need.
d <- d %>% 
  filter(!grepl("Boxes", txt)) %>% 
  filter(!grepl("pandoc", txt)) %>% 
  filter(!grepl("unnamed", txt))

## Find the lines that contain the file names. Make a logical vector called "isfile"
d$isfile <- grepl(path, d$txt)

## Take out the path and keep only the file names
d$txt <- gsub(paste0(path, '/'), "", d$txt)

## Make a column called file that contains either file names or NA
d$file <- ifelse(d$isfile, d$txt, NA)

## All the other lines of text refer to the objects detected
d$object <- ifelse(!d$isfile, d$txt, NA)

## Fill down
d <- tidyr::fill(d, "file")

## Take out NAs and select the last two columns
d <- na.omit(d)[, 3:4]

# Separate the text that is held in two parts
d <- d %>% separate(file, into = c("file", "time"), sep = ":")
d <- d %>% separate(object, into = c("object", "prob"), sep = ":")
d <- d %>% filter(!is.na(prob))

# Keep only the prediction time
d$time <- gsub("Predicted in (.+).$", "\\1", d$time)

# Convert probabilities to numbers
d$prob <- as.numeric(sub("%", "", d$prob)) / 100

# Optionally remove the file
# file.remove("capture.txt")


d %>% knitr::kable()
