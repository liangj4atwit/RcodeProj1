#Before Start----
textDisp=
"Befor using the code:
1. Check working directory, it will save a model in the current working directory
2. Check TensorFlow and Keras installations.
------------------------------------"
cat(textDisp)
textDisp=
"Enviroment for the project:
TensorFlow and Keras Lib require additional steps of instalation.
run >install_tensorflow() and >install_keras() after include lib by >library(tensorflow)
This also apply to keras backend.
------------------------------------"
cat(textDisp)


#Enviroment Check----
textDisp=
  "Checking enviroment, it will print out:
tf.Tensor(b'TensorFlow Enviroment Test', shape=(), dtype=string)
in the command line, if not, please see the instruction on TensorFLow R for installation link:
https://tensorflow.rstudio.com/installation/
------------------------------------"
cat(textDisp)
library(tensorflow)
library(keras)
tf$constant("TensorFlow Enviroment Test")


#Load Data----
#For the project, CIFAR was used for both training and testing, 
#CIFAR-100 have 20 superclass and 10 classes under each superclass
#Load as superclass
#The reason that uses CIFAR instead of resistor is beacuse the lack of file IO support in R.
#Relabele everything into .tfrecord format will take too long to process.
#Read CIFAR data set
#It will take some time to download(If not downloaded).
#It takes some time to load the data.
textDisp=
"Loading dataset, it's going to download from internet so it will take a wile for the first run,
also the size is kind of large so it might take some extra time to load.
------------------------------------"
cat(textDisp)
cifar <- dataset_cifar100(label_mode = "coarse")
textDisp=
"Dataset loaded
------------------------------------"
cat(textDisp)
textDisp=
"Defining classes
------------------------------------"
cat(textDisp)
class_names <- c('aquatic mammals', 'fish', 'flowers', 'food containers', 'fruit and vegetables',
                 'household electrical devices', 'household furniture', 'insects', 'large carnivores', 
                 'large man-made outdoor things','large natural outdoor scenes','large omnivores and herbivores',
                 'medium-sized mammals','non-insect invertebrates','people','reptiles','small mammals','trees',
                 'vehicles 1','vehicles 2')
textDisp=
"Class rename's done
------------------------------------"
cat(textDisp)


#Data Inspection----
#Data Inspection by viewing the first 30 images
textDisp=
"Plot the first 30 images from the training set
------------------------------------"
cat(textDisp)
index <- 1:30
#Parameter for ploting
par(mfcol = c(5,6), mar = rep(1, 4), oma = rep(0.2, 4))
#This load the first 30 images and take some time.
cifar$train$x[index,,,] %>% 
  purrr::array_tree(1) %>%
  purrr::set_names(class_names[cifar$train$y[index] + 1]) %>% 
  purrr::map(as.raster, max = 255) %>%
  purrr::iwalk(~{plot(.x); title(.y)})
textDisp=
"Dataset Inspection's done
------------------------------------"
cat(textDisp)


#Define Model----
textDisp=
"Define a sequential model framework
------------------------------------"
cat(textDisp)
model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(5,5),
                trainable = TRUE, padding = "same",
                input_shape = c(32, 32, 3),
                name = "First_2D_convolution_Layer",
                activation = "relu") %>%
  layer_dropout(0.1,name = "First_Dropout_Layer") %>%
  layer_max_pooling_2d(pool_size = c(2,2),trainable = TRUE,
                       name = "First_Pooling_Layer") %>%
  layer_conv_2d(filters  = 32, kernel_size = c(3,3),
                trainable = TRUE,activation = "relu",
                name = "Second_2D_convolution_Layer") %>%
  layer_dropout(0.1,name = "Second_Dropout_Layer") %>%
  layer_conv_2d(filters  = 32, kernel_size = c(3,3), 
                padding = "same",trainable = TRUE,activation = "relu",
                name = "Third_2D_convolution_Layer") %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3),
                trainable = TRUE,activation = "relu",
                name = "Fourth_2D_convolution_Layer") %>%
  layer_dropout(0.1,name = "Third_Dropout_Layer") %>%
  layer_flatten(name ="Flatten_Layer") %>%
  layer_dense(512,trainable = TRUE,name = "Fully_Connected_Dense_Layer",activation = "relu") %>%
  layer_activation("relu") %>%
  layer_dropout(0.1,name = "Fourth_Dropout_Layer") %>%
  layer_dense(20,trainable = TRUE,name = "Output_Layer",activation = "softmax")

  
textDisp=
"Model have been defined, local name 'model', here is the summary
------------------------------------"
cat(textDisp)
#Model Summary
summary(model)


#Define Compiler----
textDisp=
"Define compiler for the model
------------------------------------"
cat(textDisp)
#Define model compiler, Including loss and learning rate, not sure for R so use adam.
model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)
textDisp=
"Finished defineing compiler, training starts soon
------------------------------------"
cat(textDisp)


#Training Model----
#Start Trainning save as train_history
#unname() shuffle order for validation
#verbose = 2 will print out the progress of each epoch(Iteration)(0: progress only, 1: not print)
#It takes time!
textDisp=
"Training starts now, it will save to train_history in enviroment.
Viewer tab shows the current progress.
This will take some time to process.
The set epoch for training is 10
------------------------------------"
cat(textDisp)
train_history <- model %>% 
  fit(
    x = cifar$train$x, y = cifar$train$y,
    epochs = 10,
    validation_data = unname(cifar$test),
    verbose = 2
  )
textDisp=
"Training finished, It will start to evaluat the model now.
------------------------------------"
cat(textDisp)


#Evaluation and Saving Model----
#See the performace of the model
#Plot the record of training and the details such as accuracy and loss.
textDisp=
"The histry record will be ploted in plot tab
------------------------------------"
cat(textDisp)
plot(train_history)
#Evaluation
textDisp=
"Evaluation will be shown in the command line.
------------------------------------"
cat(textDisp)
evaluate(model, cifar$test$x, cifar$test$y, verbose = 0)
#Supositly, there should be 3 data set for the model, Training, Validation and Test.
#For the tesing of overfit and overtrain.
#Since there is only two so use test for both training validation and final model evaluate
#Save the model under "model" folder after trained(Trained model)
#The model can be load later for transfer learning or deployment with conversion to TensorFLow Lite format.
#This saves the file under the current working directory so check beforehead.
textDisp=
"Saving the trained model to the working directory under 'model' folder
------------------------------------"
cat(textDisp)
save_model_tf(object = model, filepath = "model")
textDisp=
"Saving finished.
Here is the end of the program.
------------------------------------"
cat(textDisp)
