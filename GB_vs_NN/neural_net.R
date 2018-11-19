library("keras")


trainNNd <- function(train_data, train_label, iterations = 30){
  
  dim <- ncol(train_data)
  n_classes <- length(unique(train_label))
  
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 2*dim, activation = "relu", input_shape = c(dim)) %>%
    #layer_dropout(rate = 0.05) %>%
    layer_dense(units = dim, activation = "relu", kernel_regularizer = regularizer_l2(0.01), activity_regularizer = regularizer_l2()) %>%
    #layer_dropout(rate = 0.1) %>%
    #layer_dense(units = 2*dim, activation = "relu") %>%
    #layer_dropout(rate = 0.1) %>%
    #layer_dense(units = n_classes * 2, activation = "relu") %>%
    layer_dense(units = n_classes, activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = "accuracy"
  )
  
  return(fitModelNN(model, train_data, train_label, iterations))
}


makeNN <- function(model_vec, dim, n_classes){
  
  model <- keras_model_sequential()
  
  for(i in seq(1, length(model_vec))){
    
    if(i %% 2 == 1){
      
      if(i == 1){
        layer_dense(model, units = model_vec[i], activation = "relu", input_shape = c(dim))
      } else {
        layer_dense(model, units = model_vec[i], activation = "relu")
      }
      
    } else
      layer_dropout(model, rate = model_vec[i])
  }
  
  layer_dense(model, units = n_classes, activation = "softmax")
  return(model)
}

trainNN <- function(model, train_data, train_label, iterations = 30){
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = "accuracy"
  )
  
  return(fitModelNN(model, train_data, train_label, iterations, validation_split = 0))
}


findParametersNN <- function(data, iterations = 30){
  
  n_classes <- length(unique(train_label))
  
  drop_out_max <- 0.3
  hidden_max <- 2
  node_max <- 3*ncol(train_data)
  
  max_precision <- 0
  best_model <- NULL
  
  for(hidden in seq(1, hidden_max)){
    
    basis <- c(n_classes, 0)
    
    helper <- rep(basis, times = hidden)
    
    tracker <- length(helper)
    
    print(helper)
    #summary(helper)
    #print(node_max)
    print(helper[1])
    print(tracker %% 2)
    
    count <- 0
    
    while(helper[1] <= node_max){
      
      while(TRUE){
        
        if(tracker %% 2 == 1){
          helper[tracker] <- helper[tracker] + 2
          
          if(helper[tracker] > node_max){
            break
          }
          
        } else {
          helper[tracker] <- helper[tracker] + 0.02
          
          if(helper[tracker] > drop_out_max){
            break
          }
          
        }
        
        
        precision <- crossValidate(makeNN(helper), data, trainNN, classifyNN)
        
        if(precision > max_precision){
          max_precision <- precision
          best_model <- helper
        }
        
        count <- count + 1
        
        print(helper)
        
        if(tracker < length(helper)){
          tracker <- length(helper)
        }
      }
      
      if(tracker > 1){
        print(((tracker + 1) %% 2) + 1)
        helper[tracker] = basis[((tracker + 1) %% 2) +1]
        tracker <- tracker - 1
      }
      
    }
  }
  
  return(best_model)
  
}


fitModelNN <- function(model, train_data, train_label, epochs = 40, batch_size = 64, validation_split = 0.1){
  
  x <- as.matrix(train_data)
  y <- to_categorical(as.matrix(train_label), length(unique(train_label)))
  
  model %>% fit(
    x, y,
    epochs = epochs, batch_size = batch_size,
    validation_split = validation_split
  )
  
  return(model)
  
}


classifyNN <- function(model, test_data){
  
  x <- as.matrix(test_data)
  
  return(model %>% predict_classes(x))
}

