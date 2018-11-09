library("keras")

trainNN <- function(train_data, train_label, iterations = 30){
  
  dim <- ncol(train_data)
  n_classes <- length(unique(train_label))
  
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 2*dim, activation = "relu", input_shape = c(dim)) %>%
    #layer_dropout(rate = 0.05) %>%
    layer_dense(units = 3*dim, activation = "relu") %>%
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


findParametersNN <- function(train_data, train_label, iterations = 30){
  
  n_classes <- length(unique(train_label))
  
  drop_out_max <- 0.4
  hidden_max <- 3
  node_max <- 4*ncol(train_data)
  
  max_precision <- 0
  
  for(hidden in seq(1, hidden_max)){
    
    basis <- c(n_classes, 0)
    
    helper <- rep(basis, times = hidden)
    
    tracker <- length(helper)
    
    print(helper)
    #summary(helper)
    #print(node_max)
    print(helper[1])
    print(tracker %% 2)
    
    while(helper[1] <= node_max){
      
      
      while((helper[tracker] <= node_max && tracker %% 2 == 1) || (helper[tracker] <= drop_out_max)){
        
        ######### doesn't work, tracker pointer moves incorrectly
        
        
        if(tracker %% 2 == 1){
          helper[tracker] <- helper[tracker] + 1
        } else {
          helper[tracker] <- helper[tracker] + 0.01
        }
        
        #
        # TODO crossvalidate
        #
        
        print(helper)
        
        if(tracker < length(helper)){
          tracker <- tracker + 1
        }
      }
      
      helper[tracker] = basis[(tracker %% 2) + 1]
      tracker <- tracker - 1
      
    }
  }
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

