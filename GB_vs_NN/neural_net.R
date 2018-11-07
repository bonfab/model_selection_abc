library("keras")

trainNN <- function(train_data, train_label, iterations = 30){
  
  dim <- nrow(train_data)
  
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = dim, activation = "softmax", input_shape = c(1, dim)) %>%
    layer_dense(units = dim*2, activation = "softmax") %>%
    layer_dense(units = length(unique(train_label)), activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = c("accuracy")
  )
  
  model %>% fit(
    train_data, train_label,
    epochs = iterations, batch_size = 64,
    validation_split = 0.2
  )
  
  return(model)
    
}

classifyNN <- function(model, test_data){
  return(model %>% predict_classes(test_data))
}

