library(plotly)
#library(lattice)

#points <- sapply(1:1000, function(x) vapply((x:1000)/1000, function(y)c(y, x)))
points <- sapply(1:1000, function(x) vapply((x:1000), function(y)c(y/1000, x/1000), numeric(2)))
points <- do.call(cbind, points)

x <- points[1,]
y <- points[2,]

#points <- matrix((1:1000)/1000, 1000)

#x <- points
#y <- t(points)


calc <- function(x, y){
    z <- x*(1-x)*(1-y)^2
    z <- z + y*(1-y)*(1-x)^2
    z <- z + x^2*y*(1-y)
    z <- z + y^2*x*(1-x)
    z <- z + 2*(x^2*(1-y)^2 + y^2*(1-x)^2)
    z <- 0.5*z - x*(1-x)
    return(z)
}

z <- x*(1-x)*(1-y)^2
z <- z + y*(1-y)*(1-x)^2
z <- z + x^2*y*(1-y)
z <- z + y^2*x*(1-x)
z <- z + 2*(x^2*(1-y)^2 + y^2*(1-x)^2)
z <- 0.5*z - x*(1-x)


#p <- plot_ly(x = x, y = y, z = z) %>% add_surface()

p <- plot_ly(x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = "lines", marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))


#x <- runif(50, 0, 1)
#y <- runif(50, 0, 1)
#z <- runif(50, 0, 1)

#p <- plot_ly(x = ~x, y = ~y, z = ~z, type = 'mesh3d')


print(p)
#chart_link = api_create(p, filename = "test")
#chart_link

#test <- plot_ly(x = c(1, 2, 3), y = c(1, 2, 3), Z = c(1, 2, 3), type = 'mesh3d', mode = 'lines')
#print(test)

#w <- wireframe(z ~ x*(1-x)*(1-y)*(1-y)+ y*(1-y)*(1-x)*(1-x) + x*x*y*(1-y) + y*y*x*(1-x) + 2*(x*x*(1-y)*(1-y) + y*y*(1-x)*(1-x)) - x(1-x),
               
#               colorkey = T,
 #              xlab = "From pop k",
 #               ylab = "other",
 #              drape = T
#)
#print(w)