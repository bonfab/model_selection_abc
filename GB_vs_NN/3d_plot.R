library(plotly)
library(lattice)

points <- matrix(rep((1:100)/1000, 100), 100)
x <- points
y <- t(points)
z <- x*(1-x)*(1-y)*(1-y)
z <- z + y*(1-y)*(1-x)*(1-x)
z <- z + x*x*y*(1-y)
z <- z + y*y*x*(1-x)
z <- z + 2*(x*x*(1-y)*(1-y) + y*y*(1-x)*(1-x))

p <- plot_ly(x = x, y = y, z = z, type = 'mesh3d')

print(p)
#chart_link = api_create(p, filename = "test")
#chart_link

test <- plot_ly(x = c(1, 2, 3), y = c(1, 2, 3), Z = c(1, 2, 3), type = 'mesh3d', mode = 'lines')
print(test)

w <- wireframe(z ~ x*(1-x)*(1-y)*(1-y)+ y*(1-y)*(1-x)*(1-x) + x*x*y*(1-y) + y*y*x*(1-x) + 2*(x*x*(1-y)*(1-y) + y*y*(1-x)*(1-x)) - x(1-x),
               
               colorkey = T,
               xlab = "From pop k",
                ylab = "other",
               drape = T
)
print(w)