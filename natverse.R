## Plot a Neuron
n = Cell07PNs[[1]]
df <- n$d

x1 <- df[df$PointNo %in% branchpoints(n),]
x2 <- df[df$PointNo %in% endpoints(n),]
x3 <- df[df$PointNo %in% n$StartPoint, ]
branch <- xyzmatrix(x1) ## branch points
end <- xyzmatrix(x2) ## end points
soma <- xyzmatrix(x3) ## soma

plot(n, WithNodes = F, col = 'black', cex=2)
points(branch, col = colpal2[2], pch = 19, cex = 1.35)
points(end, col = colpal2[5], pch = 19, cex = 1.35)
points(soma, col = colpal2[9], pch = 19, cex = 1.35)
ex.cs1 <- expression(plain("branch points"),  paste("end points"), paste("the soma"))
legend(195,80, ex.cs1, pch = 21, cex = 0.85, 
       pt.bg = c(colpal2[2], colpal2[5], colpal2[9]), 
       title="Type of Nodes", title.col = "gray25", pt.cex = 1.5)



