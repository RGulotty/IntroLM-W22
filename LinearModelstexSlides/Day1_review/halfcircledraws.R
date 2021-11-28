x <- seq(0, 1, 0.01)
y <- seq(0, 1, 0.01)
z <- outer(x, y, function(x, y){ifelse(y>x^2, 2/3,0)})




pdf("~/Dropbox/LinearModels/LinearModelstexSlides/Day1_review/halfcircle.pdf")

persp(x, y, z, phi = 30)

dev.off()