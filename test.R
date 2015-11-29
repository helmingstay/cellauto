require(cgolr)
.dim <- c(300,400)
.test <- new(cgolr, .dim[1],.dim[2],5,3,0,0)
.test$lives_at <- 2:3
.test$born_at <- 5
.test$grid <- matrix(
    rbinom(prod(.dim), 1, 0.4),
    nrow=.dim[1], ncol=.dim[2]
)
.test$grow <- 1.0
.test$decay <- 0.1
for (ii in 1:200) {
    print(ii)
    plot(levelplot(.test$grid))
    .test$step()
}
