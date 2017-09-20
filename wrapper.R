gamma        <- c(1, 2)
project.over <- c(35, 42, 49)
pstay        <- c(0.90, 0.95, 0.99)
tproj        <- 7 * c(21, 29)
params       <- expand.grid(gamma = gamma,
                             project.over = project.over,
                             pstay = pstay,
                             tproj = tproj)
write.csv(file = "output/params.csv", params, quote = FALSE)
params.len  <- nrow(params)
for(i in 1:params.len){
    print(paste0("i = ", i))
    pow_dist    <- params[i, "gamma"]
    n.dates.sim <- params[i, "project.over"]
    p.stay      <- params[i, "pstay"]
    t.proj      <- params[i, "tproj"]
    outfile     <- paste0("output/",i)
    source("scratch.R")
}
