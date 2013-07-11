# Day 4 Tutorial

num <- 500
nw <- network.initialize(num, directed = FALSE)
nw %v% "race" <- rep(0:1, each = 250)

formation <- ~edges + nodematch("race") + degree(0) + concurrent
target.stats <- c(225, 187, 180, 90)
dissolution <- ~offset(edges)

duration <- 20
coef.diss <- diss.coefs(dissolution, duration)
coef.diss


est <- epiNet.est(nw, 
                  formation, 
                  dissolution, 
                  target.stats, 
                  coef.diss, 
                  carnegie = TRUE)

plot(est)
plot(est, plots.joined = F, dx.start = 100)


nwsims <- epiNet.simNet(est, nsteps = 250, nsims = 1)
nwsims

trans.rate <- c(rep(0.5, 20), 0.2)
sim <- epiNet.simTrans(nwsims, 
                       sims.per.nw = 1,
                       vital = F,
                       trans.rate = trans.rate, 
                       i.num = 10, 
                       verbose = T)

nw1 <- sim$network$sim1


# Lab 

num.females <- 744/2 
num.males <- 722/2
nw <- network.initialize(num.females + num.males, 
                         bipartite = num.females, 
                         directed = F)
nw %v% 'sex' <- c(rep(1, num.females), rep(2, num.males))

deg.dist.females <- c(0.2,
                      0.786,
                      0.011,
                      0.003)
deg.dist.males <- c(0.328,
                    0.524,
                    0.127,
                    0.021)
bip.degdist.check(num.females, num.males, 
                  deg.dist.females, deg.dist.males)

tg.edges <- round(sum(num.males * deg.dist.males * 0:3))
tg.m.0 <- round(num.males * deg.dist.males[1], 0)
tg.m.1 <- round(num.males * deg.dist.males[2], 0)
tg.f.0 <- round(num.females * deg.dist.females[1], 0)
tg.f.1 <- round(num.females * deg.dist.females[2], 0)
target.stats <- c(tg.edges, tg.f.0, tg.f.1, tg.m.0, tg.m.1)

formation <- ~edges + b1degree(0:1) + b2degree(0:1)
dissolution <- ~offset(edges)
duration <- 38
coef.diss <- diss.coefs(dissolution, duration)
coef.diss

est <- epiNet.est(nw, 
                  formation, 
                  dissolution, 
                  target.stats, 
                  coef.diss, 
                  carnegie = TRUE)

plot(est) # group plot
plot(est, plots.joined = F, dx.start = 100) # individual plot

nwsims <- epiNet.simNet(est, nsteps = 250, nsims = 1)

trans.rate <- c(rep(0.5, 20), 0.25)
trans.rate2 <- c(rep(0.5, 20), 0.5)
trans.rate3 <- c(rep(0.5, 20), 0.75)
trans.rate4 <- c(rep(0.5, 20), 0.05)

set.seed(0)

sim <- epiNet.simTrans(nwsims, 
                       sims.per.nw = 1,
                       vital = F,
                       trans.rate = trans.rate, 
                       i.num = 10, 
                       verbose = T)

sim2 <- epiNet.simTrans(nwsims, 
                       sims.per.nw = 1,
                       vital = F,
                       trans.rate = trans.rate2, 
                       i.num = 10, 
                       verbose = T)

sim3 <- epiNet.simTrans(nwsims, 
                       sims.per.nw = 1,
                       vital = F,
                       trans.rate = trans.rate3, 
                       i.num = 10, 
                       verbose = T)

sim4 <- epiNet.simTrans(nwsims, 
                       sims.per.nw = 1,
                       vital = F,
                       trans.rate = trans.rate4, 
                       i.num = 10, 
                       verbose = T)


nw1 <- sim$network$sim1
sim.nw1 <- sim$network$sim1
sim2.nw1 <- sim2$network$sim1
sim3.nw1 <- sim3$network$sim1
sim4.nw1 <- sim4$network$sim1

vcols <- ifelse(nw %v% 'sex' == 1, 'pink2', 'dodgerblue')
plot(nw1, vertex.cex=0.9, vertex.col=vcols) # full network
plot(sim, ptype="network", at = 1, vertex.cex=0.9, vertex.col=vcols) # timestep 1
plot(sim, ptype="network", at = 2, vertex.cex=0.9, vertex.col=vcols) # timestep 2
plot(sim, ptype="network", at = 3, vertex.cex=0.9, vertex.col=vcols) # timestep 3
plot(sim, ptype="network", at = 4, vertex.cex=0.9, vertex.col=vcols) # timestep 4
plot(sim, ptype="network", at = 5, vertex.cex=0.9, vertex.col=vcols) # timestep 5
plot(sim, ptype="network", at = 10, vertex.cex=0.9, vertex.col=vcols) # timestep 10

plot(sim, ptype = "sim", axs = "r", sim.lines = TRUE, sim.col = c("blue", "red"))
plot(sim, ptype = "sim", compart = "si.flow", leg = T, popfrac = F, axs = "r", sim.lines = TRUE)
plot(sim, ptype = "network", at = 1, col.inf = TRUE) # partnership network
plot(sim, ptype = "sim", compart = c("i.num","i.num.m2"), leg = T, popfrac = F, axs = "r", sim.lines = TRUE, sim.col = c("blue", "red"))

# Variables: degree dist, duration, transmission, acts

plot(sim, ptype="network", at = 1, vertex.cex=0.9, vertex.col=vcols) # timestep 1
plot(sim2, ptype="network", at = 1, vertex.cex=0.9, vertex.col=vcols) # timestep 1
plot(sim3, ptype="network", at = 1, vertex.cex=0.9, vertex.col=vcols) # timestep 1

plot(sim, ptype = "sim", axs = "r", sim.lines = TRUE, sim.col = c("blue", "red"))
plot(sim, ptype = "sim", compart = "si.flow", leg = T, popfrac = F, axs = "r", sim.lines = TRUE)
plot(sim, ptype = "network", at = 1, col.inf = TRUE) # partnership network
plot(sim, ptype = "sim", compart = c("i.num","i.num.m2"), leg = T, popfrac = T, axs = "r", sim.lines = TRUE, sim.col = c("blue", "red"), main = "Network SI Model Sim 1")

plot(sim2, ptype = "sim", axs = "r", sim.lines = TRUE, sim.col = c("blue", "red"))
plot(sim2, ptype = "sim", compart = "si.flow", leg = T, popfrac = F, axs = "r", sim.lines = TRUE)
plot(sim2, ptype = "network", at = 1, col.inf = TRUE) # partnership network
plot(sim2, ptype = "sim", compart = c("i.num","i.num.m2"), leg = T, popfrac = T, axs = "r", sim.lines = TRUE, sim.col = c("blue", "red"), , main = "Network SI Model Sim 2")

plot(sim3, ptype = "sim", axs = "r", sim.lines = TRUE, sim.col = c("blue", "red"))
plot(sim3, ptype = "sim", compart = "si.flow", leg = T, popfrac = F, axs = "r", sim.lines = TRUE)
plot(sim3, ptype = "network", at = 1, col.inf = TRUE) # partnership network
plot(sim3, ptype = "sim", compart = c("i.num","i.num.m2"), leg = T, popfrac = T, axs = "r", sim.lines = TRUE, sim.col = c("blue", "red") , main = "Network SI Model Sim 3")

# Show and Tell
trans.rate4
plot(sim4, ptype = "sim", compart = c("i.num","i.num.m2"), leg = T, popfrac = T, axs = "r", sim.lines = TRUE, sim.col = c("blue", "red"), main = "Network SI Model Sim 4")
trans.rate
plot(sim, ptype = "sim", compart = c("i.num","i.num.m2"), leg = T, popfrac = T, axs = "r", sim.lines = TRUE, sim.col = c("blue", "red"), main = "Network SI Model Sim 1")
trans.rate2
plot(sim2, ptype = "sim", compart = c("i.num","i.num.m2"), leg = T, popfrac = T, axs = "r", sim.lines = TRUE, sim.col = c("blue", "red"), , main = "Network SI Model Sim 2")
trans.rate3
plot(sim3, ptype = "sim", compart = c("i.num","i.num.m2"), leg = T, popfrac = T, axs = "r", sim.lines = TRUE, sim.col = c("blue", "red") , main = "Network SI Model Sim 3")

# Yesterday's code
ergm.fit <- ergm(nw ~ edges + b1degree(0:1) + b2degree(0:1),
                 target.stats = target.stats)


ergm.sim <- simulate(ergm.fit)
par(mar=c(0,0,0,0))
vcols <- ifelse(nw %v% 'sex' == 1, 'pink2', 'dodgerblue')
plot(ergm.sim, vertex.cex=0.9, vertex.col=vcols)



