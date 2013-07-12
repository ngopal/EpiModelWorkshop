# Day 5 tutorial

num.m1 <- 500 # females
num.m2 <- 500 # males
nw <- network.initialize(num.m1 + num.m2, bipartite = num.m1, directed = F)
# Assuming no same-sex partnerships in this model for simplicity

deg.dist.m1 <- c(0.4, 0.55, 0.04, 0.01)
deg.dist.m2 <- c(0.48, 0.41, 0.08, 0.03)
bip.degdist.check(num.m1, num.m2, deg.dist.m1, deg.dist.m2)

formation <- ~edges + b1degree(0:1) + b2degree(0:1)
target.stats <- c(330, 200, 275, 240, 205) # use degdist.check to get Deg0 and Deg1 values for m1 and m2
dissolution <- ~offset(edges)
constraints <- ~bd(maxout = 3)

duration <- 25
coef.diss <- diss.coefs(dissolution, duration)
coef.diss

dx.stats <- ~ edges + b1degree(0:5) + b2degree(0:5)
est <- epiNet.est(nw, 
                  formation, 
                  dissolution, 
                  target.stats, 
                  coef.diss, 
                  constraints, 
                  carnegie=TRUE, 
                  stats=TRUE, 
                  stats.form=dx.stats)


i.prev=0.1
i.rand=TRUE 
trans.rate <- c(rep(0.2055, 3),
                rep(0.0088, 100),
                rep(0.0614, 9),
                rep(0, 10))
trans.rate <- trans.rate*4
trans.rate.m2 <- trans.rate/2
b.rate=0.0066
ds.rate=0.0025


sim <- epiNet.simTrans(est,
                       vital = TRUE,
                       i.prev = i.prev,
                       i.rand = TRUE, 
                       trans.rate = trans.rate,
                       trans.rate.m2 = trans.rate.m2,
                       b.rate = b.rate,
                       ds.rate = ds.rate,
                       #di.rate = 1/122,
                       #di.rand = TRUE,
                       sims.per.nw = 2,
                       nsteps = 300,
                       stats = TRUE,
                       stats.form = dx.stats,
                       save.trans = TRUE,
                       save.network = TRUE,
                       verbose = TRUE,
                       prog.plot = TRUE)


par(mar = c(3, 3, 1, 1), mgp = c(2, 1, 0))
plot(sim, ptype = "sim", compart = c("i.num", "i.num.m2"), popfrac = F, sim.lines = T, 
     sim.col = c("blue", "red"), sim.alpha = 0.5)

plot(sim, ptype = "sim", compart = c("si.flow", "si.flow.m2"), sim.lines = FALSE, 
     qnts = NULL, popfrac = F, ylim = c(0, 3), leg = T)

plot(sim, ptype = "network", at = 1, col.inf = T, shp.bip = T)

lowest.sim.at.250 <- which.min(sim$i.num[250, ])
highest.sim.at.250 <- which.max(sim$i.num[250, ])

plot(sim, ptype = "network", at = 250, sim = lowest.sim.at.250, col.inf = T, 
     shp.bip = T)
mtext(paste("Prev =", sim$i.num[250, lowest.sim.at.250]))

plot(sim, ptype = "network", at = 250, sim = highest.sim.at.250, col.inf = T, 
     shp.bip = T)
mtext(paste("Prev =", sim$i.num[250, highest.sim.at.250]))



sim2 <- epiNet.simTrans(est,
                        vital = TRUE,
                        i.prev = i.prev,
                        i.rand = TRUE, 
                        trans.rate = trans.rate,
                        trans.rate.m2 = trans.rate.m2,
                        b.rate = b.rate,
                        ds.rate = ds.rate,
                        di.rate = 1/122,
                        di.rand = TRUE,
                        sims.per.nw = 2,
                        nsteps = 300,
                        stats = TRUE,
                        stats.form = dx.stats,
                        save.trans = TRUE,
                        save.network = TRUE,
                        verbose = TRUE,
                        prog.plot = TRUE)


# Lab 

num.m1 <- 500 # females
num.m2 <- 500 # males
nw <- network.initialize(num.m1 + num.m2, bipartite = num.m1, directed = F)
# Assuming no same-sex partnerships in this model for simplicity

deg.dist.m1 <- c(0.223, 0.732, 0.045, 0.001)
deg.dist.m2 <- c(0.798, 0.79, 0.018, 0.001)
bip.degdist.check(num.m1, num.m2, deg.dist.m1, deg.dist.m2)

formation <- ~edges + b1degree(0:1) + b2degree(0:1)
target.stats <- c(413, 112, 366, 399, 395) # use degdist.check to get Deg0 and Deg1 values for m1 and m2
dissolution <- ~offset(edges)
constraints <- ~bd(maxout = 3)

duration <- 20
coef.diss <- diss.coefs(dissolution, duration)
coef.diss

dx.stats <- ~ edges + b1degree(0:5) + b2degree(0:5)
est <- epiNet.est(nw, 
                  formation, 
                  dissolution, 
                  target.stats, 
                  coef.diss, 
                  constraints, 
                  carnegie=TRUE, 
                  stats=TRUE, 
                  stats.form=dx.stats)


i.prev=0.1
i.rand=TRUE 
trans.rate <- c(rep(0.2055, 3),
                rep(0.0088, 100),
                rep(0.0614, 9),
                rep(0, 10))
trans.rate <- trans.rate*4
trans.rate.m2 <- trans.rate/2
b.rate=0.0066
ds.rate=0.0025

sim <- epiNet.simTrans(est,
                       vital = TRUE,
                       i.prev = i.prev,
                       i.rand = TRUE, 
                       trans.rate = trans.rate,
                       trans.rate.m2 = trans.rate.m2,
                       b.rate = b.rate,
                       ds.rate = ds.rate,
                       #di.rate = 1/122,
                       #di.rand = TRUE,
                       sims.per.nw = 2,
                       nsteps = 300, #300
                       stats = TRUE,
                       stats.form = dx.stats,
                       save.trans = TRUE,
                       save.network = TRUE,
                       verbose = TRUE,
                       prog.plot = TRUE)


par(mar = c(3, 3, 1, 1), mgp = c(2, 1, 0))
plot(sim, ptype = "sim", compart = c("i.num", "i.num.m2"), popfrac = F, sim.lines = T, 
     sim.col = c("blue", "red"), sim.alpha = 0.5)

require(ndtv)
nw <- sim$network$sim1
nw <- recodeTEA(nw)

slice.par <- list(start = 1, end = 30, interval = 1, aggregate.dur = 1, rule = "any")
compute.animation(nw, slice.par = slice.par, animation.mode = "MDSJ")

render.par=list(tween.frames = 10, 
                show.time = FALSE)
plot.par=list(mar=c(0,0,0,0))
render.animation(nw, 
                 render.par = render.par, 
                 plot.par = plot.par,
                 vertex.cex = 0.9, 
                 vertex.col = 'ndtvcol', 
                 edge.col = 'darkgrey', 
                 vertex.border = 'lightgrey', 
                 displaylabels = FALSE)



