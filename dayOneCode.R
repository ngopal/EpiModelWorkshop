

1) Susceptible-Infected-Susceptible Model
epiDCM version

Working


out.SIS.DCM <- epiDCM(type = "SIS", groups = 2, s.num = 500, i.num = 1, trans.rate = 0.2, 
    act.rate = 0.25, rec.rate = 0.01, s.num.g2=501, i.num.g2=1, trans.rate.g2=0.2, rec.rate.g2 = 0.01, balance="g1", nsteps = 500)
    
plot(out.SIS.DCM, axs = "r", leg = "full", main = "SIS DCM two groups", 
      sub = "Plotting:  Infected")

1.1) trans.rate.g2=0.4

out.SIS.DCM <- epiDCM(type = "SIS", groups = 2, s.num = 500, i.num = 1, trans.rate = 0.2, 
    act.rate = 0.25, rec.rate = 0.01, s.num.g2=501, i.num.g2=1, trans.rate.g2=0.4, rec.rate.g2 = 0.01, balance="g1", nsteps = 500)
    
plot(out.SIS.DCM, axs = "r", leg = "full", main = "SIS DCM two groups", 
      sub = "Plotting:  Infected")

1.2) trans.rate.g2=0.8

out.SIS.DCM <- epiDCM(type = "SIS", groups = 2, s.num = 500, i.num = 1, trans.rate = 0.2, 
    act.rate = 0.25, rec.rate = 0.01, s.num.g2=501, i.num.g2=1, trans.rate.g2=0.8, rec.rate.g2 = 0.01, balance="g1", nsteps = 500)
    
plot(out.SIS.DCM, axs = "r", leg = "full", main = "SIS DCM two groups", 
      sub = "Plotting:  Infected")
      
      

# Another SIS epiDCM Model
# Notes: There is a bug with balance = 'g2'. Use 'g1' for now. Also, we can't have two act.rates. Just use act.rate for group 1 for now.

lab.SIS.DCM <- epiDCM(type = "SIS", groups = 2, s.num = 500, i.num = 1, trans.rate = 0.2, rec.rate = (1:5)/100, nsteps = 500, balance = 'g1', s.num.g2 = 500, i.num.g2 = 1, act.rate = 0.5, trans.rate.g2 = 0.4)

# Playing with recovery rates. Group 1 and 2 can have different recovery rates. If recovery rate is high for any group then we reach equilibrium at some point (or the dotted lines might not even intersect the solid lines). Reaches a fixed prevalence.

lab.SIS.DCM <- epiDCM(type = "SIS", groups = 2, s.num = 500, i.num = 1, trans.rate = 0.2, 
        rec.rate = 20/100, nsteps = 1000, balance = 'g1', s.num.g2 = 500, i.num.g2 = 1, act.rate = 0.5, trans.rate.g2 = 0.4, rec.rate.g2 = 5/100)

lab.SIS.DCM <- epiDCM(type = "SIS", groups = 2, s.num = 500, i.num = 1, trans.rate = 0.2, 
                  rec.rate = 20/100, nsteps = 500, balance = 'g1', s.num.g2 = 500, i.num.g2 = 1, act.rate = 0.5, trans.rate.g2 = 0.4, rec.rate.g2 = 5/100)




2) Susceptible-Infected-Susceptible Model
epiSPM version


out.SIS.SPM.bip <- epiSPM(type = "SIS", s.num = 500, i.num = 1, trans.rate = 0.2, 
    act.rate = 0.25, rec.rate = 1/100, nsteps = 500, nsims = 25, verbose = F, 
    groups = 2, s.num.g2 = 500, i.num.g2 = 0, trans.rate.g2 = 0.1, rec.rate.g2 = 1/50, 
    balance = "g1")
plot(out.SIS.SPM.bip, compart = c("i.num", "i.num.g2"), leg = T, sub = "Plotting:  Infected")


3)SIS DCM Sensitivity Analysis for recovery rate two groups

out.SIS.DCM <- epiDCM(type = "SIS", groups = 2, s.num = 500, i.num = 1, trans.rate = 0.2, 
    act.rate = 0.25, rec.rate = (1:5)/100, s.num.g2=501, i.num.g2=1, trans.rate.g2=0.2, rec.rate.g2 = 0.01, balance="g1", nsteps = 500)

plot(out.SIS.DCM, axs = "r", leg = "full", main = " SIS DCM Sensitivity Analysis for recovery rate two groups", 
    sub = "Plotting:  Infected")
    
    
    
4) Same Data with DCM and SPM
# General notes: increased transmission rates makes people get infected fast. Recovery rates can be different between 2 groups and have a massive effect on the results.
# Question: Do all models saturate/reach equilibrium at some point?

lab.SIS.DCM <- epiDCM(type = "SIS", groups = 2, s.num = 500, i.num = 1, trans.rate = 0.2, 
rec.rate = 20/100, nsteps = 1000, balance = 'g1', s.num.g2 = 500, i.num.g2 = 1, act.rate = 0.5, trans.rate.g2 = 0.4, rec.rate.g2 = 5/100)

plot(lab.SIS.DCM)

lab.SIS.SPM.bip <- epiSPM(type = "SIS", s.num = 500, i.num = 1, trans.rate = 0.2, 
                          act.rate = 0.5, rec.rate = 20/100, nsteps = 1000, nsims = 25, verbose = F, 
                          groups = 2, s.num.g2 = 500, i.num.g2 = 0, trans.rate.g2 = 0.4, rec.rate.g2 = 5/100, 
                          balance = "g1")
plot(lab.SIS.SPM.bip, compart = c("i.num", "i.num.g2"), leg = T, sub = "Plotting:  Infected")

#Changing the sex ratio blows up the variance on the way to equilibrium...
out.SIS.SPM.bip <- epiSPM(type = "SIS", s.num = 500, i.num = 1, trans.rate = 0.2, 
                          act.rate = 0.5, rec.rate = 20/100, nsteps = 1000, nsims = 25, verbose = F, 
                          groups = 2, s.num.g2 = 500, i.num.g2 = 0, trans.rate.g2 = 0.4, rec.rate.g2 = 5/100, 
                          balance = "g1")
plot(out.SIS.SPM.bip, compart = c("i.num", "i.num.g2"), leg = T, sub = "Plotting:  Infected")


