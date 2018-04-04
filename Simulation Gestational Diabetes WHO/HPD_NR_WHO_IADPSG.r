# HPD - Highest Posterior Density (HPD) Interval

#-----8<-----
# Using boa package
ans <- boa.hpd(resultado.final[,i], alpha = 0.05)
write.csv2( ans , paste("HPD ",i,".csv", sep=""))
print( ans )

#-----8<-----

# [1] "Computing HPD Interval for NR.LGA"
# Lower Bound Upper Bound 
# 0.08768426  0.09717933 
# [1] "Computing HPD Interval for NR.PE"
# Lower Bound Upper Bound 
# 0.02852109  0.06567756 
# [1] "Computing HPD Interval for NR.CS"
# Lower Bound Upper Bound 
# 0.09675619  0.28645520 
# [1] "Computing HPD Interval for WHO.LGA"
# Lower Bound Upper Bound 
# 0.08496027  0.09427048 
# [1] "Computing HPD Interval for WHO.PE"
# Lower Bound Upper Bound 
# 0.02790777  0.06400733 
# [1] "Computing HPD Interval for WHO.CS"
# Lower Bound Upper Bound 
# 0.09531015  0.28389315 
# [1] "Computing HPD Interval for IADPSG.LGA"
# Lower Bound Upper Bound 
# 0.08278797  0.09268037 
# [1] "Computing HPD Interval for IADPSG.PE"
# Lower Bound Upper Bound 
# 0.02743658  0.06309003 
# [1] "Computing HPD Interval for IADPSG.CS"
# Lower Bound Upper Bound 
# 0.09516766  0.28336441 
