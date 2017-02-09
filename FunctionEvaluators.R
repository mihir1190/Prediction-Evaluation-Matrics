function_eval <-
function(a,m)
{metrics  = c(MAD = 0,MSE = 0,MAPE = 0, MPSE = 0, R2 = 0,TMAD = 0)

metrics
metrics["MAD"] = mean(abs(a-m))
metrics["MSE"] = mean((a-m)^2)
metrics["MAPE"] = mean(abs((a-m)/a))
metrics["MPSE"] = mean(((a-m)/a)^2)
metrics["TMAD"] = mean(abs(a-m),trim = .05)
SST = mean((a-mean(a))^2)
metrics["R2"] = (1-metrics["MSE"]/SST)
return (metrics)
}
