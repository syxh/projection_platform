
mu_local = function(dat,pars)
{
  foo0=pars$a0
  foo1=pars$a1*dcumsum(dat$cases_imported,pars$duration1)*(dat$doy<100)
  foo2=pars$a2*dcumsum(dat$cases_community,pars$duration2) # day 100 is 9 Apr, hotel based SHN 
  foo3=pars$a3*dcumsum(dat$cases_dorms,pars$duration3) # day 100 is 9 Apr, hotel based SHN 
  cb = as.numeric((dat$doy>=98)*(dat$doy<=153))
  foo2[cb==1] = foo2[cb==1]*pars$cbe
  foo3[cb==1] = foo3[cb==1]*pars$cbe
  output = foo0+foo1+foo2+foo3
  return(output)
}

