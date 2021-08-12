# dice function
dice = function(ice_obj, smooth = TRUE){
  
  effect_table = ice_obj$results
  
  if(length(unique(effect_table$.type)) ==1){
    ice = effect_table
  }
  else{
    pdp = effect_table[effect_table$.type == "pdp",]
    ice = effect_table[effect_table$.type == "ice",]
  }
  
  gridpts = sort(unique(ice[,1]))
  
  # dice curves are approximated by calculating the the average slope between neighbouring grid points. 
  # either by ice curves or (by default) first smoothing ice curves. Alternatively: D1ss approximates ice curves by splines,
  # then adds an offset to each grid point as predicts at these points again using the splines to calculate the derivative
  EstimatorWrapper = function(y){
    if(smooth) y = supsmu(x=gridpts,y=y)$y #numerical derivative of supersmooth.alternatively with offset: D1ss(x = gridpts, y = y)#
    D1tr( x = gridpts, y = y)  
  }
  
  #compute derivatives
  dice_obj = list()
  dice_obj$ice_curves = tidyr::pivot_wider(ice, names_from = .id, values_from = .value)
  dice_obj$dice_curves = apply(dice_obj$ice_curves[,-(1:3)], 2, FUN = EstimatorWrapper)
  
  #compute the sd of the derivatives at each gridpt.
  dice_obj$sd_deriv = data.frame("gridpts" = gridpts, ".sd" = apply(dice_obj$dice_curves, 1, sd))
  
  # restructure curves for plotting
  dice_obj$dice_curves = cbind(dice_obj$ice_curves[,1:3], dice_obj$dice_curves)
  dice_obj$dice_curves = tidyr::pivot_longer(dice_obj$dice_curves, cols = 4:ncol(dice_obj$dice_curves), names_to = ".id", values_to = ".value")
  
  # remove ice curves
  dice_obj$ice_curves = NULL
  
  # add derivative for pdp
  dice_obj$dpdp = cbind(pdp[,-which(colnames(pdp)==".value")], ".value" = EstimatorWrapper(pdp$.value))
  
  return(dice_obj)
}

