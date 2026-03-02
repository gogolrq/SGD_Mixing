# SGDMean - Mean estimation
# Y: input scalar
# a: batch size parameter Bt=t^a
# b1: initial value
# B: bootstrap size
SGDMean = function(Y, a = 0, b1, B = 500) {
  
  n = length(Y)
  # gradient
  dl = function(theta, x, y) {
    s = -(y - sum(x * theta)) * x
    return(s)
  }
  
  p=1
  n = length(Y)
  X = rep(1, n)
  b2 = b1 # initial value for point estimates
  
  bstar1 = rep(b1, B) # boostrap samples a
  bstar1 = matrix(bstar1, nrow = B, byrow = T)
  abstar1 = rep(0, B * p)
  abstar1 = matrix(abstar1, nrow = B)
  
  bstar2 = rep(b1, B) # boostrap samples b
  bstar2 = matrix(bstar2, nrow = B, byrow = T)
  abstar2 = rep(0, B * p)
  abstar2 = matrix(abstar2, nrow = B)
  
  Bt = 1 # batch size
  ab1 = 0 # average for trajectory a
  ab2 = 0 # average for trajectory b
  index = 0
  Bt.sim = c() # collection of batch sizes
  t = 0
  
  # if a=0, use vanilla SGD, not alternate blocks
  if (a == 0) {
    while (t < n) {
      t = t + 1
      gamma = (t + 10)^(-2 / 3)
      
      gd = 0
      gdstar = 0
      b = b1
      x = X[t]
      y = Y[t]
      gd = gd + dl(b, x, y)
      gdstar = gdstar + sapply(bstar1, dl, x = x, y = y)
      b1 = b1 - gamma * gd # update point estimate
      u = rexp(B)
      gdstar = gdstar * u
      bstar1 = bstar1 - gamma * gdstar # update bootstrap estimate, only once for vanilla SGD
      ab1 = ab1 + b1
      abstar1 = abstar1 + bstar1
    }
    ab1 = ab1 / t
    abstar1 = abstar1 / t
    ab = ab1
    abstar = abstar1
  } else{
    while (index + 2 * Bt <= n) {
      # if a>0, use alternate blocks
      t = t + 1
      gamma = (t + 10)^(-2 / 3)
      Bt = max(floor(t^a), 1)
      
      gd = 0
      gdstar = 0
      b = b1
      
      # calculate gradients
      for (i in 1:Bt) {
        index = index + 1
        x = X[index]
        y = Y[index]
        gd = gd + dl(b, x, y)
        gdstar = gdstar + apply(bstar1, 1, dl, x = x, y = y)
      }
      
      
      gd = gd / Bt
      b1 = b1 - gamma * gd # update point estimate a
      u = rexp(B) # standard exponential weight
      gdstar = gdstar / Bt
      gdstar = gdstar * u
      bstar1 = bstar1 - gamma * gdstar # update bootstrap estimates a
      ab1 = ab1 + b1 * Bt
      abstar1 = abstar1 + bstar1 * Bt
      
      
      gd = 0
      gdstar = 0
      b = b2 # point estimate b
      # calculate gradients
      for (i in 1:Bt) {
        index = index + 1
        x = X[index]
        y = Y[index]
        gd = gd + dl(b, x, y)
        gdstar = gdstar + apply(bstar2, 1, dl, x = x, y = y)
      }
      gd = gd / Bt
      b2 = b2 - gamma * gd # update point estimate b
      gdstar = gdstar / Bt
      gdstar = gdstar * u
      bstar2 = bstar2 - gamma * gdstar # update bootstrap estimates b
      ab2 = ab2 + b2 * Bt
      abstar2 = abstar2 + bstar2 * Bt
      Bt.sim = c(Bt.sim, Bt)
      
    }
    ab1 = ab1 / sum(Bt.sim) # weight average for trajectory a
    ab2 = ab2 / sum(Bt.sim) # weight average for trajectory b
    abstar1 = abstar1 / sum(Bt.sim) # weight average for bootstrap trajectory a
    abstar2 = abstar2 / sum(Bt.sim) # weight average for bootstrap trajectory b
    ab = (ab1 + ab2) / 2 # average of two trajectories
    abstar = (abstar1 + abstar2) / 2 # average of two bootstrap trajectories
  }
  
  # return avg bootstrap, avg point estimates, number of iterations
  return(list(abstar = abstar, ab = ab, t = t))
}



# SGDMedian - Median estimation
# Y: input scalar
# a: batch size parameter Bt=t^a
# b1: initial value
# B: bootstrap size
SGDMedian = function(Y, a = 0, b1, B = 500) {
  
  n = length(Y)
  # gradient
  dl = function(theta, x, y) {
    s = -sign(y - sum(x * theta)) * x
    return(s)
  }
  
  p = 1
  n = length(Y)
  X = rep(1, n)
  b2 = b1 # initial value for point estimates
  
  bstar1 = rep(b1, B) # boostrap samples a
  bstar1 = matrix(bstar1, nrow = B, byrow = T)
  abstar1 = rep(0, B * p)
  abstar1 = matrix(abstar1, nrow = B)
  
  bstar2 = rep(b1, B) # boostrap samples b
  bstar2 = matrix(bstar2, nrow = B, byrow = T)
  abstar2 = rep(0, B * p)
  abstar2 = matrix(abstar2, nrow = B)
  
  Bt = 1 # batch size
  ab1 = 0 # average for trajectory a
  ab2 = 0 # average for trajectory b
  index = 0
  Bt.sim = c() # collection of batch sizes
  t = 0
  
  # if a=0, use vanilla SGD, not alternate blocks
  if (a == 0) {
    while (t < n) {
      t = t + 1
      gamma = (t + 10)^(-2 / 3)
      
      gd = 0
      gdstar = 0
      b = b1
      x = X[t]
      y = Y[t]
      gd = gd + dl(b, x, y)
      gdstar = gdstar + sapply(bstar1, dl, x = x, y = y)
      b1 = b1 - gamma * gd # update point estimate
      u = rexp(B)
      gdstar = gdstar * u
      bstar1 = bstar1 - gamma * gdstar # update bootstrap estimate, only once for vanilla SGD
      ab1 = ab1 + b1
      abstar1 = abstar1 + bstar1
    }
    ab1 = ab1 / t
    abstar1 = abstar1 / t
    ab = ab1
    abstar = abstar1
  } else{
    while (index + 2 * Bt <= n) {
      # if a>0, use alternate blocks
      t = t + 1
      gamma = (t + 10)^(-2 / 3)
      Bt = max(floor(t^a), 1)
      
      gd = 0
      gdstar = 0
      b = b1
      
      # calculate gradients
      for (i in 1:Bt) {
        index = index + 1
        x = X[index]
        y = Y[index]
        gd = gd + dl(b, x, y)
        gdstar = gdstar + apply(bstar1, 1, dl, x = x, y = y)
      }
      
      
      gd = gd / Bt
      b1 = b1 - gamma * gd # update point estimate a
      u = rexp(B) # standard exponential weight
      gdstar = gdstar / Bt
      gdstar = gdstar * u
      bstar1 = bstar1 - gamma * gdstar # update bootstrap estimates a
      ab1 = ab1 + b1 * Bt
      abstar1 = abstar1 + bstar1 * Bt
      
      
      gd = 0
      gdstar = 0
      b = b2 # point estimate b
      # calculate gradients
      for (i in 1:Bt) {
        index = index + 1
        x = X[index]
        y = Y[index]
        gd = gd + dl(b, x, y)
        gdstar = gdstar + apply(bstar2, 1, dl, x = x, y = y)
      }
      gd = gd / Bt
      b2 = b2 - gamma * gd # update point estimate b
      gdstar = gdstar / Bt
      gdstar = gdstar * u
      bstar2 = bstar2 - gamma * gdstar # update bootstrap estimates b
      ab2 = ab2 + b2 * Bt
      abstar2 = abstar2 + bstar2 * Bt
      Bt.sim = c(Bt.sim, Bt)
      
    }
    ab1 = ab1 / sum(Bt.sim) # weight average for trajectory a
    ab2 = ab2 / sum(Bt.sim) # weight average for trajectory b
    abstar1 = abstar1 / sum(Bt.sim) # weight average for bootstrap trajectory a
    abstar2 = abstar2 / sum(Bt.sim) # weight average for bootstrap trajectory b
    ab = (ab1 + ab2) / 2 # average of two trajectories
    abstar = (abstar1 + abstar2) / 2 # average of two bootstrap trajectories
  }
  
  # return avg bootstrap, avg point estimates, number of iterations
  return(list(abstar = abstar, ab = ab, t = t))
}

# SGDOLS - Linear regression
# Y: response
# X: predictors
# a: batch size parameter Bt=t^a
# b1: initial value
# B: bootstrap size
SGDOLS = function(X, Y, a = 0, b1, B = 500) {
  
  n = length(Y)
  # gradient
  dl = function(theta, x, y) {
    s = -(y - sum(x * theta)) * x
    return(s)
  }
  
  # Batch size=1 for the first 500 batches to warm start
  npilot = 500
  t = 0
  while (t < npilot) {
    t = t + 1
    gamma = 0.05
    gd = 0
    b = b1
    x = X[t, ]
    y = Y[t]
    gd = gd + dl(b, x, y)
    b1 = b1 - gamma * gd
  }
  
  p = 3
  b2 = b1 # initial value for point estimates
  
  bstar1 = rep(b1, B) # boostrap samples a
  bstar1 = matrix(bstar1, nrow = B, byrow = T)
  abstar1 = rep(0, B * p)
  abstar1 = matrix(abstar1, nrow = B)
  
  bstar2 = rep(b1, B) # boostrap samples b
  bstar2 = matrix(bstar2, nrow = B, byrow = T)
  abstar2 = rep(0, B * p)
  abstar2 = matrix(abstar2, nrow = B)
  
  Bt = 1 # batch size
  ab1 = 0 # average for trajectory a
  ab2 = 0 # average for trajectory b
  index = 0
  Bt.sim = c() # batch sizes
  t = 0
  
  
  
  while (index + 2 * Bt <= n) {
    t = t + 1
    gamma = (t + 10)^(-2 / 3)
    Bt = max(floor(t^a), 1)
    
    gd = 0
    gdstar = 0
    b = b1 # point estimate a
    # calculate gradients
    for (i in 1:Bt) {
      index = index + 1
      x = X[index, ]
      y = Y[index]
      gd = gd + dl(b, x, y)
      gdstar = gdstar + apply(bstar1, 1, dl, x = x, y = y)
    }
    gd = gd / Bt
    b1 = b1 - gamma * gd # update point estimate a
    u = rexp(B) # standard exponential weight
    gdstar = t(gdstar) / Bt
    gdstar = sweep(gdstar, 1, u, "*")
    bstar1 = bstar1 - gamma * gdstar # update bootstrap estimates a
    ab1 = ab1 + b1 * Bt
    abstar1 = abstar1 + bstar1 * Bt
    
    
    gd = 0
    gdstar = 0
    b = b2
    # calculate gradients
    for (i in 1:Bt) {
      index = index + 1
      x = X[index, ]
      y = Y[index]
      gd = gd + dl(b, x, y)
      gdstar = gdstar + apply(bstar2, 1, dl, x = x, y = y)
    }
    gd = gd / Bt
    b2 = b2 - gamma * gd # update point estimate b
    gdstar = t(gdstar) / Bt
    gdstar = sweep(gdstar, 1, u, "*")
    bstar2 = bstar2 - gamma * gdstar # update bootstrap estimates b
    ab2 = ab2 + b2 * Bt
    abstar2 = abstar2 + bstar2 * Bt
    Bt.sim = c(Bt.sim, Bt)
  }
  ab1 = ab1 / sum(Bt.sim) # weight average for trajectory a
  ab2 = ab2 / sum(Bt.sim) # weight average for trajectory b
  abstar1 = abstar1 / sum(Bt.sim) # weight average for bootstrap trajectory a
  abstar2 = abstar2 / sum(Bt.sim) # weight average for bootstrap trajectory b
  ab = (ab1 + ab2) / 2 # average of two trajectories
  abstar = (abstar1 + abstar2) / 2 # average of two bootstrap trajectories
  
  # return avg bootstrap, avg point estimates, iterations, batch sizes
  return(list(
    abstar = abstar,
    ab = ab,
    t = t,
    Bt.sim = Bt.sim
  ))
}


# SGDLAD - Least absolute deviation regression
# Y: response
# X: predictors
# a: batch size parameter Bt=t^a
# b1: initial value
# B: bootstrap size
SGDLAD = function(X, Y, a = 0, b1, B = 500) {
  
  n = length(Y)
  # gradient
  dl = function(theta, x, y) {
    s = -sign(y - sum(x * theta)) * x
    return(s)
  }
  
  # Batch size=1 for the first 500 batches to warm start
  npilot = 500
  t = 0
  while (t < npilot) {
    t = t + 1
    gamma = 0.05
    gd = 0
    b = b1
    x = X[t, ]
    y = Y[t]
    gd = gd + dl(b, x, y)
    b1 = b1 - gamma * gd
  }
  
  p = 3
  b2 = b1 # initial value for point estimates
  
  bstar1 = rep(b1, B) # boostrap samples a
  bstar1 = matrix(bstar1, nrow = B, byrow = T)
  abstar1 = rep(0, B * p)
  abstar1 = matrix(abstar1, nrow = B)
  
  bstar2 = rep(b1, B) # boostrap samples b
  bstar2 = matrix(bstar2, nrow = B, byrow = T)
  abstar2 = rep(0, B * p)
  abstar2 = matrix(abstar2, nrow = B)
  
  Bt = 1 # batch size
  ab1 = 0 # average for trajectory a
  ab2 = 0 # average for trajectory b
  index = 0
  Bt.sim = c() # batch sizes
  t = 0
  
  
  
  while (index + 2 * Bt <= n) {
    t = t + 1
    gamma = (t + 10)^(-2 / 3)
    Bt = max(floor(t^a), 1)
    
    gd = 0
    gdstar = 0
    b = b1 # point estimate a
    # calculate gradients
    for (i in 1:Bt) {
      index = index + 1
      x = X[index, ]
      y = Y[index]
      gd = gd + dl(b, x, y)
      gdstar = gdstar + apply(bstar1, 1, dl, x = x, y = y)
    }
    gd = gd / Bt
    b1 = b1 - gamma * gd # update point estimate a
    u = rexp(B) # standard exponential weight
    gdstar = t(gdstar) / Bt
    gdstar = sweep(gdstar, 1, u, "*")
    bstar1 = bstar1 - gamma * gdstar # update bootstrap estimates a
    ab1 = ab1 + b1 * Bt
    abstar1 = abstar1 + bstar1 * Bt
    
    
    gd = 0
    gdstar = 0
    b = b2
    # calculate gradients
    for (i in 1:Bt) {
      index = index + 1
      x = X[index, ]
      y = Y[index]
      gd = gd + dl(b, x, y)
      gdstar = gdstar + apply(bstar2, 1, dl, x = x, y = y)
    }
    gd = gd / Bt
    b2 = b2 - gamma * gd # update point estimate b
    gdstar = t(gdstar) / Bt
    gdstar = sweep(gdstar, 1, u, "*")
    bstar2 = bstar2 - gamma * gdstar # update bootstrap estimates b
    ab2 = ab2 + b2 * Bt
    abstar2 = abstar2 + bstar2 * Bt
    Bt.sim = c(Bt.sim, Bt)
  }
  ab1 = ab1 / sum(Bt.sim) # weight average for trajectory a
  ab2 = ab2 / sum(Bt.sim) # weight average for trajectory b
  abstar1 = abstar1 / sum(Bt.sim) # weight average for bootstrap trajectory a
  abstar2 = abstar2 / sum(Bt.sim) # weight average for bootstrap trajectory b
  ab = (ab1 + ab2) / 2 # average of two trajectories
  abstar = (abstar1 + abstar2) / 2 # average of two bootstrap trajectories
  
  # return avg bootstrap, avg point estimates, iterations, batch sizes
  return(list(
    abstar = abstar,
    ab = ab,
    t = t,
    Bt.sim = Bt.sim
  ))
}