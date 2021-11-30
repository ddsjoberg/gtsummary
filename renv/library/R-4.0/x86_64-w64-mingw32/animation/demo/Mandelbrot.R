local({
  jet.colors = colorRampPalette(c(
    '#00007F', 'blue', '#007FFF', 'cyan', '#7FFF7F', 'yellow', '#FF7F00', 'red',
    '#7F0000'))
  m = 400
  C = complex(real = rep(seq(-1.8, 0.6, length.out = m), each = m), 
              imag = rep(seq(-1.2, 1.2, length.out = m), m))
  C = matrix(C, m, m)
  Z = 0
  X = array(0, c(m, m, 20))
  for (k in 1:20) {
    Z = Z^2 + C
    X[, , k] = exp(-abs(Z))
  }
  par(mar = rep(0, 4), ann = FALSE)
  for (k in 1:20) {
    dev.hold()
    image(X[, , k])
    ani.pause(0.01)
  }
})
