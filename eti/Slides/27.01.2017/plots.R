radToDeg <- function(rad) {
    return((rad * 57.2958))
}

R <- 100
C <- 10 * 10^-6
wg <- 1/(R*C)
w <- 10:10^5
w_2 <- w/wg

y <- atan(1/w_2)
y_2 <- atan(- (w/wg))

# Phase R
plot(w,radToDeg(y), log="x", ylim=c(-90, 90), type="l", col="red", ylab="Phase (deg)", 
     xlab="Frequency (rad/s)")

# Phase C
lines(w, radToDeg(y_2), type="l", col="blue")

legend("topright", c("Phase Kondensator", "Phase Widerstand"), 
       col=c("blue", "red"), lwd=1)

# Magnitude R
y_r <- 20 * log10(w_2/sqrt(1+w_2^2))
plot(w, y_r, ylim=c(-50, 10), log="x", type="l", col="red", 
     xlab="Frequency (rad/s)", ylab="Magnitude (dB)")

# Magnitude C
y_c <- 20 * log10(1/sqrt(1+w_2^2))
lines(w, y_c, col="blue")

legend("topright", c("Amplitude Kondensator", "Amplitude Widerstand"), 
       col=c("blue", "red"), lwd=1)

