s0 <- 1:1440
set.seed(33550336)
jpeg('WIT presentation 2/fakeoutlier.jpg', width=960, height=480)

plot(A_true[1,]+noise_true*(4-tanh(abs(s0-720)/120-3)), col='red', lty=2, lwd=2,
     type='l',
     xlab='Time',ylab='MIMS',
     main='Paradigm of Outlier Detection',ylim=c(0,5))
lines(pmax(A_true[1,]-noise_true*(4+tanh(abs(s0-720)/120-3)), 0), col='red', lty=2, lwd=2)
lines(pmax(A_true[1,]+rnorm(1440,sd=noise_true), 0), col='black')
lines(pmax(A_true[1,]+rnorm(1440,sd=noise_true)+
             dnorm(s0, mean=480,sd=30)/dnorm(480,480,30)*2, 0),
      col='red', lty=2)
lines(pmax(A_true[1,]+rnorm(1440,sd=noise_true)+
             dnorm(s0, mean=1350,sd=30)/dnorm(1350,1350,30)*2, 0),
      col='blue', lty=2)
lines(A_true[1,], ylim=c(0,5), lwd=2, col='green')
legend('topleft',
       lty=c(1,2,1,1,1),
       lwd=c(2,2,1,1,1),
       col=c('green','red','black','red','blue'),
       legend=c('Estimated Mean','Prediction Bounds',
                'Non-outlier','"Morning Shift"','"Night Shift"'))
dev.off()