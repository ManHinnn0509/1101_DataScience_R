csv = read.csv('table_v2.csv', fileEncoding='utf-8')

# 3 Graphs:
# a <-> b
# a <-> c
# b <-> c

# --- 2(a)
scores_TA1 = csv$Score..TA_1.
scores_TA2 = csv$Score..TA_2.
scores_TA3 = csv$Score..TA_3.

# BOXPLOT
df <- data.frame(
	values=c(scores_TA1, scores_TA2, scores_TA3),
	vars=rep(c('TA1', 'TA2', 'TA3'))
)
boxplot(values ~ vars, data=df)

# DENSITY FUNCTION
plot(density(x=scores_TA1, bw=1, na.rm=T), col='red')
lines(density(x=scores_TA2, bw=1, na.rm=T), col='green')
lines(density(x=scores_TA3, bw=1, na.rm=T), col='blue')
legend('topright', c('TA1', 'TA2', 'TA3'), col=c('red', 'green', 'blue'), cex=0.2)

# ECDF
plot(
	ecdf(scores_TA1),
	main='Scores of TA 1, 2, 3',
	col='red'
)

lines(
	ecdf(scores_TA2),
	col='green'
)

lines(
	ecdf(scores_TA3),
	col='blue'
)

legend(
	'topleft',
	c('TA1', 'TA2', 'TA3'),
	pch=19,
	col=c('red', 'green', 'blue'),
	cex=0.35
)

# legend() works but the size & ratio looks weird in RStudio...

# --- 2(b)

# *: Let's assume that they have common items..
#    But they don't, due to bad design :/
#    Happens when TA1 <-> TA3

# BOXPLOT
# 1 <-> 2
temp <- data.frame(
	values=c(scores_TA1[1:5], scores_TA2[1:5]),
	vars=rep(c('TA1', 'TA2'))
)
boxplot(values ~ vars, data=temp)

# 1 <-> 3 *
temp <- data.frame(
	values=c(scores_TA1[1:5], scores_TA3[11:15]),
	vars=rep(c('TA1', 'TA3'))
)
boxplot(values ~ vars, data=temp)

# 2 <-> 3
temp <- data.frame(
	values=c(scores_TA2[11:15], scores_TA3[11:15]),
	vars=rep(c('TA2', 'TA3'))
)
boxplot(values ~ vars, data=temp)

# DENSITY FUNCTION
# 1 <-> 2
plot(density(x=scores_TA1[1:5], bw=1, na.rm=T), col='red')
lines(density(x=scores_TA2[1:5], bw=1, na.rm=T), col='green')

# 1 <-> 3
plot(density(x=scores_TA1[1:5], bw=1, na.rm=T), col='red')
lines(density(x=scores_TA3[11:15], bw=1, na.rm=T), col='blue')

# 2 <-> 3
plot(density(x=scores_TA2[11:15], bw=1, na.rm=T), col='green')
lines(density(x=scores_TA3[11:15], bw=1, na.rm=T), col='blue')

# ECDF

# 1 <-> 2
plot(ecdf(scores_TA1[1:5]), main='TA1 <-> TA2', col='red')
lines(ecdf(scores_TA2[1:5]), col='green')

# 1 <-> 3 *
plot(ecdf(scores_TA1[1:5]), main='TA1 <-> TA3', col='red')
lines(ecdf(scores_TA3[11:15]), col='blue')

# 2 <-> 3
plot(ecdf(scores_TA2[11:15]), main='TA2 <-> TA3', col='green')
lines(ecdf(scores_TA3[11:15]), col='blue')

# --- 2(c)

# 1 <-> 2
plot(scores_TA1[1:5], col='red')
points(scores_TA2[1:5], col='green')

# 1 <-> 3 *
plot(scores_TA1[1:5], col='red')
points(scores_TA3[11:15], col='blue')

# 2 <-> 3
plot(scores_TA2[11:15], col='green')
points(scores_TA3[11:15], col='blue') # HUH? This line isn't working?

# --- 2(d)

drawCor <- function(x, y) {
	corValue = round(cor(x, y), 1)
	title = paste('Correlation: ', corValue)
	
	# Creating the plot
	plot(x, y, pch = 19, col="lightblue", main=title)
	
	# Regression line
	abline(lm(y ~ x), col="red", lwd=3)
}

# 0.7
drawCor(scores_TA1[1:5], scores_TA2[1:5])

# 0.3 ~ 0.7
drawCor(scores_TA1[6:10], scores_TA2[6:10])

# 0.1 ~ 0.3 *
drawCor(scores_TA1[1:5], scores_TA3[11:15])

# -0.1 ~ 0.1
drawCor(scores_TA2[11:15], scores_TA3[11:15])
