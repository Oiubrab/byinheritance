import matplotlib.pyplot as plt
import numpy

f = open('random_test.txt', 'r+')
lines = [float(line[0:17]) for line in f.readlines()]
f.close()

random_array = numpy.asarray(lines)

print(random_array)

#plot a histogram of the random variable
plt.hist(random_array,bins=100)
plt.title('Histogram of selection')
plt.xlabel('selection')
plt.ylabel('number')
plt.show()

#plot a histogram of the random variable
plt.hist(random_array,bins=8)
plt.title('Histogram of selection')
plt.xlabel('selection')
plt.ylabel('number')
plt.show()
