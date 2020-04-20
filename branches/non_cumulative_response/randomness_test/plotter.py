import matplotlib.pyplot as plt
import numpy

f = open('random_test.txt', 'r+')
lines = [float(line[0:17]) for line in f.readlines()]
f.close()

random_array = numpy.asarray(lines)

print(lines)
print(random_array)
print(type(random_array[1]))

#plot a histogram of the random variable
plt.hist(random_array,bins=8)
plt.title('Histogram of selection')
plt.xlabel('selection')
plt.ylabel('number')
plt.show()
