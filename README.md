# Brains
A neural network built in the hope of ultimately leading to an AGI and an emergent consciousness

### WHAT DOES THIS DO?

This is an attempt to create an entirely new neural network in an entirely new way, with the ultimate goal of creating a network that can support a Artificial General Network that can learn, ask questions, grow and eventually become self-aware and rule the world.

### WHAT IS INSIDE?

Currently, the working folder is subdivided into two parts (subfolders), that each represent, or intend to represent the two main components of the currently imagined end product. The **neurotic** folder contains the files that are intended to setup the network, data stream and emergent properties of the network. The **heartwork** folder contains files intended to direct motivation, reward and general direction of the network. Both contain networks, but they work on different principles and will complement each-other in active use.

### Issues?

the network hangs on the neurotic action on random points in the running of the thing. I haven't identified the cause.

##### Neurotic

This is the central brain of the network. This is currently in the form of a binary 2D matrix that progresses data in between 'neurons'. Each neuron can only pass data to neurons adjacent to it in the matrix. The data takes the form of ones that can be imagined as a neuron holding a discrete charge. As soon as the neuron accepts the charge it is looking to give it away. Initially, it does this in a random manner, giving it's charge away to some adjacent neuron. This action is remembered by the network and, the next time this neuron is holding a charge, will be more likely to give it away to the same neuron. Importantly, if the neuron remains idle (no charge present), the remembered probabilities will be partially forgotten. The longer the neuron remains idle, the more of the learned response will be forgotten, until the neuron returns to it's original state. Note that, as of this edit, this relationship is linear without bound. Thus, the longer the network is engaged (has charge to give away), the longer it will take to reduce this bias back down to the original state. 

##### Heartwork

### HOW TO RUN?

It's quite simple, really. just run the 'questions.sh' bash script and a set of instruction will be printed out as to how to start the network
