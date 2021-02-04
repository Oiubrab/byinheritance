# Mainline
## Introduction

This is a readme to explain the madness you see in the file system before you. I have created life and so am a god. Anyway, this is a network that runs on a binary shifting-odds concept. Each bit (a 1 moving throught the network) is commanded by an array of *variable probability definitions*, defined by **numerical buckets**

## Branch

This is the main trunk and is currently being developed to work with stock market data so that data can be fed in and the system will learn to develop investment strategies that will maximise cash flow.

**Note:** Setting it to maximise total cash is not the way to go as the network will eventually learn to lock the user out of the account

## Status

Currently, the Network kernel is functional. The network kernel is run twice in an opencoarray format with two networks - the thinking part and the motivation control part. The motivation part has not been written yet so no strategies will be developed, but data can be fed into the network and out of the network. Software to port in stock data and use network output to make trades has not yet been written.

## Concepts and Examples

### Bucket

#### Selector

Random number: 0.28

#### Buckets

(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

#### Reality

As the **Random Number** falls between 0.2 and 0.3, it lies in the **Second Bucket**. This means that the data in this node of the network will feed in to the second route

### Routes

| 1 | 2 | 3 |
| - | - | - |
| 4 | x | 5 |
| 6 | 7 | 8 |

The table above shows the mapping for each node's routes. The x representes the node in question - the *home* node - and the numbered positions represent nodes adjacent to the home node. For instance, if route 1 is selected, the bit in the home node will move left one space and up one space in the network
