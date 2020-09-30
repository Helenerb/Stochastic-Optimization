import numpy as np
import random
import matplotlib.pyplot as plt
from Dijkstra import findNeighbours, makeAdjacency, dijkstra

def realize(old_pos, pos, graph):  # Simulating realizations of traffic image
    mean = graph[old_pos, pos]
    return np.random.normal(mean, 1.0)

def getNextPos(position, Q, neighbours):  # Finding the next position based on optimal Q-value
    best_Q = - np.infty
    for n in neighbours:
        if Q[position, n] > best_Q:
            best_Q = Q[position, n]
            new_pos = n
    return new_pos

def findNeigboursArray(node, dim):  # Finding neighbours that returns them in a specified order
    x, y = node//dim, node % dim
    neighbours = []  # remove the 0 in the end

    if y != 0:  # Not on northern edge
        neighbours.append(x*dim + y - 1)
    else:
        neighbours.append(-1)

    if y != (dim-1):  # Not on the southern edge
        neighbours.append(x*dim + y + 1)
    else:
        neighbours.append(-1)

    if x != 0:  # Not on the western edge
        neighbours.append((x - 1)*dim + y)
    else:
        neighbours.append(-1)

    if x != dim - 1:  # Not on the eastern edge
        neighbours.append((x + 1)*dim + y)
    else:
        neighbours.append(-1)

    return neighbours

def qCloseToTarget(dim, target, towards, away_from):  # Optimistic Q-values
    Q = np.zeros((dim**2, dim**2))
    x_target, y_target = target//dim, target % dim

    for node in range(dim**2):
        neighs = findNeigboursArray(node, dim)
        for n in range(4):
            if n == 0 and neighs[n] != -1:  # north
                x, y = neighs[n] // dim, neighs[n] % dim
                if y >= y_target:
                    Q[node, neighs[n]] = towards
                else:
                    Q[node, neighs[n]] = away_from
            if n == 1 and neighs[n] != -1:  # south
                x, y = neighs[n] // dim, neighs[n] % dim
                if y <= y_target:
                    Q[node, neighs[n]] = towards
                else:
                    Q[node, neighs[n]] = away_from
            if n == 2 and neighs[n] != -1:  # west
                x, y = neighs[n] // dim, neighs[n] % dim
                if x >= x_target:
                    Q[node, neighs[n]] = towards
                else:
                    Q[node, neighs[n]] = away_from
            if n == 3 and neighs[n] != -1:  # east
                x, y = neighs[n] // dim, neighs[n] % dim
                if x <= x_target:
                    Q[node, neighs[n]] = towards
                else:
                    Q[node, neighs[n]] = away_from
    return Q

def driveTowardsTarget(equal, target, dim):  # Function that finds which direction brings you closer to target
    x_target, y_target = target//dim, target % dim
    closest_node = None
    shortest_dist = np.infty
    for n in equal:
        x, y = n//dim, n % dim
        if abs(x-x_target) + abs(y-y_target) < shortest_dist:
            closest_node = n
    return closest_node

def getNextPosSH(position, Q, dim, target):  # Finding the next position with the "smart" heuristic
    neighbours = findNeighbours(position, dim)
    best_Q = - np.infty
    for n in neighbours:
        if Q[position, n] > best_Q:
            best_Q = Q[position, n]
            new_pos = n
            equal = [n]
        elif Q[position, n] == best_Q:
            equal.append(n)
    if len(equal) > 0:
        new_pos = driveTowardsTarget(equal, target, dim)
    return new_pos




def qLearning(graph, shortestTimes, dim, target, N, epsilon, alpha, gamma, method):
    # Grahp: the adjacency matrix
    # dim: the dimension of the road network
    # ShortestTimes: the optimal times found by Dijkstra
    # target: the exit
    # N: number of learning steps

    if method == "e-greedy" or method == "smart-heuristic":
        Q = np.zeros((dim**2, dim**2))
    if method == "optimistic":
        Q = qCloseToTarget(dim, target, 1, -5)

    # taking the first step manually:
    old_pos = np.random.randint(0, dim**2)
    while old_pos == target:  # to prevent starting in target
        old_pos = np.random.randint(0, dim ** 2)
    old_neighs = findNeighbours(old_pos, dim)
    position = getNextPos(old_pos, Q, old_neighs)
    time_spent = realize(old_pos, position, graph)
    optimal_time = shortestTimes[old_pos]

    new_pos = None  # initializing to nothing
    relative_driving_times = []  # initialize to empty array
    driving_times = []  # absolute driving times, initialize to empty array
    n_reached_target = []  # at what learning step was target reached, initialize to empty array

    for n in range(N):
        if position == target:  # if last iteration found the target, save information and start over
            relative_driving_times.append(time_spent/optimal_time)
            driving_times.append(time_spent)
            n_reached_target.append(n)
            old_pos = np.random.randint(0, dim**2)
            while old_pos == target:  # To prevent restarting in target node
                old_pos = np.random.randint(0, dim ** 2)
            neighbours = findNeighbours(old_pos, dim)
            position = getNextPos(old_pos, Q, neighbours)
            time_spent = realize(old_pos, position, graph)
            optimal_time = shortestTimes[old_pos]

        try:
            neighbours = findNeighbours(position, dim)
        except:
            neighbours = findNeighbours(position[0], dim) #bugfix, sometimes position becomes a list

        if method == "e-greedy":
            if np.random.uniform(0, 1) < epsilon:
                new_pos = random.sample(neighbours, 1)
            else:
                new_pos = getNextPos(position, Q, neighbours)

        if method == "smart-heuristic":
            if np.random.uniform(0, 1) < epsilon:
                new_pos = random.sample(neighbours, 1)
            else:
                new_pos = getNextPosSH(position, Q, dim, target)

        if method == "optimistic":
            new_pos = getNextPos(position, Q, neighbours)

        # Updating Q:
        rel = realize(old_pos, position, graph)  # realization
        Q[old_pos, position] = gamma*(-rel + alpha*Q[position, new_pos]) + (1-gamma)*Q[old_pos, position]
        time_spent += rel
        if new_pos == target:
            rel = realize(position, new_pos, graph)
            #Giving high reward for finding target value. updating the "next" step as initial value = 0
            Q[position, new_pos] = gamma * (-rel + 200) + (1 - gamma) * Q[position, new_pos]
            time_spent += rel
        old_pos = position
        position = new_pos

    return Q, relative_driving_times, driving_times, n_reached_target

if __name__ == "__main__":
    #graph20 = makeAdjacency(20)
    graph20 = np.loadtxt('graph20.txt', dtype=int)
    shortestTimes20 = np.loadtxt('shortestTime20.txt', dtype=int)
    Qeps, relativeDrivingTimes, drivingTimes, nReachedTarget = qLearning(graph20, shortestTimes20, 20, 4, 100000, 0.05, 0.95, 0.05, "e-greedy")
    QOpt, rDTOptimistic, drTOptimistic, nRTOptimistic = qLearning(graph20, shortestTimes20, 20, 4, 100000, 0.05, 0.95, 0.05, "optimistic")
    QSH, rDTSH, drTSH, nRTSH = qLearning(graph20, shortestTimes20, 20, 4, 100000, 0.05, 0.95, 0.05, "smart-heuristic")

    plt.figure()
    plt.title("All methods")
    plt.plot(nRTOptimistic, rDTOptimistic, label="Relative travel times Optimistic")
    plt.plot(nRTSH, rDTSH, label="Relative travel times Smart Heuristic")
    plt.plot(nReachedTarget, relativeDrivingTimes, label="Relative travel times epsilon greedy")
    plt.xlabel("Number of learning steps")
    plt.ylabel("Travel time relative to optimal travel time")
    plt.legend()
    plt.show()
