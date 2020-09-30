import numpy as np
import copy


def findNeighbours(node, dim):  # To find the possible steps from a given crossing
    if type(node) == list:  # bug: sometimes position becomes a list
        if len(node) > 1:
            print('This is weird, the position is now: ', node)
        node = node[0]  # getting first element in list
    x, y = node//dim, node % dim
    neighbours = {0}  # implemented like this to get a set and not a dict

    if y != 0:  # Not on northern edge
        neighbours.add(x*dim + y - 1)

    if y != (dim-1):  # Not on the southern edge
        neighbours.add(x*dim + y + 1)

    if x != 0:  # Not on the western edge
        neighbours.add((x - 1)*dim + y)

    if x != dim - 1:  # Not on the eastern edge
        neighbours.add((x + 1)*dim + y)
    neighbours.remove(0)

    return neighbours


def makeAdjacency(dim=100):  # the function that makes a model of the traffic in the city
    adj = np.zeros((dim**2, dim**2)) # initializing the adjacency matrix, the traffic image
    for node in range(dim**2):
        for n in findNeighbours(node, dim):
            adj[node, n] = np.random.choice(np.linspace(1, 10, 10))  # sampling average driving time for road segment
    return adj


def dijkstra(graph, source_node, target_node):
    provisional_distance = np.ones(len(graph[0]))*np.infty  # distance from source_node to all others, initialized to infinity
    provisional_distance[source_node] = 0  # set distance from source_node to source_node = 0
    queue = {source_node} #Gjør om til array hvis noe blir rart!
    seen_nodes = {-1}  # this is a set, makes searching faster. initialized with -1 because I could not understand how to make it empty
    hops = {source_node: (source_node, )}  # This is a dictionary

    counter = 0
    while target_node not in seen_nodes:
        min_dist = np.infty
        current_node = None
        for node in queue:
            if provisional_distance[node] < min_dist and node not in seen_nodes:
                min_dist = provisional_distance[node]
                current_node = node

        if counter == 0:
            print("current_node som burde være source node: ", current_node)  # DEBUG
        seen_nodes.add(current_node)
        neighbours = findNeighbours(current_node, int(np.sqrt(len(graph[0]))))
        for n in neighbours:
            queue.add(n)
            dist = provisional_distance[current_node] + graph[current_node, n]
            if graph[current_node, n] == 0:
                print('ohnooo du har ikke fått til adjacency for current_node ', current_node, "og nabo ",n)
            if provisional_distance[n] > dist:
                provisional_distance[n] = dist
                curr = hops[current_node]
                try:
                    a = curr + (n, )
                    hops[n] = a
                except:
                    print("You were not able to update hops:(")
                    print("counter: ", counter)
                    print("hops: ", hops)
                    print("curr: ", curr)
                    print("current node: ", current_node, "\n")
        counter += 1
        if counter > 10*len(graph[0]**2):
            print("This is taking too long, what is happening?!?!?!?")
            break

    return hops[target_node], provisional_distance[target_node]

if __name__ == "__main__":
    #TODO: first run this, then change which block is commented out

    Graph20 = makeAdjacency(20)
    np.savetxt('graph20.txt', Graph20, fmt='%d')

    '''
    Graph20 = np.loadtxt('graph20.txt', dtype=int)

    shortest_time = np.zeros(20**2)
    for i in range(20**2):
        hops, dist = dijkstra(Graph20, i, 4)
        shortest_time[i] = dist

    np.savetxt('shortestTime20.txt', shortest_time, fmt='%d')

    #hops, dist = dijkstra(TestGraph, 9900, 19)
    #print(hops, dist)
    '''