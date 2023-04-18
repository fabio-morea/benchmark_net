# benchmark_net
A python script to generate LFR benchmark network
according to:
https://networkx.org/documentation/stable/reference/generated/networkx.generators.community.LFR_benchmark_graph.html

Mixing parameter mu ranging from 0.05 to 0.95

results.csv shows the results of comparison between
- true labels from LFR
- labels identified by Louvain community detection
over 100 iterations


