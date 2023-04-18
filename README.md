# A python script to generate LFR benchmark network.

Lancichinetti–Fortunato–Radicchi benchmark (LFR) is an algorithm that generates benchmark networks (artificial networks that resemble real-world networks). They have a priori known communities and are used to compare different community detection methods. The advantage of the benchmark over other methods is that it accounts for the heterogeneity in the distributions of node degrees and of community sizes. More info: https://en.wikipedia.org/wiki/Lancichinetti%E2%80%93Fortunato%E2%80%93Radicchi_benchmark

The code is in Python, unsing networkX library:
https://networkx.org/documentation/stable/reference/generated/networkx.generators.community.LFR_benchmark_graph.html

With the given parameters, there are 36 communities.
Mixing parameter mu ranges from 0.05 to 0.95.

results.csv shows the results of comparison between
- true labels from LFR
- labels identified by Louvain community detection over 100 iterations (a distribution, summarized by mean and std)
- labels identified by "consensus community detection" (a single result, based on repeated Louvain and pairwise comparison)



