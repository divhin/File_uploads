Social Network Analysis
================
Divya Hindupur
3/29/2018

Social network analysis \[SNA\] is the mapping and measuring of
relationships and flows between people, groups, organizations,
computers, URLs, and other connected information/knowledge entities. The
nodes in the network are the people and groups while the links show
relationships or ties between the nodes. SNA provides both a visual and
a mathematical analysis of human relationships.

# Using package ‘igraph’ for building social network graphs

igraph is a library and R package for network analysis.The main goals of
the igraph library is to provide a set of data types and functions for
1) pain-free implementation of graph algorithms, 2) fast handling of
large graphs, with millions of vertices and edges, 3) allowing rapid
prototyping via high level languages like R.

``` r
library(igraph)
```

## Importing and combining the datasets:

For this tutorial we will be using the dataset containing information of
the employee network at a company - Krackhardt High-Tech obtained from
Stanford SNA Data. There is a dataset pertaining to each kind of
employee relationship. We import the datasets into R dataframes:

``` r
advice_df = read.table('Krack-High-Tec-edgelist-Advice.txt')
friendship_df = read.table('Krack-High-Tec-edgelist-Friendship.txt')
reports_to_df = read.table('Krack-High-Tec-edgelist-ReportsTo.txt')
attributes = read.csv('Krack-High-Tec-Attributes.csv', header=T)
```

Adding column names to the datasets:

``` r
colnames(advice_df) = c('ego', 'alter', 'advice_tie')
colnames(friendship_df) = c('ego', 'alter', 'friendship_tie')
colnames(reports_to_df) = c('ego', 'alter', 'reports_to_tie')
```

Before we merge these data, we need to make sure ‘ego’ and ‘alter’
columns are are the same across data sets.The command below should
return TRUE for every row if all ego rows are the same for the relation
types - advice and friendship:

``` r
which(advice_df$ego != friendship_df$ego)
which(advice_df$alter != friendship_df$alter)
which(reports_to_df$alter != friendship_df$alter)
which(reports_to_df$ego != friendship_df$ego)
```

we then combine them into a single data frame and rename the
columns:

``` r
krack_all_df = cbind(advice_df, friendship_df$friendship_tie,reports_to_df$reports_to_tie)
names(krack_all_df)[4:5] = c("friendship_tie","reports_to_tie") 
```

Reduce to non-zero edges so that the edge list only contains actual ties
of some
type.

``` r
krack_all_nonzero_edges = subset(krack_all_df,(advice_tie > 0 | friendship_tie > 0 | reports_to_tie > 0))
```

## Loading data into the graph dataframe

Now we can import our data into a “graph” object using igraph’s
graph.data.frame() function. Coercing the data into a graph object is
what allows us to perform network-analysis

``` r
krack_all = graph.data.frame(krack_all_nonzero_edges) 
summary(krack_all)
```

    ## IGRAPH 8a82b04 DN-- 21 232 -- 
    ## + attr: name (v/c), advice_tie (e/n), friendship_tie (e/n),
    ## | reports_to_tie (e/n)

By default, graph.data.frame() treats the first two columns of a data
frame as an edge list and any remaining columns as edge attributes.
Thus, the 232 edges appearing in the summary() output refer to the 232
pairs of vertices that are joined by any type of tie. The tie types
themselves are listed as edge attributes.

To get a vector of edges for a specific type of tie, use the
get.edge.attribute()
    function.

``` r
get.edge.attribute(krack_all, 'advice_tie')
```

    ##   [1] 1 1 1 0 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1
    ##  [36] 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1
    ##  [71] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 1 1
    ## [106] 1 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 1 1 0 1 1 1 1 0 1 1 1 1
    ## [141] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0 0 1 0 0 0 0 0 0
    ## [176] 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1
    ## [211] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

If you would like to symmetrize the network, making all asymmetric ties
symmetric, use the as.undirected() function:

``` r
krack_all_symmetrized = as.undirected(krack_all, mode='collapse')
```

## Adding Vertex Attributes To a Graphs Object

First create a vector of vertex labels and pass it as parameter to the
graph.data.frame object during the creation of the graph:

``` r
attributes = cbind(1:length(attributes[,1]), attributes)
krack_all = graph.data.frame(d = krack_all_nonzero_edges,vertices = attributes)
```

Note that we now have ‘AGE,’ ‘TENURE,’ ‘LEVEL,’ and ‘DEPT’ listed
alongside ‘name’ as vertex attributes.

``` r
summary(krack_all)
```

    ## IGRAPH cb139b7 DN-- 21 232 -- 
    ## + attr: name (v/c), AGE (v/n), TENURE (v/n), LEVEL (v/n), DEPT
    ## | (v/n), advice_tie (e/n), friendship_tie (e/n), reports_to_tie
    ## | (e/n)

We can see a list of the values for a given attribute for all of the
members in the
    network.

``` r
get.vertex.attribute(krack_all, 'AGE')
```

    ##  [1] 33 42 40 33 32 59 55 34 62 37 46 34 48 43 40 27 30 33 32 38 36

## Visualize The Networks

We first plot all the people and relationships in Krackhardt
High-Tech:

``` r
plot(krack_all)
```

![](SocialNetworkAnalysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

This plot is too crowded, so we look at the networks for single edge
types or relationship type
wise.

#### Advice Only

``` r
krack_advice_only = delete.edges(krack_all, E(krack_all)[get.edge.attribute(krack_all,name = "advice_tie") == 0])
plot(krack_advice_only)
```

![](SocialNetworkAnalysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

#### Friendship Only

``` r
krack_friendship_only = delete.edges(krack_all,E(krack_all)[get.edge.attribute(krack_all, name = "friendship_tie") == 0])
plot(krack_friendship_only)
```

![](SocialNetworkAnalysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

#### Reports-To Only

``` r
krack_reports_to_only = delete.edges(krack_all, E(krack_all)[get.edge.attribute(krack_all, name = "reports_to_tie") == 0])
plot(krack_reports_to_only)
```

![](SocialNetworkAnalysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

The graph is still messy, so we clean things up a bit. For simplicity,
we’ll focus on reports\_to ties for now.

We start by optimizing the layput of the plot. Here we use the
fruchterman.reingold layout. Other options are described in the igraph
help page for “layout,” which can be accessed by entering ?layout.

``` r
reports_to_layout = layout.fruchterman.reingold(krack_reports_to_only)
plot(krack_reports_to_only, layout=reports_to_layout)
```

![](SocialNetworkAnalysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

We color-code vertices by department and clean up the plot by removing
vertex labels and shrinking the arrow size

``` r
dept_vertex_colors = get.vertex.attribute(krack_all,"DEPT")
colors = c('darkslateblue', 'cyan', 'dodgerblue', 'blue', 'deepskyblue')
dept_vertex_colors[dept_vertex_colors == 0] = colors[1]
dept_vertex_colors[dept_vertex_colors == 1] = colors[2]
dept_vertex_colors[dept_vertex_colors == 2] = colors[3]
dept_vertex_colors[dept_vertex_colors == 3] = colors[4] 
dept_vertex_colors[dept_vertex_colors == 4] = colors[5]

plot(krack_reports_to_only, layout=reports_to_layout, vertex.color=dept_vertex_colors, vertex.label=NA,edge.arrow.size=.5)
```

![](SocialNetworkAnalysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Now we set the vertex size by tenure.

``` r
tenure_vertex_sizes = get.vertex.attribute(krack_all,"TENURE")
plot(krack_reports_to_only, layout=reports_to_layout, vertex.color=dept_vertex_colors, vertex.label=NA, edge.arrow.size=.5,vertex.size=tenure_vertex_sizes)
```

![](SocialNetworkAnalysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

Now we incorporate additional tie types. We’ll use the layout generated
by the reports-to ties but overlay the advice and friendship ties in red
and blue.

``` r
tie_type_colors = c('gray','darkseagreen1', 'lightgoldenrod')
E(krack_all)$color[ E(krack_all)$advice_tie==1 ] = tie_type_colors[1]
E(krack_all)$color[ E(krack_all)$friendship_tie==1 ] = tie_type_colors[2]
E(krack_all)$color[ E(krack_all)$reports_to_tie==1 ] = tie_type_colors[3]
E(krack_all)$arrow.size=.5 
V(krack_all)$color = dept_vertex_colors
V(krack_all)$frame = dept_vertex_colors
plot(krack_all, layout=reports_to_layout, vertex.color=dept_vertex_colors, vertex.label=NA, edge.arrow.size=.5,vertex.size=tenure_vertex_sizes)

legend(1, 1.25,legend = c('Advice', 'Friendship','Reports To'), col = tie_type_colors, lty=1, cex = .7)
```

![](SocialNetworkAnalysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

Another option for visualizing different network ties relative to one
another is to overlay the edges from one tie type on the structure
generated by another tie type. Here we can use the reports-to layout but
show the friendship
ties:

``` r
plot(krack_friendship_only, layout=reports_to_layout, vertex.color=dept_vertex_colors, vertex.label=NA, edge.arrow.size=.5, vertex.size=tenure_vertex_sizes, main='Krackhardt High-Tech Managers')
```

![](SocialNetworkAnalysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

References: \* <http://igraph.org/r/doc/igraph.pdf> \*
<https://www.r-bloggers.com/network-visualization-in-r-with-the-igraph-package/>
\* <https://sna.stanford.edu/index.php> \*
<http://www.orgnet.com/sna.html>
