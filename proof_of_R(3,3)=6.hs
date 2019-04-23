import Lemmas_for_R_three_three
import Colored_graph

main = do
  let bear = pigeonhole_principle_at_i (create_Graph 4) 0
  let bear1 = color_edge_blue bear (1,2)
  let bear2 = color_edge_blue bear1 (1,3)
  let bear3 = color_edge_blue bear2 (2,3)
  let graph = create_Graph 6
  let graph1 = pigeonhole_principle_at_i graph 0
  let graph2 = lemma_1_no_red_tri_at_vert_i graph1 0
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn "The crux of the proof is actually the following lemma; \"A red (or"
  putStrLn "blue) bear-claw graph, i.e. a vertex with three edges attached, must"
  putStrLn "contain either a red triangle or a blue triangle.\""
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn "We now prove the first lemma."
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn "W.L.O.G assume it is a red bear claw, which is represented by the"
  putStrLn "following adjacency matrix"
  prin_adj_matrix bear
  putStrLn "where the entry jth column and the ith row is the color of edge (i,j)."
  putStrLn "Also may assume there is no red or blue triangle for a contradiction."
  putStrLn "Notice that if the edge (1,2) is colored red then the vertices [0,1,2]"
  putStrLn "would form a red triangle. Therefore that edge must be colored blue,"
  putStrLn "which leaves us with the following"
  prin_adj_matrix bear1
  putStrLn "Like-wise if the edge (1,3) is colored red then the vertices [0,1,3]"
  putStrLn "would form a red triangle. Therefore that edge must be colored blue,"
  putStrLn "which leaves us with the following"
  prin_adj_matrix bear2
  putStrLn "Like-wise if the edge (2,3) is colored red then the vertices [0,2,3]"
  putStrLn "would form a red triangle. Therefore that edge must be colored blue,"
  putStrLn "which leaves us with the following"
  prin_adj_matrix bear3
  putStrLn "But notice that this forms a blue triangle with the vertices [1,2,3]"
  putStrLn "which we verify with a call to is_blue_triangle([1,2,3]) which outputs"
  putStrLn (show (is_blue_triangle (subgraph_at_vertices bear3 [1,2,3])))
  putStrLn "Which completes the proof."
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn "We can now prove that R(3,3) <= 6."
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn "If we have an intially uncolored graph on 6 vertices "
  putStrLn "(labeled [0,1,2,3,4,5]) like the following"
  prin_adj_matrix graph
  putStrLn "then if we notice that vertex 0 must have the following vertices"
  putStrLn (show (getnone (head (fst graph))))
  putStrLn "colored either red or blue. Because we are fitting 5 vertices into"
  putStrLn "2 boxes, i.e. red or blue, then one of the boxes must contain 3"
  putStrLn "vertices. W.L.O.G (or by symmmetry) assume that the red box has at"
  putStrLn "least 3 vertices. Which gives the following adjacency matrix"
  prin_adj_matrix graph1
  putStrLn "Notice that the subgraph at vertices [0,1,2,3], i.e. the graph"
  prin_adj_matrix (subgraph_at_vertices graph1 [0,1,2,3])
  putStrLn "Is exactly the red bear-claw from lemma 1. Therefore by assuming there"
  putStrLn "is no red triangle (for a contradiction) we construct blue triangle "
  putStrLn "which we represent by the call lemma_1_no_red_tri_at_vert_i (graph1 0)"
  putStrLn "which returns"
  prin_adj_matrix (lemma_1_no_red_tri_at_vert_i graph1 0)
  putStrLn "which we can verify contains a blue triangle with a call to"
  putStrLn "is_blue_triangle(graph2 [1,2,3]) which returns"
  putStrLn (show (is_blue_triangle (subgraph_at_vertices graph2 [1,2,3])))
  putStrLn "Which completes the proof."
