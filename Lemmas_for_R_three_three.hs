module Lemmas_for_R_three_three where
import Colored_graph

blue_triangle :: Graph
blue_triangle = color_edge_blue (color_edge_blue (color_edge_blue (create_Graph 3) (0,1)) (0,2)) (1,2)

lemma_1_no_red_tri_at_vert_i :: Graph -> Int -> Graph
lemma_1_no_red_tri_at_vert_i gs i = if (length (getredlist ((fst gs) !! i))) >2
                                      then imbed_graph_coloring_at_vertices (blue_triangle) (gs) (take 3 (getredlist ((fst gs) !! i)))
                                      else gs

red_triangle :: Graph
red_triangle = color_edge_red (color_edge_red (color_edge_red (create_Graph 3) (0,1)) (0,2)) (1,2)

lemma_1_no_blue_tri_at_vert_i :: Graph -> Int -> Graph
lemma_1_no_blue_tri_at_vert_i gs i = if (length (getbluelist ((fst gs) !! i))) >2
                                      then imbed_graph_coloring_at_vertices (red_triangle) (gs) (take 3 (getbluelist ((fst gs) !! i)))
                                      else gs

red_bearclaw :: Graph
red_bearclaw = color_edge_red (color_edge_red (color_edge_red (create_Graph 4) (0,1)) (0,2)) (0,3)

pigeonhole_principle_at_i :: Graph -> Int -> Graph
pigeonhole_principle_at_i gs i = if (length (getnone ((fst gs) !! i))) >2
                                      then imbed_graph_coloring_at_vertices (red_bearclaw) (gs) ([i]++(take 3 (getnone ((fst gs) !! i))))
                                      else gs

is_blue_triangle :: Graph -> Bool
is_blue_triangle gs = and ( map ((==) Blue) (snd gs))

is_red_triangle :: Graph -> Bool
is_red_triangle gs = and ( map ((==) Redd) (snd gs))
