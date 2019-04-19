type Vertex  = ([Int],[Int],[Int])

getredlist :: Vertex -> [Int]
getredlist (x,_,_) = x

getbluelist :: Vertex -> [Int]
getbluelist (_,x,_) =  x

getuncolored :: Vertex -> [Int]
getuncolored (_,_,x) = x

append :: Int -> [Int] -> [Int]
append n x = x ++ [n]

add_two_empty :: [Int] -> Vertex
add_two_empty x = ([],[],x)

create_vertex_set :: Int -> [Vertex]
create_vertex_set 2 = [([],[],[1]),([],[],[0])]
create_vertex_set n = (map add_two_empty (map (append (n-1)) (map getuncolored (create_vertex_set (n-1))))) ++ [(add_two_empty [0..n-2])]

data Edge = Red | Blue | Uncolored
  deriving (Eq, Ord, Show, Read)

simple_edges :: Int -> (Int,Int)
simple_edges 0 = (0,1)
simple_edges n = if  ((snd (simple_edges (n-1))) - (fst (simple_edges (n-1)))) == 1 then (0,(snd (simple_edges (n-1)))+1) else ((fst (simple_edges (n-1))) +1 , snd (simple_edges (n-1)))

is_edge_red :: Vertex -> Int -> Bool
is_edge_red v i = if (length (filter (== i) (getredlist v))) /= 0 then True else False

is_edge_blue :: Vertex -> Int -> Bool
is_edge_blue v i = if length ((filter (== i) (getbluelist v))) /= 0 then True else False

edges ::  [Vertex] -> Int -> Edge
edges vs n = if is_edge_red (vs !! ( fst (simple_edges n))) (snd (simple_edges n)) then Red else
              if is_edge_blue (vs !! ( fst (simple_edges n))) (snd (simple_edges n)) then Blue else Uncolored

type Graph = ([Vertex],[Edge])

create_Graph :: Int -> Graph
create_Graph n = (create_vertex_set n , map (edges (create_vertex_set n)) [0..(((n-1)*n) `quot`2)-1])

edges_to_num :: (Int,Int) -> Int
edges_to_num (0,1) = 0
edges_to_num (x,y) = x+ (((y * (y-1))) `div` 2)

insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (y:ys) x = if x < y then x : y : ys else
                    if x == y then y : ys else y : insert ys x

vertex_color_red_update :: Vertex -> Int -> Vertex
vertex_color_red_update (r,b,u) i = (insert r i,b,filter (/=i) u)

vertex_color_blue_update :: Vertex -> Int -> Vertex
vertex_color_blue_update (r,b,u) i = (r,insert b i,filter (/=i) u)

color_edge_red :: Graph -> (Int,Int) -> Graph
color_edge_red (vs,es) (i,j) = (vertices,edge_set)
  where
    vertices = (take i vs) ++ [vertex_color_red_update (head (drop i vs)) j] ++ (take (j-i-1) (drop (i+1) vs)) ++ [vertex_color_red_update (head (drop j vs)) i] ++ (drop (j+1) vs)
    edge_set  = map (edges vertices) [0..((((length vertices)-1)*(length vertices)) `quot`2)-1]

color_edge_blue :: Graph -> (Int,Int) -> Graph
color_edge_blue (vs,es) (i,j) = (vertices,edge_set)
  where
    vertices = (take i vs) ++ [vertex_color_blue_update (head (drop i vs)) j] ++ (take (j-i-1) (drop (i+1) vs)) ++ [vertex_color_blue_update (head (drop j vs)) i] ++ (drop (j+1) vs)
    edge_set  = map (edges vertices) [0..((((length vertices)-1)*(length vertices)) `quot`2)-1]

what_is_edge_color :: Graph -> (Int,Int) -> Edge
what_is_edge_color (vs,es) e = es !! (edges_to_num e)

row_color :: Graph -> Int -> [Edge]
row_color gs 1 = [what_is_edge_color gs (0,1)]
row_color gs n = map (what_is_edge_color gs) (map simple_edges (map (\x -> x+(((n-1)*n) `quot` 2))[0..n-1]))

color_mapping :: Graph -> Edge -> (Int, Int) -> Graph
color_mapping gs ed e |  ed == Red = color_edge_red gs e| ed == Blue = color_edge_blue gs e  | otherwise = gs

vert_translator :: [Int] -> (Int,Int) -> (Int,Int)
vert_translator ls (i,j) = (ls!!i,ls!!j)

convert_vs_to_es :: [Int] -> [(Int,Int)]
convert_vs_to_es vs = map ( vert_translator vs)  (map simple_edges [0..((((length vs)-1)*(length vs)) `quot`2)-1])

nfold_zipster :: (a->b->c->a)->a->[b]->[c]->a
nfold_zipster f x [y] [z] = f x y z
nfold_zipster f x (y:ys) (z:zs) = nfold_zipster f (f x y z) ys zs

imbed_graph_coloring_at_vertices :: Graph -> Graph -> [Int] -> Graph
imbed_graph_coloring_at_vertices subgs gs ls = nfold_zipster color_mapping gs (snd subgs) (convert_vs_to_es ls)

nfold_mapster :: (a->b->c->a)->a->b->[c]->a
nfold_mapster f x y [z] = f x y z
nfold_mapster f x y (z:zs) = nfold_mapster f (f x y z) y zs

get_edges_at_vertices :: Graph -> [Int] -> [Edge]
get_edges_at_vertices gs vs = nfold_mapster (\ x y z -> x ++ [what_is_edge_color y z]  ) []  gs (convert_vs_to_es vs)

subgraph_at_vertices :: Graph -> [Int] -> Graph
subgraph_at_vertices gs ls = nfold_zipster color_mapping (create_Graph (length ls)) (get_edges_at_vertices gs ls) (convert_vs_to_es [0..((length ls) -1)])

is_symmtric :: Graph -> (Int,Int) -> Bool
is_symmtric gs (i,j) = (vr,vb,vu) == (wr,wb,wu)
  where
      vr = filter (/=j) (getredlist   ((fst gs) !! i))
      vb = filter (/=j) (getbluelist  ((fst gs) !! i))
      vu = filter (/=j) (getuncolored ((fst gs) !! i))
      wr = filter (/=i) (getredlist   ((fst gs) !! j))
      wb = filter (/=i) (getbluelist  ((fst gs) !! j))
      wu = filter (/=i) (getuncolored ((fst gs) !! j))
