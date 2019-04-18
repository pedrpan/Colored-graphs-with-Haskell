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

type Edge = (Vertex,Vertex)

simple_edges :: Int -> (Int,Int)
simple_edges 0 = (0,1)
simple_edges n = if  ((snd (simple_edges (n-1))) - (fst (simple_edges (n-1)))) == 1 then (0,(snd (simple_edges (n-1)))+1) else ((fst (simple_edges (n-1))) +1 , snd (simple_edges (n-1)))

edges ::  [Vertex] -> Int -> Edge
edges vs n = ( vs !! fst (simple_edges n) , vs !! snd (simple_edges n))

type Graph = ([Vertex],[Edge])

create_Graph :: Int -> Graph
create_Graph n = (create_vertex_set n , map (edges (create_vertex_set n)) [0..(((n-1)*n) `quot`2)-1])

edges_to_num :: (Int,Int) -> Int
edges_to_num (0,1) = 0
edges_to_num (x,y) = x+ (((y * (y-1))) `div` 2)

insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (y:ys) x = if x < y then x : y : ys else y : insert ys x

vertex_color_red_update :: Vertex -> Int -> Vertex
vertex_color_red_update (b,r,u) i = (b,insert r i,filter (/=i) u)

vertex_color_blue_update :: Vertex -> Int -> Vertex
vertex_color_blue_update (b,r,u) i = (insert b i,r,filter (/=i) u)

color_edge_red :: Graph -> (Int,Int) -> Graph
color_edge_red (vs,es) (i,j) = (vertices,edge_set)
  where
    vertices = (take i vs) ++ [vertex_color_red_update (head (drop i vs)) j] ++ (take (j-i-1) (drop (i) vs)) ++ [vertex_color_red_update (head (drop j vs)) i] ++ (drop j vs)
    edge_set  = map (edges vertices) [1..length vertices]

color_edge_blue :: Graph -> (Int,Int) -> Graph
color_edge_blue (vs,es) (i,j) = (vertices,edge_set)
  where
    vertices = (take i vs) ++ [vertex_color_blue_update (head (drop i vs)) j] ++ (take (j-i-1) (drop (i) vs)) ++ [vertex_color_blue_update (head (drop j vs)) i] ++ (drop j vs)
    edge_set  = map (edges vertices) [1..length vertices]
