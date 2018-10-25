import Euterpea
import Codec.Midi
import System.Random

data Node = Node {nodeNote :: Music Pitch, child :: [Music Pitch]} | End
    deriving (Show, Eq)

randInts :: Int -> [Int]
randInts seed = recInts (mkStdGen seed) where
recInts g = let (i,g') = next g in i : recInts g'

randNat :: Int -> Int
randNat i = abs ((randInts i) !! 0)

playNodeNote :: Node -> IO ()
playNodeNote (Node n _) = play n

findNote:: [Node] -> Music Pitch -> Node
findNote [] _ = End
findNote [End] _ = End
findNote ((Node p c):ns) p' =
  if p' == p then (Node p c) else findNote ns p'

getNextNode :: [Node] -> Node -> Int -> Node
getNextNode _ End _ = End
getNextNode _ (Node _ []) _ = End
getNextNode _ _ 0 = End
getNextNode ls (Node p c) n =
  let
    pitch = c!!n
    in findNote ls pitch

getNextMusic :: [Node] -> Music Pitch -> Int -> Music Pitch
getNextMusic [] _ _ = rest 3080
getNextMusic nl m i =
  let
    node1 = findNote nl m
    in
    case node1 of
      End -> rest 3080
      Node _ cl ->
       let
         len = length cl
         r = i `mod` len
         in
         if len == 0 then rest 3080 else cl !! r

getNodeNote :: Node -> Music Pitch
getNodeNote End = rest 3080
getNodeNote (Node p _) = p

getMusic:: (Either String Codec.Midi.Midi) -> Music Pitch
getMusic (Right file) = fmap fst (fromMidi file)

addPair:: [Node] -> Music Pitch -> Music Pitch -> [Node]
addPair [] p p' = [Node p [p']]
addPair ((Node p ls):ns) p' p'' =
  if p == p' then (Node p (p'':ls)):ns else (Node p ls):(addPair ns p' p'')

addNode :: [Node] -> Node -> [Node]
addNode [] n = [n]
addNode (n:ns) n' =
  let
    Node p c = n
    Node p' c' = n'
  in
    if p == p' then (Node p (c++c')):ns else n:(addNode ns n')

spilt :: Music Pitch -> [Music Pitch]
spilt (Prim n1) = [(Prim n1)]
spilt (Modify _ m) = spilt m
spilt (p1 :+: p2) = (spilt p1) ++ (spilt p2)
spilt (p1 :=: p2) = (spilt p1) ++ (spilt p2)

mapFromList :: [Music Pitch] -> [Node]
mapFromList [] = []
mapFromList (m:[]) =
  case m of
    Prim (Rest _ )-> []
    _ -> addPair [] m (rest 3080)
mapFromList (m:m':ms) =
  case (m,m') of
    (Prim (Rest _), _) -> mapFromList (m':ms)
    (_, Prim (Rest _)) -> mapFromList (m:ms)
    _ -> addPair (mapFromList (m':ms)) m m'


clearMods :: Music Pitch -> Music Pitch
clearMods (Prim n1) = Prim n1
clearMods (Modify _ m) = clearMods m
clearMods (m1 :+: m2) = (clearMods m1) :+: (clearMods m2)
clearMods (m1 :=: m2) = (clearMods m1) :=: (clearMods m2)

mergeMaps:: [Node] -> [Node] -> [Node]
mergeMaps [] ls = ls
mergeMaps ls [] = ls
mergeMaps ls (n:ns) = mergeMaps (addNode ls n) ns

mergeMapsList:: [[Node]] -> [Node]
mergeMapsList [] = []
mergeMapsList (n:ns) = mergeMaps n (mergeMapsList ns)

mapMusic :: Music Pitch -> [Node]
mapMusic (Prim n1) = addPair [] (Prim n1) (rest 3080)
mapMusic ((Prim n1) :+: ((Prim n2) :+: rm)) =
  addPair (mapMusic ((Prim n2) :+: rm)) (Prim n1) (Prim n2)
mapMusic (p1 :+: p2) = mergeMaps (mapMusic p1) (mapMusic p2)
mapMusic (p1 :=: p2) = mergeMaps (mapMusic p1) (mapMusic p2)
mapMusic (Modify _ m) = mapMusic m

musicSize :: Music Pitch -> Int
musicSize (Prim n) = 1
musicSize (Modify _ m) = musicSize m
musicSize (m1 :=: m2) = max (musicSize m1) (musicSize m2)
musicSize (m1 :+: m2) = (musicSize m1) + (musicSize m2)

playMap :: [Node] -> Int -> Music Pitch -> Music Pitch
playMap ls i m =
  let
    nextNote = getNextMusic ls m i
    in
      if nextNote == (rest 3080) then m
        else m :+: (playMap ls (randNat i) nextNote)

mapFromFile :: Either String Codec.Midi.Midi -> [Node]
mapFromFile f =
  mapFromList (filter ((/=) (rest 0)) (spilt (clearMods (getMusic f))))

main =
  let
    filePathList = [
                    "mozart_eine_kleine_easy.mid",
                    "EspanjaPrelude.mid",
                    "its-a-small-world.mid",
                    "bach_bourree.mid"
                    ]
    in
    do
      fileList <- sequence (map importFile filePathList)
      let
        musicMap = mergeMapsList (map mapFromFile fileList)
        startPitch1 = getNodeNote (musicMap !! 25)
        in
        play (playMap musicMap 300 startPitch1)
