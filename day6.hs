import Data.List
import Data.Function
-- import Data.Map.NonEmpty (NEMap)
-- import qualified Data.Map.NonEmpty as NM
import Data.Map (Map)
import qualified Data.Map as M

data Orbit = Object `Orbits` Object
    deriving (Eq, Show)

data Object = Com | Object String
    deriving (Eq, Show, Ord)

readObject :: String -> Object
readObject "COM" = Com
readObject ref = Object ref

readOrbit :: String -> Orbit
readOrbit oo =
    let Just i = findIndex (== ')') oo
        orbitee = readObject $ take i oo
        orbiter = readObject $ drop (i + 1) oo
    in orbiter `Orbits` orbitee

type Orbitations = Map Object [Object]

toKeyVal :: Orbit -> (Object, [Object])
toKeyVal (o `Orbits` c) = (c, [o])

dummy = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\n"

buildOrbitMap :: String -> Orbitations
buildOrbitMap = M.fromListWith (<>) . map (toKeyVal  . readOrbit) . words

type Dist = Int
type MarkedOrbitations a = Map Object (a, [Object])

markDists :: Orbitations -> MarkedOrbitations Dist
markDists orbs =
    let markObj nxt dist obj =
            case M.lookup obj orbs of
                Nothing -> M.singleton obj (dist, [])
                Just oo ->
                    let marked = M.singleton obj (dist, oo)
                        rest = map (nxt (dist+1)) oo
                    in M.unions (marked : rest)
    in fix markObj 0 Com

sumMarks = M.foldr (\(d,_) acc -> d + acc) 0

main1 = print . sumMarks . markDists . buildOrbitMap =<< readFile "day6-input"

markPaths :: Orbitations -> MarkedOrbitations [Object]
markPaths orbs =
    let markObj nxt rpath obj =
            case M.lookup obj orbs of
                Nothing -> M.singleton obj (reverse rpath, [])
                Just oo ->
                    let marked = M.singleton obj (reverse rpath, oo)
                        rest = map (nxt (obj : rpath)) oo
                    in M.unions (marked : rest)
    in fix markObj [] Com

shortestPath :: Object -> Object -> MarkedOrbitations [Object] -> Int
shortestPath a b morbs =
    let (pa,_) = morbs M.! a
        (pb,_) = morbs M.! b
        horp nxt oo1@(o1:p1) oo2@(o2:p2)
            | o1 == o2 = nxt p1 p2
            | otherwise = (oo1,oo2)
        horp _ oo1 oo2 = (oo1, oo2)
        (remA, remB) = fix horp pa pb

    in length remA + length remB

dummy2 = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"

santaPath = shortestPath (Object "YOU") (Object "SAN")
main = print . santaPath . markPaths . buildOrbitMap =<< readFile "day6-input"
