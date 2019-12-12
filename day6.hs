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
type MarkedOrbitations = Map Object (Dist, [Object])

markObjects :: Orbitations -> MarkedOrbitations
markObjects orbs =
    let markObj nxt dist obj =
            case M.lookup obj orbs of
                Nothing -> M.singleton obj (dist, [])
                Just oo ->
                    let marked = M.singleton obj (dist, oo)
                        rest = map (nxt (dist+1)) oo
                    in M.unions (marked : rest)
    in fix markObj 0 Com

sumMarks = M.foldr (\(d,_) acc -> d + acc) 0

main = print . sumMarks . markObjects . buildOrbitMap =<< readFile "day6-input"
