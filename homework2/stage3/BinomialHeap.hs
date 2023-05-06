module BinomialHeap where

import Data.Char (toUpper)
import Data.Foldable
import Data.Function (on)
import Data.List hiding (insert)

{-
    Reprezentarea unui arbore binomial, având priorități de tipul p și chei
    de tipul k. Conform proprietății definitorii a acestor arbori, fiecare
    copil i dintre cei r copii ai unui nod de rang r, cu 1 <= i <= r, trebuie
    să aibă exact r-i copii la rândul său, adică r-1, r-2, ..., 1, 0 copii,
    exact în această ordine descrescătoare. Dacă rădăcina are r copii, atunci,
    în conjuncție cu proprietate anterioară, întregul arbore are 2^r noduri.
-}
data BinomialTree p k
  = EmptyTree
  | Node {prio :: p, key :: k, children :: [BinomialTree p k]}
  deriving (Eq)

{-
    Reprezentarea unui heap binomial, ca listă de arbori binomiali ce respectă
    proprietatea de heap. Mai precis, considerăm că, în fiecare arbore binomial,
    rădăcina are cea mai mică prioritate; analog pt subarbori. Câmpul size
    desemnează numărul de elemente din heap (nu numărul de arbori). Mai precis,
    dimensiunea este egală cu suma dimensiunilor arborilor din listă. Cum
    dimensiunea unui arbore de rang r este 2^r, dimensiunea este o sumă
    de puteri ale lui 2, unde exponenții sunt dimensiunile câmpurilor children
    (rangurile) din rădăcinile arborilor nevizi.
-}
data BinomialHeap p k = BinomialHeap {size :: Int, trees :: [BinomialTree p k]}
  deriving (Eq)

{-
    *** TODO ***

    Construiește recursiv un arbore binomial de rang r din doi arbori binomiali
    de rang r-1, atașând unul dintre arbori drept prim copil al rădăcinii
    celuilalt. Maniera în care se realizează atașarea trebuie să țină cont
    de faptul că cei doi arbori respectă proprietatea de heap, și că arborele
    rezultant trebuie de asemenea să o respecte. Astfel, arborele cu cheia mai
    mică din rădăcină trebuie să încorporeze arborele cu cheia mai mare.

    Atenție! Cei doi arbori primiți ca parametru au întotdeauna același rang,
    conform principiului de construcție. Funcția nu necesită parcurgeri
    recursive, putând opera direct la nivelul rădăcinilor.

    Constrângeri: utilizați gărzi.

    Hint: pt pattern matching, pot fi utile alias-urile (@).

    Exemple:

    > attach (Node 0 'a' []) (Node 1 'b' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}

    > attach (Node 1 'b' []) (Node 0 'a' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}
-}
attach :: Ord p => BinomialTree p k -> BinomialTree p k -> BinomialTree p k
attach EmptyTree EmptyTree = EmptyTree
attach EmptyTree node = node
attach node EmptyTree = node
attach tree1@(Node p1 k1 c1) tree2@(Node p2 k2 c2)
  | p1 < p2 = tree1 {children = tree2 : c1}
  | otherwise = tree2 {children = tree1 : c2}

{-
    *** TODO ***

    Introduce un arbore binomial nevid într-o listă cu alți arbori binomiali,
    toți arborii respectând proprietatea de heap. Cum nu pot exista simultan
    în listă doi arbori binomiali cu același rang, la întâlnirea unui arbore
    cu același rang cu cel care se dorește introdus, este necesară atșarea
    unuia la celălalt, cu crearea unui transport.

    Operația o oglindește pe cea de incrementare a unui număr binar din etapa 1.
    Astfel, elementele EmptyTree sunt analoagele biților 0, iar elementele Node,
    biților 1. O diferență este că, în această etapă, biții 1 „nu mai arată toți
    la fel”, ci elementele Node au rangul dat de poziția în listă. Spre exemplu:
    * un element Node de pe poziția 0 din listă trebuie să aibă rangul 0
      (dimensiunea 2^0 = 1)
    * un element Node de pe poziția 1 din listă trebuie să aibă rangul 1
      (dimensiunea 2^1 = 2)
    * un element Node de pe poziția 2 din listă trebuie să aibă rangul 2
      (dimensiunea 2^2 = 4)
    etc.

    Gestiunea transportului apărut în incrementare corespunde operației attach.
    Modul în care va fi utilizată mai departe funcția insertTree garantează
    respectarea presupunerilor funcției attach, cum că arborii primiți ca
    parametru au întotdeauna același rang.

    Constrângeri: utilizați
    * construcția case
    * funcția attach.

    Exemple:

    > insertTree (Node 1 'a' []) []
    [Node {prio = 1, key = 'a', children = []}]

    > insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > insertTree (Node 3 'c' []) $ insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}
insertTree :: Ord p => BinomialTree p k -> [BinomialTree p k] -> [BinomialTree p k]
insertTree tree@(Node p1 k1 c1) trees = case trees of
  [] -> [tree]
  EmptyTree : rest -> tree : rest
  node@(Node p2 k2 c2) : rest -> EmptyTree : insertTree (attach node tree) rest

{-
    *** TODO ***

    Heap-ul vid.
-}
emptyHeap :: BinomialHeap p k
emptyHeap = BinomialHeap 0 []

{-
    *** TODO ***

    Introduce o cheie cu prioritatea aferentă într-un heap binomial.

    Constrângeri: utilizați funcția insertTree.

    Exemple:

    > insert 1 'a' emptyHeap
    BinomialHeap
        { size = 1
        , trees = [Node {prio = 1, key = 'a', children = []}]
        }

    > insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap
        { size = 2
        , trees = [ EmptyTree
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }

    > insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap
        { size = 3
        , trees = [ Node {prio = 3, key = 'c', children = []}
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }
-}

insert :: Ord p => p -> k -> BinomialHeap p k -> BinomialHeap p k
insert prio key heap@(BinomialHeap s trees) =
  BinomialHeap (s + 1) $ insertTree (Node prio key []) trees

{-
    *** TODO ***

    Dacă heap-ul nu este vid, întoarce perechea formată din prioritatea minimă
    și cheia aferentă; în caz contrar, întoarce Nothing. Cum toți arborii din
    listă respectă proprietatea de heap, este suficient să parcurgeți doar
    rădăcinile pt a o determina pe cea cu prioritate minimă, fără a fi necesară
    explorarea nivelurilor inferioare ale arborilor.

    Constrângeri: pt selectarea arborilor nevizi din listă (ignorând elementele
    EmptyTree), utilizați list comprehension cu pattern matching.

    Hint: pt determinarea elementului minim dintr-o listă pe baza valorii
    calculate de o funcție numită criteriu, utilizați o expresie de forma:
    minimumBy (compare `on` criteriu) lista.

    Exemple:

    > findMin emptyHeap
    Nothing

    > findMin $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    Just (1,'a')
-}
findMin :: Ord p => BinomialHeap p k -> Maybe (p, k)
findMin heap@(BinomialHeap 0 []) = Nothing
findMin heap = Just $ foldl comparePrio (head pairs) pairs
  where
    pairs = [(p, k) | (Node p k c) <- trees heap]
    comparePrio acc pair = if fst acc < fst pair then acc else pair

{-
    Funcția zipExtend este similară funcției predefinite zip. Scopul ei este
    de a compensa limitarea funcției zip, care se oprește când atinge sfârșitul
    listei mai scurte. Primii doi parametri reprezintă valori cu care se extind
    prima, respectiv a doua listă, în funcție de care listă este mai scurtă.
    O puteți folosi în cele ce urmează.

    Exemple:

    > zipExtend 0 'z' [1,2] "abcd"
    [(1,'a'),(2,'b'),(0,'c'),(0,'d')]

    > zipExtend 0 'z' [1,2,3,4] "ab"
    [(1,'a'),(2,'b'),(3,'z'),(4,'z')]
-}
zipExtend :: a -> b -> [a] -> [b] -> [(a, b)]
zipExtend a' _ [] bs = zip (repeat a') bs
zipExtend _ b' as [] = zip as (repeat b')
zipExtend a' b' (a : as) (b : bs) = (a, b) : zipExtend a' b' as bs

{-
    *** TODO ***

    Combină două liste de arbori binomiali care respectă proprietatea de heap.
    Observațiile din comentariile funcției insertTree, legate de necesitatea
    atașării arborilor cu același rang, rămân valabile.

    Operația o oglindește pe cea de adunare a două numere binare din etapa 1.

    Constrângeri:
    * evitați recursivitatea explicită
    * utilizați funcția zipExtend pt a facilita aplicarea unor funcționale.

    Exemple:

    > mergeTrees [Node 1 'a' []] []
    [Node {prio = 1, key = 'a', children = []}]

    > mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > mergeTrees [Node 3 'c' []] $ mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}
mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
mergeTrees trees1 trees2 = case lastCarry of
  EmptyTree -> result
  _ -> result ++ [lastCarry]
  where
    addTrees carry (EmptyTree, EmptyTree) = (EmptyTree, carry)
    addTrees carry (node1@(Node p1 k1 c1), node2@(Node p2 k2 c2)) = (attach node2 node1, carry)
    addTrees EmptyTree (node, EmptyTree) = (EmptyTree, node)
    addTrees EmptyTree (EmptyTree, node) = (EmptyTree, node)
    addTrees carry (EmptyTree, node) = (attach node carry, EmptyTree)
    addTrees carry (node, EmptyTree) = (attach node carry, EmptyTree)
    (lastCarry, result) = mapAccumL addTrees EmptyTree $ zipExtend EmptyTree EmptyTree trees1 trees2

{-
    *** TODO ***

    Combină două heap-uri binomiale.

    Constrângeri: utilizați funcția mergeTrees.

    Exemple: similare cu cele de la mergeTrees.
-}
merge :: Ord p => BinomialHeap p k -> BinomialHeap p k -> BinomialHeap p k
merge heap1@(BinomialHeap size1 trees1) heap2@(BinomialHeap size2 trees2) =
  BinomialHeap newSize mergedTrees
  where
    newSize = size1 + size2
    mergedTrees = mergeTrees trees1 trees2

----------------------------------- Etapa 3 ------------------------------------

{-
    *** TODO ***

    Funcție ajutătoare, care izolează, pe rând, câte un element al unei liste,
    înlocuindu-l în cadrul listei cu un altul primit ca parametru. Întoarce
    o listă de perechi, care conțin pe prima poziție un element al listei
    originale, iar pe a doua, lista din care provine elementul, dar după
    înlocuirea de mai sus. Se va evita introducerea elementului înlocuitor
    pe ultima poziție din listă.

    Constrângeri:
    * puteți utiliza și recursivitate explicită, dar utilizați funcționale
      pe cât posibil (măcar pt părți din rezolvare)
    * NU este permisă adăugarea de asumpții asupra tipului a, e.g. Eq a.

    Exemple:

    > isolate 0 [1,2,3]
    [(1,[0,2,3]),(2,[1,0,3]),(3,[1,2])]  -- fără 0 în ultima listă
-}
isolate :: a -> [a] -> [(a, [a])]
isolate placeHolder list = snd $ mapAccumL f ([], tail list) list
  where
    f (firstPart, secondPart) elem = ((newFirstPart, newSecondPart), (elem, newList))
      where
        newFirstPart = firstPart ++ [elem]
        newSecondPart = tail secondPart
        newList = case secondPart of
          [] -> firstPart
          _ -> firstPart ++ [placeHolder] ++ secondPart

{-
    *** TODO ***

    Elimină din heap prima rădăcină de prioritate minimă. Aceasta presupune
    înlăturarea întregului arbore cu rădăcina de prioritate minimă din lista
    de arbori ai heap-ului (prin înlocuirea lui cu EmptyTree) și apoi
    combinarea (mergeTrees) noii liste de arbori cu lista de subarbori (orfani)
    ai rădăcinii tocmai înlăturate.

    Atenție! Având în vedere că lista de arbori ai heap-ului este ordonată
    crescător în raport cu rangul, iar lista de subarbori orfani este ordonată
    descrescător (conform structurii arborilor binomiali), este necesară
    inversarea ultimeia înainte de combinarea celor două liste!

    Constrângeri: utilizați isolate.

    Hint: vedeți indicațiile de la findMin pentru determinarea primei rădăcini
    de prioritate minimă.

    Exemple:

    > removeMin emptyHeap
    BinomialHeap {size = 0, trees = []}

    > removeMin $ insert 1 'a' emptyHeap
    BinomialHeap {size = 0, trees = []}

    > removeMin $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap {size = 1, trees = [Node {prio = 2, key = 'b', children = []}]}

    > removeMin $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap
        { size = 2
        , trees = [ EmptyTree
                  , Node {prio = 2, key = 'b', children = [Node {prio = 3, key = 'c', children = []}]}
                  ]
        }

    După ce implementați, răspundeți la întrebarea: Care este contribuția
    evaluării leneșe la utilizarea eficientă a funcției isolate?
-}
removeMin :: (Ord p, Eq k) => BinomialHeap p k -> BinomialHeap p k
removeMin heap@(BinomialHeap 0 []) = heap
removeMin (BinomialHeap s child) = BinomialHeap (s - 1) newChildren
  where
    isolatedTrees = isolate EmptyTree child
    compareTree (EmptyTree, restTrees1) (tree, restTrees2) = (tree, restTrees2)
    compareTree (min, restTrees1) (EmptyTree, restTrees2) = (min, restTrees1)
    compareTree (min@(Node v1 _ _), restTrees1) (tree@(Node v2 _ _), restTrees2) =
      if v1 > v2 then (tree, restTrees2) else (min, restTrees1)
    (isolatedTree, remainingTrees) = foldl compareTree (head isolatedTrees) isolatedTrees
    newChildren = mergeTrees remainingTrees $ reverse $ children isolatedTree

{-
    *** TODO ***

    Instanțiați clasa Show pt tipul (BinomialTree p k) în următorul mod:
    * un arbore vid este reprezentat prin "*"
    * un arbore nevid este reprezentat prin:

        prioritate (cheie)
          <copil 1>
          ...
          <copil n>

      unde reprezentarea copiilor este indentată cu 2 spații față de cea
      a părintelui.

    Hint: replicate și intercalate.

    Exemple:

    > EmptyTree
    *

    > Node 1 'a' []
    1 ('a')

    > Node 1 'a' [Node 2 'b' []]
    1 ('a')
      2 ('b')

    > Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    1 ('a')
      3 ('c')
        4 ('d')
      2 ('b')
-}
instance (Show p, Show k) => Show (BinomialTree p k) where
  show = showNode 0
    where
      indent n = replicate (2 * n) ' '
      printNode (Node p k _) = show p ++ " (" ++ show k ++ ")"
      showNode depth EmptyTree = indent depth ++ "*"
      showNode depth node@(Node _ _ []) =
        indent depth ++ printNode node
      showNode depth node@(Node _ _ child) =
        indent depth ++ printNode node ++ "\n" ++ intercalate "\n" (map ((depth + 1) `showNode`) child)

{-
    *** TODO ***

    Instanțiați clasa Show pt tipul (BinomialHeap p k) cu reprezentarea:

    <arbore 1>
    ...
    <arbore n>

    Exemple:

    > insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    3 ('c')
    1 ('a')
      2 ('b')

    > insert 5 'e' $ insert 4 'd' $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    5 ('e')
    *
    1 ('a')
      3 ('c')
        4 ('d')
      2 ('b')
-}
instance (Show p, Show k) => Show (BinomialHeap p k) where
  show (BinomialHeap _ child) = intercalate "\n" $ map show child

{-
    *** TODO ***

    Instanțiați clasa Functor cu constructorul (BinomialTree p). Observați
    că clasa Functor așteaptă constructori unari de tip, dar BinomialTree
    este binar; acest lucru înseamnă că BinomialTree trebuie aplicat parțial
    pe tipul priorităților, care conduce la fixarea acestuia, și varierea doar
    a tipului cheilor (vedeți tipul lui fmap mai jos).

    fmap aplică o funcție pe cheile din arbore, fără a altera structura acestuia.

    Exemple:

    > fmap toUpper $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    1 ('A')
      3 ('C')
        4 ('D')
      2 ('B')
-}
instance Functor (BinomialTree p) where
  -- fmap :: (k1 -> k2) -> BinomialTree p k1 -> BinomialTree p k2
  fmap f EmptyTree = EmptyTree
  fmap f (Node p k c) = Node p (f k) $ map (fmap f) c

{-
    *** TODO ***

    Instanțiați clasa Functor cu constructorul (BinomialHeap p). Observațiile
    aferente instanței pt (BinomialTree p) de mai sus sunt valabile și aici.

    fmap aplică o funcție pe cheile din heap, fără a altera structura listei
    de arbori sau a arborilor înșiși.

    Exemple:

    > fmap toUpper $ insert 5 'e' $ insert 4 'd' $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    5 ('E')
    *
    1 ('A')
      3 ('C')
        4 ('D')
      2 ('B')
-}
instance Functor (BinomialHeap p) where
  -- fmap :: (k1 -> k2) -> BinomialHeap p k1 -> BinomialHeap p k2
  fmap f (BinomialHeap s child) = BinomialHeap s $ map (fmap f) child

{-
    *** TODO BONUS ***

    Instanțiați clasa Foldable cu constructorul (BinomialTree p) astfel încât
    funcția foldr să împăturească toate cheile din arbore la o singură valoare
    (vedeți tipul lui foldr).

    Dacă încercați să dați o definiție directă, similară celei pe listele
    standard, veți observa că, la un nod din arbore, aveți la dispoziție o cheie
    de tipul k și o listă de acumulatori având tipul [b] (rezultată din
    împăturirea recursivă a copiilor), astfel încât nu este clar pe ce parametru
    de tipul b trebuie aplicată funcția f de la foldr. Cumva, ar trebui să
    combinăm toți acumulatorii din listă într-unul singur de tipul b, dar
    nu știm să facem acest lucru pt orice tip b.

    Prin urmare, vom căuta o abordare alternativă. Observăm că, prin aplicarea
    parțială a funcției f asupra unei chei de tipul k, obținem o funcție cu
    tipul (b -> b). Dacă ne mulțumim doar cu aceste aplicații parțiale, putem
    obține prin împăturirea recursivă a copiilor o listă cu tipul [b -> b], în
    loc de [b]. Pe aceste funcții le putem combina întotdeauna prin compunere,
    indiferent de tipul b. În final, la nivelul rădăcinii, obținem o singură
    funcție, pe care o putem aplica asupra acumulatorului inițial de la foldr.

    Constrângeri: Nu se acceptă liniarizarea prealabilă a arborelui într-o listă
    de chei și aplicarea foldr pe lista rezultantă. Dimpotrivă, în exemplele
    de mai jos, liniarizarea însăși este definită pe baza lui foldr implementat
    de voi.

    Exemple:

    -- Cheia maximă din arbore (caracterele sunt ordonate)
    > foldr max 'a' $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    'd'

    -- maximum este predefinit, într-o manieră similară celei cu foldr de mai sus.
    > maximum $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    'd'

    -- Liniarizarea arborelui, i.e. lista cheilor (String = [Char])
    > foldr (:) [] $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    "acdb"

    -- toList este predefinit prin foldr ca mai sus.
    > toList $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    "acdb"
-}
instance Foldable (BinomialTree p) where
  -- foldr :: (k -> b -> b) -> b -> BinomialTree p k -> b
  foldr f acc tree = undefined
