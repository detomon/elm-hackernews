module MultiwayTree.Extra exposing
    ( goToPath
    , replace
    , updatePath
    , removeChild
    )

import List.Extra
import MultiwayTree exposing (Tree(..))
import MultiwayTreeZipper exposing (Breadcrumbs, Zipper, goToRoot, goToChild)


type alias Path =
    List Int


{-| Goto given path.
-}
goToPath : Path -> Zipper a -> Maybe (Zipper a)
goToPath path zipper =
    List.foldl (\i -> Maybe.andThen (goToChild i)) (Just zipper) path


{-| Map tree node to a new node.
-}
replace : (Tree a -> Maybe (Tree a)) -> Tree a -> Tree a
replace fn ((Tree datum children) as tree) =
    fn tree
        |> Maybe.withDefault (Tree datum (List.map (replace fn) children))


{-| Update tree at given path.
-}
updatePath : (Zipper a -> Maybe (Zipper a)) -> Path -> Zipper a -> Zipper a
updatePath fn path tree =
    tree
        |> goToPath path
        |> Maybe.andThen fn
        |> Maybe.andThen goToRoot
        |> Maybe.withDefault tree


{-| Remove child at index.
-}
removeChild : Int -> Zipper a -> Maybe (Zipper a)
removeChild idx ( Tree d c, breadcrumbs ) =
    Just ( Tree d (List.Extra.removeAt idx c), breadcrumbs )
