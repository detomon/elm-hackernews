module MultiwayTree.Extra exposing
    ( Path
    , goToPath
    , replace, updateAt, removeChild
    , zipperChildren
    )

{-| Adds extra function to MultiwayTree and MultiwayTreeZipper.


# Types

@docs Path


# Navigation API

@docs goToPath


# Update API

@docs replace, updateAt, removeChild


# Access API

@docs zipperChildren

-}

import List.Extra
import MultiwayTree exposing (Forest, Tree(..))
import MultiwayTreeZipper exposing (Breadcrumbs, Zipper, goToChild, goToRoot)


{-| Defines a path to a tree node relative to root.
-}
type alias Path =
    List Int


{-| Goto given path.
-}
goToPath : Path -> Zipper a -> Maybe (Zipper a)
goToPath path zipper =
    List.foldl (\idx -> Maybe.andThen (goToChild idx)) (Just zipper) path


{-| Map tree node to a new node.
-}
replace : (Tree a -> Maybe (Tree a)) -> Tree a -> Tree a
replace fn ((Tree datum children) as tree) =
    case fn tree of
        Just tree_ ->
            tree_

        Nothing ->
            Tree datum <|
                List.map (replace fn) children



--fn tree
--    |> Maybe.withDefault (Tree datum (List.map (replace fn) children))


{-| Update tree at given path.
-}
updateAt : (Zipper a -> Maybe (Zipper a)) -> Path -> Zipper a -> Zipper a
updateAt fn path tree =
    tree
        |> goToPath path
        |> Maybe.andThen fn
        |> Maybe.andThen goToRoot
        |> Maybe.withDefault tree


{-| Remove child at index.
-}
removeChild : Int -> Zipper a -> Maybe (Zipper a)
removeChild idx ( Tree datum children, breadcrumbs ) =
    Just ( Tree datum (List.Extra.removeAt idx children), breadcrumbs )


{-| Get children of zipper.
-}
zipperChildren : Zipper a -> Forest a
zipperChildren zipper =
    let
        ( Tree _ children, _ ) =
            zipper
    in
    children
