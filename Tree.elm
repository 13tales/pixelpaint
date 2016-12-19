module Tree exposing (..)

import Window exposing (Size)
import Mouse exposing (Position)


-- Tree definition


type Tree a
    = Node BoundingBox (Tree a) (Tree a) (Tree a) (Tree a)
    | Leaf (Bounded a)


type alias BoundingBox =
    { pos : Position
    , size : Size
    }


type alias Bounded a =
    { boundingBox : BoundingBox
    , val : a
    }


newBox : Int -> Int -> Int -> Int -> BoundingBox
newBox x y w h =
    { pos = { x = x, y = y }
    , size = { width = w, height = h }
    }


setBounds : BoundingBox -> a -> Bounded a
setBounds box v =
    { boundingBox = box
    , val = v
    }


boxCentre : BoundingBox -> Position
boxCentre box =
    { x = (box.pos.x + (box.size.width // 2))
    , y = (box.pos.y + (box.size.height // 2))
    }


boxHeight : BoundingBox -> Int
boxHeight b =
    b.size.height


boxWidth : BoundingBox -> Int
boxWidth b =
    b.size.width


boxesEqual : BoundingBox -> BoundingBox -> Bool
boxesEqual l r =
    l.pos.x
        == r.pos.x
        && l.pos.y
        == r.pos.y
        && l.size.width
        == r.size.width
        && l.size.height
        == r.size.height


subDivide : Tree a -> Tree a
subDivide t =
    case t of
        Node _ _ _ _ _ ->
            t

        Leaf l ->
            let
                parentBox =
                    l.boundingBox

                leafValue =
                    l.val

                ( parentX, parentY, childWidth, childHeight ) =
                    ( parentBox.pos.x
                    , parentBox.pos.y
                    , parentBox.size.width // 2
                    , parentBox.size.height // 2
                    )

                ( neBox, nwBox, seBox, swBox ) =
                    ( newBox parentX parentY childWidth childHeight
                    , newBox (parentX + childWidth) parentY childWidth childHeight
                    , newBox parentX (parentY + childHeight) childWidth childHeight
                    , newBox (parentX + childWidth) (parentY + childHeight) childWidth childHeight
                    )
            in
                Node parentBox
                    (Leaf <| setBounds neBox leafValue)
                    (Leaf <| setBounds nwBox leafValue)
                    (Leaf <| setBounds seBox leafValue)
                    (Leaf <| setBounds swBox leafValue)


cullTree : Tree a -> Tree a
cullTree t =
    case t of
        Leaf l ->
            t

        Node box (Leaf ne) (Leaf nw) (Leaf se) (Leaf sw) ->
            if equalFour ne.val nw.val se.val sw.val then
                Leaf (setBounds box ne.val)
            else
                t

        Node box ne nw se sw ->
            Node box (cullTree ne) (cullTree nw) (cullTree se) (cullTree sw)


equalFour : a -> a -> a -> a -> Bool
equalFour a b c d =
    a == b && b == c && c == d
