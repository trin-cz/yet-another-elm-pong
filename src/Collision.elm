module Collision
    exposing
        ( Side(Top, Right, Bottom, Left)
        , rectangleSide
        , Rectangle(..)
        , rectangle
        , rectangleIsCollision
        )

{-| Detect collision/intersection of geometry in a defined 2D coordinate space
AKA tell me when objects are touching or overlapping.

All objects use the same coordinate system you might see in an algebra or
physics problem, origin (0,0) is at the center of the object,
so they're compatible with the core Graphics.Collage coordinate system.


# Basic geometry

@docs Rectangle, rectangle, Circle, circle


# Rectangle to Rectangle Collision

@docs axisAlignedBoundingBox, rectangleSide, Side


# Circle to Circle Collision

@docs circleToCircle

-}


{-| Represents rectangular hitbox geometry.
-}
type Rectangle
    = Rectangle { cx : Float, cy : Float, w : Float, h : Float }


{-| Create a Rectangle hitbox from coordinates (cx, cy) and geometry (width and height)

Arguments:

    rectangle centerX centerY width height

Example:

    rectangle 5 5 10 10
    -- a 10 x 10 rectangle centered on coordinates 5,5

-}
rectangle : Float -> Float -> Float -> Float -> Rectangle
rectangle centerX centerY width height =
    Rectangle { cx = centerX, cy = centerY, w = width, h = height }


{-| Represents sides of a Rectangle
-}
type Side
    = Top
    | Right
    | Bottom
    | Left


{-| Very efficiently detect which side of a Rectangle is colliding with another Rectangle

    rect1 = rectangle 5 5 10 10
    rect2 = rectangle 7 5 10 10

    rectangleSide rect1 rect2 -- Just Right
    -- rect1 is coliding with it's right side onto rect2

-}
rectangleSide : Rectangle -> Rectangle -> Maybe Side
rectangleSide (Rectangle rect1) (Rectangle rect2) =
    {-
       Calculate which side of a rectangle is colliding w/ another, it works by
       getting the Minkowski sum of rect2 and rect1, then checking where the centre of
       rect1 lies relatively to the new rectangle (from Minkowski) and to its diagonals
       * thanks to sam hocevar @samhocevar for the formula!
    -}
    let
        w =
            0.5 * (rect1.w + rect2.w)

        h =
            0.5 * (rect1.h + rect2.h)

        dx =
            rect2.cx - rect1.cx

        dy =
            rect2.cy - rect1.cy

        wy =
            w * dy

        hx =
            h * dx
    in
        if abs dx <= w && abs dy <= h then
            if (wy > hx) then
                if (wy > -hx) then
                    Just Top
                else
                    Just Left
            else if (wy > -hx) then
                Just Right
            else
                Just Bottom
        else
            Nothing


rectangleIsCollision : Rectangle -> Rectangle -> Bool
rectangleIsCollision r1 r2 =
    isJust <| rectangleSide r1 r2


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Nothing ->
            False

        Just _ ->
            True
