module Sandpile exposing (..)

import Animator exposing (Timeline)
import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Array exposing (..)
import Task
import Time

main = Browser.element {init=init, update=update, subscriptions=subscriptions, view=view}

type alias Cell = {x:Int, y:Int, height:Int}
type alias Conf = List Cell
type Model = Setup | Simulate Conf
type Msg = RandGen
         | Start
         | Tick Time.Posix
    

unit = 6
size = 100

init: () -> (Model, Cmd Msg)
init _ =
    (Setup, Cmd.none)
       
initialConf:  (Model, Cmd Msg)
initialConf  =
    let
        base =
            List.concat <|
            List.map
                (\x -> List.map (\y -> {x=x,y=y,height=0}) <| List.range 0 (size-1))
                <| List.range 0 (size-1)
        conf = {x=50,y=50,height=100000} ::
                     (List.filter (\c -> c.x /= 50 || c.y /= 50) base)
    in
        (Simulate conf, Cmd.none)
        --((List.drop 1 conf), Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg, model) of
        (Start, Setup) -> initialConf
        (Tick _, Simulate conf) -> (Simulate (stabilize conf), Cmd.none)
        (_,_) -> (model, Cmd.none)

neighbor: Cell -> Conf -> Conf
neighbor cell model =
    List.filter (\c -> ((abs (cell.x - c.x)) + (abs (cell.y - c.y))) == 1) model

                           

fire: Cell -> Conf -> Conf
fire cell model =
    if cell.height >= 4 then
        let
            nei = List.map (\c -> {c|height=(c.height+1)} ) <| neighbor cell model
            others = List.filter (\c -> ((abs (cell.x - c.x)) + (abs (cell.y - c.y))) > 1) model
        in
            List.concat [({cell | height=(cell.height-4)} :: nei ), others]
    else
        model

isStable: Conf -> Bool
isStable model =
    List.all (\c -> c.height<4) model

stabilize: Conf -> Conf
stabilize model =
    if isStable model then
        model
    else
        let
            unstables = List.filter (\c -> c.height >= 4) model
        in
            List.foldr (\u conf -> fire u conf) model unstables
                 
cellSvg: Cell -> Svg Msg
cellSvg cell =
    let
        color height =
            if height >= 4 then
                "black"
            else if height == 3 then
                     "red"
                 else if height == 2 then
                          "orange"
                     else if height == 1 then
                              "yellow"
                         else
                             "white"
    in
    rect [x <| String.fromInt (cell.x*unit)
         ,y <| String.fromInt (cell.y*unit)
         ,width <| String.fromInt unit
         ,height <| String.fromInt unit
         ,fill <| color cell.height
         ,stroke "noe"
         ][]
    
                   
view: Model -> Html Msg
view model =
    case model of
        Setup ->
            Html.div [Html.Events.onClick Start]
                [Html.button [][text "start"]]
        Simulate conf ->
            svg [width "800"
                ,height "800"
                ]
            <| List.map cellSvg conf

subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 0.1 Tick

                      
