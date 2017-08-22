port module Todo exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import String
import Task

main : Program (Maybe Model) Model Msg

main = 
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

port setStorage : Model -> Cmd msg

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    ( newModel, cmds ) = fresh msg model
  in
    ( newModel, Cmd.batch [ setStorage newModel, cmds ])

-- MODEL

type alias Model =
  { entries : List Entry
  , field : String 
  , id : Int
  , visibility : String
  }

type alias Entry =
  { desc : String
  , completed : Bool
  , editing : Bool
  , id : Int
  }

empty : Model
empty = 
  { entries = []
  , field = ""
  , id = 0
  , visibility = "All"
  }

newEntry : String -> Int -> Entry
newEntry desc id =
  { desc = desc
  , completed = False
  , editing = False
  , id = id
  }

init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault empty savedModel ! []


-- UPDATE

type Msg = Noop
  | UpdateField String
  | EditingEntry Int Bool
  | UpdateEntry Int String
  | Add
  | Delete Int
  | DeleteComplete
  | Check Int Bool
  | CheckAll Bool
  | ChangeVisibility String

fresh : Msg -> Model -> ( Model, Cmd Msg )
fresh msg model =
  case msg of
    Noop ->
      model ! []

    UpdateField str ->
      { model | field = str }
        ! []

    EditingEntry id isEditing ->
      let 
        updateEntry t =
          if t.id == id then
            { t | editing = isEditing }
          else
            t

        focus =
          Dom.focus ( "todo-" ++ toString id )
      in
        { model | entries = List.map updateEntry model.entries }
          ! [ Task.attempt (\_ -> Noop) focus ]

    UpdateEntry id task ->
      let 
        updateEntry t =
          if t.id == id then
            { t | desc = task }
          else 
            t
      in 
        { model | entries = List.map updateEntry model.entries }
          ! []
          
    Add ->
      { model
        | id = model.id + 1
        , field = ""
        , entries =
            if String.isEmpty model.field then
              model.entries
            else
              model.entries ++ [ newEntry model.field model.id ]
      } ! []

    Delete id ->
      { model | entries = List.filter (\t -> t.id /= id) model.entries }
        ! []

    DeleteComplete ->
      { model | entries = List.filter (.completed >> not) model.entries }
        ! []

    Check id isCompleted ->
      let
        updateEntry t =
          if t.id == id then
            { t | completed = isCompleted }
          else
            t
      in
        { model | entries = List.map updateEntry model.entries }  
          ! []

    CheckAll isCompleted ->
      let
        updateEntry t =
            { t | completed = isCompleted }
      in
        { model | entries = List.map updateEntry model.entries }  
          ! []

    ChangeVisibility visibility ->
      { model | visibility = visibility }
        ! []

-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "todoapp"
    , style [ ( "visibility", "hidden" ) ] 
    ]
    [ section
      [ class "todoapp" ]
      [ lazy viewInput model.field
      , lazy2 viewEntries model.visibility model.entries
      , lazy2 viewControls model.visibility model.entries
      ]
    , infoFooter
    ]

viewInput : String -> Html Msg

viewInput task =
  header
    [ class "header" ]
    [ h1 [] [ text "todos" ]
    , input
      [ class "new-todo"
      , placeholder "What your plan today?"
      , autofocus True
      , value task
      , name "newTodo"
      , onInput UpdateField
      , onEnter Add
      ]
      []
    ]

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    isEnter code =
      if code == 13 then
        Json.succeed msg
      else
        Json.fail "not ENTER"
  in
    on "keydown" (Json.andThen isEnter keyCode)

-- VIEW ALL

viewEntries : String -> List Entry -> Html Msg
viewEntries visibility entries =
  let
    isVisible todo =
      case visibility of
        "Completed" ->
          todo.completed

        "Active" ->
          not todo.completed

        _ ->
          True

    allCompleted =
      List.all .completed entries

    cssVisibility =
      if List.isEmpty entries then
        "hidden"
      else
        "visible"
  in
    section
      [ class "main" 
      , style [ ( "visibility", cssVisibility ) ]
      ]
      [ input
        [ class "toggle-all"
        , type_ "checkbox"
        , name "toggle"
        , checked allCompleted
        , onClick (CheckAll (not allCompleted))
        ]
        []
      , label
        [ for "toggle-all" ]
        [ text "Mark all as complete" ]
      , Keyed.ul [ class "todo-list" ] <|
        List.map viewKeyedEntry (List.filter isVisible entries)
      ]

-- VIEW SINGLE ENTRY

viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry todo =
  ( toString todo.id, viewEntry todo)

viewEntry : Entry -> Html Msg
viewEntry todo =
  li
    [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
    [ div
      [ class "view" ]
      [ input
        [ class "toggle"
        , type_ "checkbox"
        , checked todo.completed
        , onClick (Check todo.id (not todo.completed))
        ]
        []
      , label
        [ onDoubleClick (EditingEntry todo.id True) ]
        [ text todo.desc ]
      , button
        [ class "destroy"
        , onClick (Delete todo.id)
        ]
        []
      ]
    , input
      [ class "edit"
      , value todo.desc
      , name "title"
      , id ("todo" ++ toString todo.id)
      , onInput (UpdateEntry todo.id)
      , onBlur (EditingEntry todo.id False)
      , onEnter (EditingEntry todo.id False)
      ]
      []
    ]

-- VIEW CONTROLS AND FOOTER

viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
  let
    entriesCompleted = List.length (List.filter .completed entries)
    entriesLeft = List.length entries - entriesCompleted

  in
    footer
      [ class "footer"
      , hidden (List.isEmpty entries)
      ]
      [ lazy viewControlsCount entriesLeft
      , lazy viewControlsFilters visibility
      , lazy viewControlsClear entriesCompleted
      ]

viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
  let
    item_ =
      if entriesLeft == 1 then " item"
      else " items"
  in
    span
      [ class "todo-count" ]
      [ strong [] [ text (toString entriesLeft) ]
      , text (item_ ++ " left")
      ]

viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
  ul
    [ class "filters" ]
    [ visibilitySwap "#/" "All" visibility
    , text " "
    , visibilitySwap "#/active" "Active" visibility
    , text " "
    , visibilitySwap "#/completed" "Completed" visibility
    ]

visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
  li
    [ onClick (ChangeVisibility visibility) ]
    [ a [ href uri , classList [ ( "selected", visibility == actualVisibility ) ] ]
      [ text visibility ]
    ]

viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
  button
    [ class "clear-completed"
    , hidden (entriesCompleted == 0)
    , onClick DeleteComplete
    ]
    [ text ("Clear completed (" ++ toString entriesCompleted ++ ")") ]

infoFooter : Html Msg
infoFooter =
  footer [ class "info" ]
    [ p [] [text "Double-click to edit a todo" ]
    , p [] [ text "Writted by evancz" ]
    , p []
        [ text "Part of "
        , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
        ]
    ]