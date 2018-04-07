module Main exposing (..)

import Html exposing (Html, div, text, program, input, button, br, Attribute, textarea, img)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Html.Attributes exposing (placeholder, value, src)

import Regex exposing (HowMany,replace,regex)

import Http
import RemoteData exposing (WebData)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required, custom, optional)


-- MODEL

--type SpellType = Creature | Enchantment | Artifact | Instant | Sorcery

type alias JsonImageUris = {
    small : String
    , normal : String
}

type alias JsonCard = {
    name : String
    , manaCost : String
    , typeLine : String
    , power : Maybe String
    , toughness : Maybe String
    , oracleText : String
    , imageUris : JsonImageUris
}

type alias ManaCost = {
    generic : Maybe Int
    , b : Maybe Int
    , u : Maybe Int
    , g : Maybe Int
    , r : Maybe Int
    , w : Maybe Int
    , x : Maybe Int
    , total : Maybe Int
}

type alias Category = {
    id : Int
    , name : String
}

type alias Card = {
    name : String
    , manaCost : ManaCost
    , types : String
    , power : Maybe Int --maybe merge these into a record
    , toughness : Maybe Int
    , oracleText : String
    , imageUri : String
    --, categories : List Int -- move this into a separate record to hold input data
}

type alias CardSaveData = {
    name : String
    , categories : List Int
}

type alias SaveData = {
    categories : List Category
    , cardData : List CardSaveData
}

type alias ImportBoxData = {
    inputFieldText : String
    , importStatusText : Maybe String
    , massImportFieldText : String
}

type alias Model = {
    importBoxData : ImportBoxData
    , deck : List Card
--    , maybeBoard : List Card
    , expandedCardName : Maybe String
    , saveData : SaveData
}

emptySaveData = 
    { categories = [{id = 0, name = "Card draw"},{id = 1, name = "Control"},{id = 2, name = "Ramp"},{id = 3, name = "Util"},{id = 4, name = "Fun"}]
    , cardData = []
    }

emptyCardSaveData cardName = 
    { name = cardName
    , categories = []
    }
    

apiUrl : String
apiUrl = "https://api.scryfall.com"

init : ( Model, Cmd Msg )
init =
    ( {
        importBoxData = { inputFieldText = "", importStatusText = Nothing, massImportFieldText = "ponder\nvorinclex\nhavok festival\nhypergenesis\nmalignus"}
        , deck = []
--        , maybeBoard = []
        , expandedCardName = Nothing
        , saveData = emptySaveData
    }, Cmd.none )


-- MESSAGES


type Msg
    = NoOp
    | ChangeInputFieldText String
    | ChangeMassImportFieldText String
    | MassImportCards (List String)
    | AddCard String
    | OnFetchCompleted (WebData JsonCard)
    | OnCardSelected String
    | SetCategoryOnCard Bool Int String


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []
        ChangeInputFieldText newText ->
            setImportFieldText newText model ! []
        ChangeMassImportFieldText newText ->
            setMassImportFieldText newText model ! []
        AddCard cardName ->
            case validateCardNameInput cardName of
                (False, reson) -> 
                    (model |> setImportStatusText (Just reson) |> setImportFieldText "") ! []
                (True, reson) ->
                    (model |> setImportStatusText (Just "Starting fetch") |> setImportFieldText "") ! [fetchCard cardName]
        MassImportCards cardList ->
            model ! (List.map (\c -> fetchCard c) cardList)
        OnFetchCompleted response ->
            case response of
                RemoteData.NotAsked ->
                    setImportStatusText (Just "NotAsked") model ! []
                RemoteData.Loading ->
                    setImportStatusText (Just "Loading") model ! []
                RemoteData.Success jsonCard ->
                    let
                        newModel = setImportStatusText (Just ("Sucess: Added " ++ jsonCard.name)) model 
                        saveData = model.saveData
                        newSaveData = {saveData | cardData = (emptyCardSaveData jsonCard.name) :: saveData.cardData }
                    in
                        {newModel | deck = (jsonCard |> convertJsonCardIntoCard) :: newModel.deck, expandedCardName = Just jsonCard.name, saveData = newSaveData} ! []
                RemoteData.Failure error ->
                    setImportStatusText (Just ("Error: " ++ toString error)) model ! []
        OnCardSelected cardName ->
            {model | expandedCardName = Just cardName} ! []
        SetCategoryOnCard value categoryId cardName ->
            let 
                saveData = model.saveData
                newSaveData = {saveData | cardData = saveData.cardData |> setCategoryOnCard cardName categoryId value }
            in  
                {model | saveData = newSaveData } ! []

setCategory : Bool -> Int -> List Int -> List Int
setCategory value categoryId cardCategoryList =
    let
        containsCategory = cardCategoryList |> List.member categoryId
    in
        case (containsCategory, value) of
        (False, True) -> categoryId::cardCategoryList
        (True, False) -> cardCategoryList |> List.partition (\c -> c == categoryId) |> Tuple.second
        (_, _) -> cardCategoryList

setCategoryOnCard : String -> Int -> Bool -> List CardSaveData -> List CardSaveData
setCategoryOnCard cardName categoryId value cardList =
    cardList |> List.map (\c -> if c.name == cardName then {c | categories = setCategory value categoryId c.categories} else c)

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg
            else
                Decode.fail "not ENTER"
    in
        on "keydown" (Decode.andThen isEnter keyCode)                   

-- Misc 

--findCard : String -> List Card -> Maybe Card
findCard cardName cardList =
    cardList |> List.filter (\c -> c.name == cardName) |> List.head

setImportFieldText : String -> Model -> Model
setImportFieldText text model = 
    let
        old = model.importBoxData
        new = {old | inputFieldText = text}
    in
        {model | importBoxData = new}

setMassImportFieldText : String -> Model -> Model
setMassImportFieldText text model = 
    let
        old = model.importBoxData
        new = {old | massImportFieldText = text}
    in
        {model | importBoxData = new}

setImportStatusText : Maybe String -> Model -> Model
setImportStatusText text model =
    let
        oldImportBoxData = model.importBoxData
        newImportBoxData = {oldImportBoxData | importStatusText = text}
    in
        {model | importBoxData = newImportBoxData}

countOccurences : String -> List String -> Maybe Int
countOccurences character list =
    let 
        count = list |> List.filter (\c -> (String.toUpper c)==character) |> List.length
    in
        case count of
            0 -> Maybe.Nothing
            cnt -> Just cnt
            
getGenericCost : List String -> Maybe Int
getGenericCost list =
    let
        digits = list |> List.filter (\s -> Regex.contains (regex "\\d") s == True)
    in
        digits |> List.head |> Maybe.andThen (\d -> String.toInt d |> Result.toMaybe)

addMaybe : Maybe Int -> Maybe Int -> Maybe Int
addMaybe v1 v2 =
    case v1 of
    Nothing -> v2
    Just x -> 
        case v2 of
        Nothing -> v1
        Just y -> Just (x+y)

parseManacost : String -> ManaCost
parseManacost manaCostString = 
    let
        decurl = Regex.replace Regex.All (regex "[{}]") (\_ -> " ")
        manaSymbols = manaCostString |> decurl |> String.split " "
        generic = getGenericCost manaSymbols 
        b = countOccurences "B" manaSymbols
        u = countOccurences "U" manaSymbols
        g = countOccurences "G" manaSymbols
        r = countOccurences "R" manaSymbols
        w = countOccurences "W" manaSymbols
        x = countOccurences "X" manaSymbols
    in
        {
            generic = generic
            , b = b
            , u = u
            , g = g
            , r = r
            , w = w
            , x = x
            , total = b |> addMaybe u |> addMaybe g |> addMaybe r |> addMaybe w |> addMaybe generic
        }

parseMaybeStringToMaybeInt : Maybe String -> Maybe Int
parseMaybeStringToMaybeInt str =
    case str of
        Nothing ->
            Nothing
        Just s ->
            (String.toInt s) |> 
                (\res -> case res of
                    Ok i ->
                        Just i
                    Err s ->
                        Nothing
                ) 

convertJsonCardIntoCard : JsonCard -> Card   
convertJsonCardIntoCard jsonCard = 
    {
        name = jsonCard.name
        , manaCost = parseManacost jsonCard.manaCost
        , types = jsonCard.typeLine
        , power = parseMaybeStringToMaybeInt jsonCard.power
        , toughness = parseMaybeStringToMaybeInt jsonCard.toughness
        , oracleText = jsonCard.oracleText
        , imageUri = jsonCard.imageUris.normal
        --, categories = []
    }

validateCardNameInput : String -> ( Bool, String )
validateCardNameInput cardNameInput =
    case cardNameInput of
        "" -> (False, "Failed: Import field was empty")
        _ -> (True, "Ok")
            
fetchCard : String -> Cmd Msg
fetchCard cardName = 
    Http.get (apiUrl ++ "/cards/named?fuzzy=\"" ++ cardName ++ "\"") cardDecoder
    |> RemoteData.sendRequest
    |> Cmd.map OnFetchCompleted

cardsDecoder : Decode.Decoder (List JsonCard)
cardsDecoder = 
  Decode.list cardDecoder

imageUrisDecoder = 
  decode JsonImageUris
  |> required "small" Decode.string
  |> required "normal" Decode.string

cardDecoder : Decode.Decoder JsonCard
cardDecoder =
  decode JsonCard
  |> required "name" Decode.string
  |> required "mana_cost" Decode.string
  |> required "type_line" Decode.string
  |> optional "power" (Decode.map Just Decode.string) Nothing
  |> optional "toughness" (Decode.map Just Decode.string) Nothing
  |> required "oracle_text" Decode.string
  |> required "image_uris" imageUrisDecoder


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

renderImportStatusText : Maybe String -> Html Msg
renderImportStatusText imortStatusText = 
    case imortStatusText of
        Nothing -> div [] []
        Just txt -> div [] [text txt]

renderPowerAndToughness : Card -> Html Msg
renderPowerAndToughness card = 
    case (card.power, card.toughness) of
        (Just p, Just t) -> div [] [text ((toString p) ++ "/" ++ (toString t))]
        (_, _) -> div [] []

assembleCostSymbols : String -> Maybe Int -> String
assembleCostSymbols symbol repeats =
    case repeats of
        Just x ->
            String.repeat x symbol
        Nothing ->
            ""

assembleGenericCost : Maybe a -> String
assembleGenericCost cost =
    case cost of
        Just x ->
            toString x
        Nothing ->
            ""
            
renderManaCost : ManaCost -> Html Msg
renderManaCost manaCost =
    div [] [
        text (
            assembleCostSymbols "w" manaCost.w 
            ++ assembleCostSymbols "u" manaCost.u 
            ++ assembleCostSymbols "b" manaCost.b 
            ++ assembleCostSymbols "r" manaCost.r 
            ++ assembleCostSymbols "g" manaCost.g 
            ++ assembleGenericCost manaCost.generic
            ++ assembleCostSymbols "x" manaCost.x
            ++ (case manaCost.total of 
                Just x -> " (" ++ toString x ++ ")"
                Nothing -> "")
        )
        ]

renderCard : Card -> Html Msg
renderCard card = 
    div[ Html.Attributes.style[("border-style", "solid"),("border-width", "1px")], onClick (OnCardSelected card.name) ][
        div [] [text card.name] 
        , div[] [text card.types]
        , renderManaCost card.manaCost
        , renderPowerAndToughness card
        --, div[] [text card.oracleText]
    ]

renderCardList : List Card -> Html Msg
renderCardList cards = 
    div [] (cards |> List.map renderCard)

renderDeckList : List Card -> Html Msg
renderDeckList cards =
    Html.fieldset [][
        text "Deck"
        , renderCardList cards
    ]

renderMaybeBoard : List Card -> Html Msg
renderMaybeBoard cards =
    Html.fieldset [][
        text "Maybe"
        , renderCardList cards
    ]

checkbox isChecked msg name =
  Html.label[ Html.Attributes.style [("padding", "20px")] ]
    [ input [ Html.Attributes.type_ "checkbox", onClick msg, Html.Attributes.checked isChecked ] []
    , text name
    ]

renderMassImportBox inputBoxData =
    div [] [
        textarea [placeholder "Mass import", onInput ChangeMassImportFieldText, value inputBoxData.massImportFieldText] []
        , button [ onClick (MassImportCards (String.split "\n" inputBoxData.massImportFieldText))] [ text "Mass import" ]
        , renderImportStatusText inputBoxData.importStatusText
    ]

renderCardInputBox : ImportBoxData -> Html Msg
renderCardInputBox inputBoxData = 
    Html.fieldset [] [
        input [placeholder "Enter cardname", onInput ChangeInputFieldText, onEnter (AddCard inputBoxData.inputFieldText) , value inputBoxData.inputFieldText] []
        , button [ onClick (AddCard inputBoxData.inputFieldText)] [ text "Add" ]
        , renderImportStatusText inputBoxData.importStatusText
        , renderMassImportBox inputBoxData
    ]

renderCardCategoryCheckbox : String -> List Int -> Category -> Html Msg
renderCardCategoryCheckbox cardName cardCategories category = 
    let
        hasCategory = cardHasCategory category cardCategories
    in
        checkbox hasCategory (SetCategoryOnCard (not hasCategory) category.id cardName) category.name

cardHasCategory category cardCategories =
    List.member category.id cardCategories

renderCardCategoryCheckboxes : String -> List Category -> List Int -> Html Msg
renderCardCategoryCheckboxes cardName availiableCategories cardCategories =
    div [] (availiableCategories |> List.map (\c -> renderCardCategoryCheckbox cardName cardCategories c))

renderExpandedCard : Maybe String -> SaveData -> List Card -> Html Msg
renderExpandedCard expandedCardName saveData cardList =
    case expandedCardName of
    Nothing -> 
        div [][]
    Just cardName ->
        case (findCard cardName cardList, findCard cardName saveData.cardData) of
        (Just c, Just scd) -> 
            div [] 
                [
                    img [src c.imageUri] []
                    , renderCardCategoryCheckboxes c.name saveData.categories scd.categories
                ]
        _ -> 
            div [][]

view : Model -> Html Msg
view model =
    div []
        [ renderCardInputBox model.importBoxData
        , br [] []
        , renderExpandedCard model.expandedCardName model.saveData model.deck
        , renderDeckList model.deck
        --, renderMaybeBoard model.maybeBoard
        ]


-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }