---
# The code, written in elm, uses litvis and elm-vegalite to generate a vega-lite
# visualisation specification. In a litvis preview (in Atom or VS-Code),
# the vega-lite specification json is output below the rendering of the
# visualisation. For more info see:
# - https://github.com/gicentre/litvis
# - https://github.com/gicentre/elm-vegalite

id: litvis

elm:
  dependencies:
    gicentre/elm-vegalite: latest
    gicentre/tidy: latest
---

@import "scdl_litvis_styles.less"

```elm {l=hidden}
import Tidy exposing (..)
import VegaLite exposing (..)
```

# Adult Social Care in England - How much do we know?

```elm {v j interactive}
-- Variable/Function naming conventions
----------------------------------------------------------------
-- dm_* : domain model as defined by The Health Foundation
-- src_* : a source of publically available social care measures
-- msr_* : a publically available measure of social care data
-- make* : parameterised helper functions to avoid code duplication, given we have 3 charts with the same structure, and within each chart there are 3 layers with common encodings.
-- my* : parameterisation variables, to distinguish from elm-vegalite var/functions/types.
-- In the code we take advantage of Elm as a higher level language than the JSON data format, to compose our visualisation with reduced repetition, hence a more maintainable and extensible solution.


adultSocialCareDataMapping : Spec
adultSocialCareDataMapping =
    -- The visualisation has three charts, vertically concatenated. The first chart is an aggregation of publically available measures mapped to level 1 of THF domain model (e.g. Users). The second chart is an aggregation at level 2 (e.g. Users:Demographics). The third chart is the absolute mapping at level 3 (e.g. Users:Demographics:Ethnicity).
    -- Within each chart there are three layers. Layer 1 contains a set of vertically stacked rectangles, one for each domain dimension, colour encoded by the number of measure mappings to the domain. Layer 2 is a set of vertical lines, one per measure. Layer 3 is a set of point marks each representing a mapping from measure to domain; we encode the underlying level 3 domain "phase" (demand, supply, operate, outcome) to the colour channel.
    let
        data =
            dataFromUrl "../data/dm_map_msr_joined_v1.31.csv"

        -- Domain model level 1, fixed height 300
        chartLevel1 =
            makeChart 1 300

        -- Domain model level 2, auto height
        chartLevel2 =
            makeChart 2 0

        -- Domain model level 2, auto height
        chartLevel3 =
            makeChart 3 0

        res =
            resolve
                << resolution
                    (reScale
                        [ ( chColor, reIndependent )
                        ]
                    )
    in
    toVegaLite
        [ data []
        , res []
        , vConcat [ chartLevel1, chartLevel2, chartLevel3 ]
        , title "Adult Social Care in England\nPublically available measures mapped to The Health Foundation three-level domain model." [ tiAnchor anMiddle ]
        ]
```

```elm {l=hidden}
makeChart : Int -> Int -> Spec
makeChart myLevel myHeight =
    let
        -- Layer 1 contains a set of vertically stacked rectangles, one for each domain dimension (e.g. Users at Level 1, Users:Demographics at Level 2, Users:Demographics:Ethnicity at Level 3). We encode the THF domain to the Y-axis, sorted descending to put service users at the top (sadly our best nod to humanism) and colour in inverse proportion to the number of valid measures to draw attention to gaps (i.e. fewer mappings).
        specRectangleValidMeasureCount =
            asSpec
                [ rect (makeRectMarkAttributes myLevel)
                , makeEncRectangleValidMappingCount myLevel []
                , makeTransforms myLevel 1 [] -- layer 1
                ]

        -- Layer 2 is a set of vertical lines, one per measure. Each line extends from between the first and last domain dimensions as defined in layer 1. A bit like the Ranged Dot Plot in the example gallery.
        specLineVerticalMeasure =
            asSpec
                [ line [ maColor "lightgray", maStrokeWidth 1 ]
                , makeEncLineVerticalMeasure myLevel []
                , makeTransforms myLevel 2 [] -- layer 2
                ]

        -- Layer 3 is a set of point marks each representing a mapping from measure to domain. We encode the underlying level 3 domain phase (demand, supply, operate, outcome) to the colour channel.
        specPointMeasureDomainMapping =
            asSpec
                [ point
                    [ maShape symSquare -- just for the legend, we override later with custom SVGs
                    , maSize 1000
                    , maStrokeWidth 0
                    ]
                , makeTransforms myLevel 3 [] -- layer 3
                , makeEncPointMeasureDomainMapping myLevel []
                , makeSelections myLevel []
                ]

        res =
            resolve
                << resolution (reLegend [ ( chShape, reIndependent ) ])
                << resolution
                    (reScale
                        [ ( chColor, reIndependent )
                        , ( chShape, reIndependent )
                        , ( chY, reShared )
                        , ( chX, reShared )
                        ]
                    )
    in
    asSpec
        [ width 1500
        , height (toFloat myHeight)
        , layer
            [ specRectangleValidMeasureCount
            , specLineVerticalMeasure
            , specPointMeasureDomainMapping
            ]
        , res []
        ]


makeEncRectangleValidMappingCount : Int -> List LabelledSpec -> ( VLProperty, Spec )
makeEncRectangleValidMappingCount myLevel =
    -- Layer 1 in each chart is rows of rectangles, with the number of valid measure mappings encoded to the colour channel.
    encoding
        << makePositionY myLevel
        << color
            -- encoding is inverse to number of measures to highlight gaps.
            [ mName "msr_id"
            , mQuant
            , mAggregate opValid
            , mLegend []
            , mScale
                -- Note the reverse scaling, dark to light
                [ scRange (raStrs [ "hsl(0,5%,90%)", "hsl(0,5%,100%)" ])
                ]
            ]
        << tooltips
            --alas this tooltip gets hidden as it is in layer 1
            [ [ tName "msr_id"
              , tQuant
              , tAggregate opValid
              , tTitle "Number of measure mappings in domain. NB: at levels 1 and 2, those in the same phase are hidden behind each other."
              ]
            ]



--------------------------------------------------------------------------------


makeRectMarkAttributes : Int -> List MarkProperty
makeRectMarkAttributes myLevel =
    -- Trying to be a bit Tufte - we don't need strokes at Level 1 because we can afford vertical real estate, but they are necessary at levels 2 and 3
    case myLevel of
        1 ->
            []

        _ ->
            [ maStroke "lightgray", maStrokeWidth 1, maStrokeOpacity 0.5 ]



--------------------------------------------------------------------------------


makePositionY : Int -> List LabelledSpec -> List LabelledSpec
makePositionY myLevel =
    -- All three layers in all three charts share a Y position channel.
    let
        myLevelStr =
            String.fromInt myLevel

        myField =
            case myLevel of
                1 ->
                    "dm_l1_name"

                2 ->
                    "dm_l1_l2_name"

                3 ->
                    "dm_l1_l3_name"

                _ ->
                    "Uh!"

        mySortField =
            "dm_sort_l" ++ myLevelStr

        myAxis =
            pAxis
                [ axTitle ("THF Domain Dimension (Level " ++ myLevelStr ++ ")")
                , axTitleAngle 0
                , axTitleAlign haRight
                , axTitleX -8
                , axTitleY 0
                , axLabelLimit 200
                ]
    in
    position Y
        [ pName myField
        , pNominal
        , pSort [ soByField mySortField opMin, soDescending ]
        , myAxis
        ]



--------------------------------------------------------------------------------


makeEncLineVerticalMeasure : Int -> List LabelledSpec -> ( VLProperty, Spec )
makeEncLineVerticalMeasure myLevel =
    -- Layer 2 in each chart contains vertical lines, one per measure.
    encoding
        << detail [ dName "msr_id", dNominal ]
        << makePositionX myLevel
        << makePositionY myLevel



--------------------------------------------------------------------------------


makePositionX : Int -> List LabelledSpec -> List LabelledSpec
makePositionX myLevel =
    -- In each of the three charts, two of the three layers (line and point) need an X position.
    let
        myAxis =
            case myLevel of
                1 ->
                    pAxis
                        [ axLabels False
                        , axTicks False
                        , axDomain False
                        , axTitle "\nEach vertical is a publically available measure. Hover over the coloured mappings for details; brush over them to filter the chart below; change the horizontal sort order using the drop-down above; click the legend to filter."

                        --, axTitleFontStyle "italic"
                        --, axTitleFontSize 14
                        --, axTitleFontWeight Lighter
                        ]

                _ ->
                    pAxis
                        [ axLabels False
                        , axTicks False
                        , axDomain False
                        , axTitle ""
                        ]
    in
    position X
        [ pName "msr_id"
        , pNominal

        -- See the foldAs transform and the selSortOrder selection to see how the dynamic sort order works.
        , pSort [ soByField "sort_value" opMin, soAscending ]
        , myAxis
        ]



--------------------------------------------------------------------------------


makeEncPointMeasureDomainMapping : Int -> List LabelledSpec -> ( VLProperty, Spec )
makeEncPointMeasureDomainMapping myLevel =
    -- The third layer is a set of point marks which represent a mapping from measure to domain. We encode the underlying level 3 domain phase (demand, supply, operate, outcome) to the colour channel.
    let
        -- Each chart level needs one extra tooltip (dm_l*_name).
        -- This is far too much code for the problem! Should use Array slice instead.
        myToolTipsA =
            [ [ tName "src_org", tNominal, tTitle "Org" ]
            , [ tName "src_name", tNominal, tTitle "Source" ]
            , [ tName "msr_name", tNominal, tTitle "Measure" ]
            , [ tName "msr_strength", tQuant, tTitle "Measure strength" ]
            , [ tName "dm_l1_name", tNominal, tTitle "Level 1 domain" ]
            ]

        myToolTipsB =
            case myLevel of
                1 ->
                    myToolTipsA

                2 ->
                    myToolTipsA ++ [ [ tName "dm_l2_name", tNominal, tTitle "Level 2 domain" ] ]

                _ ->
                    myToolTipsA
                        ++ [ [ tName "dm_l2_name", tNominal, tTitle "Level 2 domain" ]
                           , [ tName "dm_l3_name", tNominal, tTitle "Level 3 domain" ]
                           ]

        myToolTipsC =
            myToolTipsB
                ++ [ [ tName "dm_phase", tNominal, tTitle "Level 3 domain phase" ]
                   , [ tName "msr_id", tQuant, tTitle "Our measure id" ]
                   ]
    in
    encoding
        << makePositionX myLevel
        << makePositionY myLevel
        << shape
            [ mName "dm_phase"
            , mNominal
            , mScale (makePhaseShapeMap myLevel) -- our SVGs
            , mLegend [] -- no legend required as we use the square in makeChart
            ]
        << (case myLevel of
                3 ->
                    fill
                        [ mName "dm_phase"
                        , mNominal
                        , mScale phaseColourMap
                        , mTitle "THF Domain Phase (shift/click)"
                        , mLegend [ leOrient loTop ]
                        ]

                _ ->
                    fill
                        [ mSelectionCondition (selectionName ("selBrushL" ++ String.fromInt myLevel))
                            [ mName "dm_phase"
                            , mNominal
                            , mScale phaseColourMap
                            , mTitle "THF Domain Phase (shift/click)"
                            ]
                            [ mStr "lightgray" ]
                        ]
           )
        << opacity
            [ mName "msr_strength"
            , mQuant
            , mScale
                -- override the default opacity range of 0.3 to 0.8
                [ scDomain (doNums [ 0.1, 1.0 ])
                , scRange (raNums [ 0.0, 1.0 ])
                ]
            , mLegend []
            ]
        << tooltips myToolTipsC



--------------------------------------------------------------------------------


makeTransforms : Int -> Int -> List LabelledSpec -> ( VLProperty, Spec )
makeTransforms myLevel myLayer =
    -- All 3 layers in all 3 charts need to be filtered on sort order and legend. The point layer in levels 1 and 2 also needs to filter just for rows in their level to avoid overplotting the marks and breaking the opacity encoding.
    -- The dynamic sort order trick is borrowed from Jake Vanderplas (for many dynamic encoding use cases). We execute a FoldAs transformation which effectively duplicates every row of our data (we have two sort options), then we transform filter on the rows we want for the selected sort order.
    let
        myTrans =
            transform
                << foldAs [ "msr_strength", "phase_bitmap" ] "sort_by" "sort_value"
                << filter (fiSelection "selSortOrder")
                << filter (fiSelection "selLegend")

        myStrengthFilter =
            filter (fiExpr ("datum.msr_strength_l" ++ String.fromInt myLevel ++ "_fix != 0.0"))

        myBrushFilterOneLevelUp =
            -- brush filter from level above
            filter (fiSelection ("selBrushL" ++ String.fromInt (myLevel - 1)))

        myBrushFilterTwoLevelsUp =
            -- brush filter from level above
            filter (fiSelection ("selBrushL" ++ String.fromInt (myLevel - 2)))
    in
    case myLevel of
        1 ->
            case myLayer of
                3 ->
                    myTrans << myStrengthFilter

                _ ->
                    myTrans

        2 ->
            case myLayer of
                3 ->
                    myTrans << myStrengthFilter << myBrushFilterOneLevelUp

                _ ->
                    myTrans << myBrushFilterOneLevelUp

        _ ->
            myTrans << myBrushFilterOneLevelUp << myBrushFilterTwoLevelsUp



--------------------------------------------------------------------------------


makeSelections : Int -> List LabelledSpec -> ( VLProperty, Spec )
makeSelections myLevel =
    -- Al 3 layers need to know about the selections (albeit they are resolved).
    let
        mySelections =
            selection
                -- Drop down to dynamically change sort order
                << select "selSortOrder"
                    seSingle
                    [ seFields [ "sort_by" ]
                    , seBind
                        [ iSelect "sort_by"
                            [ inOptions
                                [ "msr_strength"
                                , "phase_bitmap"
                                ]
                            , inName "Sort measures horizontally by "
                            ]
                        ]
                    , seInit [ ( "sort_by", str "msr_strength" ) ]
                    ]
                -- An interactive legend for the domain phase (demand, supply, operate, outcome)
                << select "selLegend" seMulti [ seBindLegend [ blField "dm_phase" ] ]
    in
    if myLevel /= 3 then
        -- and a brush which will filter the next level chart(s).
        mySelections << select ("selBrushL" ++ String.fromInt myLevel) seInterval []

    else
        -- no need for a brush at level 3
        mySelections



--------------------------------------------------------------------------------


phaseColourMap =
    -- for the points in layer 3
    categoricalDomainMap
        [ ( "demand", "red" )
        , ( "supply", "orange" )
        , ( "operate", "blue" )
        , ( "outcome", "green" )
        ]



--------------------------------------------------------------------------------


makePhaseShapeMap : Int -> List ScaleProperty
makePhaseShapeMap myLevel =
    --We are also applying a trick with non-overlapping SVG paths to make sure that each domain phase appears in its own sub-row.
    case myLevel of
        1 ->
            categoricalDomainMap
                -- These are non-overlapping, vertically stacked rectangles.
                [ ( "demand", "M -0.05,-1.0 H 0.05 V-0.50 H-0.05 z" )
                , ( "supply", "M -0.05,-0.5 H 0.05 V 0.00 H-0.05 z" )
                , ( "operate", "M -0.05,0.0  H 0.05 V 0.50 H-0.05 z" )
                , ( "outcome", "M -0.05,0.5  H 0.05 V 1.00 H-0.05 z" )
                ]

        2 ->
            -- At level 2 we can get away with pairs overlapping, but only because we got lucky with the shape of the data! I admit this is a dirty hack, but it saves vertical real estate for this data set.
            categoricalDomainMap
                [ ( "demand", "M -0.05,-0.5 H 0.05 V 0.00 H-0.05 z" )
                , ( "supply", "M -0.05,-0.5 H 0.05 V 0.00 H-0.05 z" )
                , ( "operate", "M -0.05,0.0  H 0.05 V 0.50 H-0.05 z" )
                , ( "outcome", "M -0.05,0.0  H 0.05 V 0.50 H-0.05 z" )
                ]

        _ ->
            -- At level 3 the SVG trick isn't strictly necessary as each row is unique in terms of phase, but lets stick to the pattern anyway, so the point sizing can be the same in every level.
            categoricalDomainMap
                [ ( "demand", "M -0.05,-0.25 H 0.05 V 0.25 H-0.05 z" )
                , ( "supply", "M -0.05,-0.25 H 0.05 V 0.25 H-0.05 z" )
                , ( "operate", "M -0.05,-0.25 H 0.05 V 0.25 H-0.05 z" )
                , ( "outcome", "M -0.05,-0.25 H 0.05 V 0.25 H-0.05 z" )
                ]
```
