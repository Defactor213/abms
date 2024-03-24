globals[
  total_houses_sold
  total_houses_sold_chinese
  total_houses_sold_chinese_price
  total_houses_sold_chinese_average
  total_houses_sold_indian
  total_houses_sold_indian_price
  total_houses_sold_indian_average
  total_houses_sold_malay
  total_houses_sold_malay_price
  total_houses_sold_malay_average
  total_houses_sold_others
  total_houses_sold_others_price
  total_houses_sold_others_average
]
breed [sellers seller]
breed [buyers buyer]

;---------@ERIC add what u need--------
sellers-own[
  seller-id
  race
  ask-price
  selling?
  room_type
  family_size
]

buyers-own[
  income
  race
  first_time?
  offer_price
  room_type
  family_size
]

to setup

  clear-all
  setup-hdb
  setup-buyers 100
  reset-ticks
end

; Setup the Singapore Map and the HDB around Singapore
to setup-hdb
  import-pcolors "singaporeMapBin.jpeg"
  ask patches with [pcolor = black]
  [
    set pcolor red
  ]
  ask patches with [pcolor = white]
  [
    set pcolor black
  ]
  ask patch 0 0 [
    set pcolor black
    ask neighbors [
      set pcolor black
    ]
  ]
  ;---------NEED CHANGE TO SUIT SINGAPORE MAP-------------
  let center-x max-pxcor / 2
  let center-y max-pycor / 2

  let red-patches patches with [pcolor = red]
  ifelse any? red-patches [
    set center-x mean [pxcor] of red-patches
    set center-y mean [pycor] of red-patches
  ] [
    print "No red patches found"
  ]

  ; Adjust HDB distribution according to Singapore map
  ask patches with [pcolor = red] [
    let distance-from-center distancexy center-x center-y
    ; Fine-tune this formula to reduce the overall density of HDB blocks
    let prob-hdb (0.3 - (distance-from-center / (world-width / 2)) * 0.5)
    if random-float 1 < prob-hdb [
      set pcolor green  ; Mark as HDB block
      setup-sellers 100
    ]
  ]

end

; Setup the sellers
to setup-sellers [num-sellers]
  ;--------TO CHANGE

  ; Initial counts for each race
  let count-chinese 0
  let count-indian 0
  let count-malay 0
  let count-others 0

  ; Maximum allowed sellers for each race based on percentages
  ;-----------TO CHANGE
  let max-chinese eip_chinese * num-sellers
  let max-indian eip_indian * num-sellers
  let max-malay eip_malay * num-sellers
  let max-others eip_others * num-sellers

  ; Create sellers and assign their properties
  sprout-sellers num-sellers[
    set shape "house"
    set color green

    ; Determine the seller's race
    let my-race pick-race count-chinese max-chinese count-indian max-indian count-malay max-malay count-others max-others
    set race my-race

    ; Update race counts
    if my-race = "Chinese" [ set count-chinese count-chinese + 1 ]
    if my-race = "Indian" [ set count-indian count-indian + 1 ]
    if my-race = "Malay" [ set count-malay count-malay + 1 ]
    if my-race = "Others" [ set count-others count-others + 1 ]

    ; Initialize other seller variables

    ; -----------TO CHANGE
    let my-family-size 1 + random 5  ; random family size between 1 and 5
    set family_size my-family-size
    ;; 2,3,4,5,
    ifelse (my-family-size <= 2) [
      set room_type "2-room"
    ] [
      ifelse (my-family-size <= 3) [
        set room_type "3-room"
      ] [
        ifelse (my-family-size <= 4) [
          set room_type "4-room"
        ] [
          set room_type "5-room"
        ]
      ]
    ]

    ; ----- PLACEHOLDER for ask-price initialization
    set ask-price (random-float 100) + 100  ; Example: random ask-price between 100 and 200

    ; ----- PLACEHOLDER for chances of selling
    let selling_var? random-float 1.0 < 0.5  ; 50% chance of selling the house

    set selling? selling_var?

    if not selling?[
      set color yellow
    ]
  ]
end

; Picks the race of the seller (within the Ethnic Integration Policy)
to-report pick-race [c-chinese max-chinese c-indian max-indian c-malay max-malay c-others max-others]
  let available-races []

  ; Correctly update the available-races list using lput
  if c-chinese < max-chinese [
    set available-races lput "Chinese" available-races
  ]
  if c-indian < max-indian [
    set available-races lput "Indian" available-races
  ]
  if c-malay < max-malay [
    set available-races lput "Malay" available-races
  ]
  if c-others < max-others [
    set available-races lput "Others" available-races
  ]

  ; Check if the list is empty and report accordingly
  ifelse empty? available-races [
    report "Error"  ; This is a safeguard and indicates a problem with the setup if triggered
  ] [
    report one-of available-races  ; Randomly select one of the available races
  ]
end

; Creating buyer agents
to setup-buyers [num_to_create]

  create-buyers num_to_create ; Adjust the number of buyers as needed
  [
    set shape "person"
    set color blue
    set size 4

    ; Determine the buyer's race
    let races ["Chinese" "Indian" "Malay" "Others"]
    let my-race one-of races

    ;---------change the mean and std deviation for income level --------
    ; Determine the income level
    ; -----------TO CHANGE
    let mean-income
    let std-deviation 2000 ; NEEDA CHANGE
    let my-income random-normal mean-income std-deviation

    ; PLACEHOLDER Determine if its firsttimer or not
    let first-timer? random-float 1.0 < 0.5  ; 50% chance of being a first-time buyer
    let first-timer ifelse-value (first-timer?) [true] [false]

    ; -----------TO CHANGE
    ; Determine the offer price of the buyer
    let my-offer-price random 980000 + 20000

    ; -----------TO CHANGE
    let my-family-size 1 + random 5  ; random family size between 1 and 5
    set family_size my-family-size
    ;; 2,3,4,5,
    ifelse (my-family-size <= 2) [
      set room_type "2-room"
    ] [
      ifelse (my-family-size <= 3) [
        set room_type "3-room"
      ] [
        ifelse (my-family-size <= 4) [
          set room_type "4-room"
        ] [
          set room_type "5-room"
        ]
      ]
    ]


    ; Assigning initialized attributes to each buyer
    set race my-race
    set income my-income
    set first_time? first-timer?
    set offer_price my-offer-price

    ; Find all black patches
    let black-patches patches with [pcolor = black]

    ; Choose one random red patch
    let selected-patch one-of black-patches

    ; Assigning position to buyer on the selected red patch
    move-to selected-patch
  ]
end



; The main simulation loop
to go
  set total_houses_sold 0
  set total_houses_sold_chinese 0
  set total_houses_sold_indian 0
  set total_houses_sold_others 0
  set total_houses_sold_malay 0

  ; Placeholder for the main simulation steps, such as moving buyers, initiating transactions.

  ask buyers [
    ifelse government_policy? and ethnic-integration_policy?[
      buyer-initiate-meeting
    ] [
      ;; if there is no ethnic intergation policy only
      ifelse not ethnic-integration_policy? and government_policy?[
        buyer-initiate-meeting_no_eip
      ][
        ;; if there is no government policy only
        ifelse ethnic-integration_policy? and not government_policy?[
          buyer-initiate-meeting_no_government
        ][
          buyer-initiate-meeting_no_eip_no_governemnt
        ]
      ]
    ]

  ]
  tick
  seller-selling-again
  avg_prices_by_race
  ;; update-mean-offer-price-histogram
  update-total-houses-sold-histogram

  if ticks mod 1000 = 0 [
    ask buyers [
      set income (income * (1 + inflation))
    ]
  ]

  setup-buyers 10
end

to buyer-initiate-meeting
  print("buyer-initiate-meeting")
  ; Get all eligible sellers (if selling? is true)
  let eligible-sellers sellers with [selling? = true]
  ; If there are any eligible sellers
  ifelse any? eligible-sellers [
    ; Check if the sellers have the same room type as the buyers
    let same_room_type eligible-sellers with [room_type = [room_type] of self]
    ; Calculate the grant that the buyers would get (their new offer price)
    ifelse any? same_room_type [
      let buyer_price offer_price
      if first_time? [
        let fam_grant family_grant_discount room_type income
        let ehg enhanced_housing_grant income
        set buyer_price offer_price - fam_grant - ehg
        print (word "After grant: " buyer_price)
      ]
      ; Check if the sellers' ask price <= buyer's offer price
      print(word "same_room_type" same_room_type)

      let max_ask_price max [ask-price] of eligible-sellers
      let lowest_ask_price max_ask_price
      let lowest_ask_seller nobody

      ask same_room_type with [ask-price <= buyer_price][
        ; Get the other sellers on the same patch as the current_seller
        let other_sellers other turtles-on patch-here
        let my-list sort (turtle-set other_sellers)

          ; Initialize counters for each ethnicity
          let count_chinese 0
          let count_indian 0
          let count_malay 0
          let count_others 0
          ; Loop through each other seller
          foreach my-list [other_seller ->
            ; Check the ethnicity of the other_seller and update the corresponding counter
            if [race] of other_seller = "Chinese" [
              set count_chinese count_chinese + 1
            ]
            if [race] of other_seller = "Indian" [
              set count_indian count_indian + 1
            ]
            if [race] of other_seller = "Malay" [
              set count_malay count_malay + 1
            ]
            if [race] of other_seller = "Others" [
              set count_others count_others + 1
            ]
          ]

          ;; from the list of sellers check what races they are able to sell to
          let races_to_sell_to eip_checker count_chinese count_indian count_malay count_others

          ;; check if the current seller has the same race as the races_to_sell_to list and if the ask_price is lower than the previous agent
        if member? [race] of self races_to_sell_to and ask-price <= lowest_ask_price [
          set lowest_ask_price ask-price
          set lowest_ask_seller self
        ]

        ]

      print(word"sold to" lowest_ask_seller)

      if lowest_ask_seller != nobody[
          ask lowest_ask_seller [
          set color yellow   ; Change color of the seller to yellow
          set selling? false  ; Set the selling? variable of the seller to false
          set race [race] of self ; Change the race of the seller to the buyers race (since they have alr sold it)

        ]
        set total_houses_sold total_houses_sold + 1

        ifelse race = "Chinese"[
          set total_houses_sold_chinese total_houses_sold_chinese + 1
            set total_houses_sold_chinese_price total_houses_sold_chinese_price + offer_price
        ][
          ifelse race = "Indian" [
              set total_houses_sold_indian total_houses_sold_indian + 1
              set total_houses_sold_indian_price total_houses_sold_indian_price + offer_price
          ][
            ifelse race = "Malay" [
                set total_houses_sold_malay total_houses_sold_malay + 1
                set total_houses_sold_malay_price total_houses_sold_malay_price + offer_price
            ][
                set total_houses_sold_others total_houses_sold_others + 1
                set total_houses_sold_others_price total_houses_sold_others_price + offer_price
            ]
          ]
        ]
      ]

      ] [
        print "No affordable sellers found"
      ]
    ] [
      print "No sellers with the same room type"
    ]
end


to buyer-initiate-meeting_no_government
  print("buyer-initiate-meeting_no_government")
  ; Get all eligible sellers (if selling? is true)
  let eligible-sellers sellers with [selling? = true]
  ; If there are any eligible sellers
  ifelse any? eligible-sellers [
    ; Check if the sellers have the same room type as the buyers
    let same_room_type eligible-sellers with [room_type = [room_type] of self]
    ; Calculate the grant that the buyers would get (their new offer price)
    ifelse any? same_room_type [
      let buyer_price offer_price
      ; Check if the sellers' ask price <= buyer's offer price
      print(word "same_room_type" same_room_type)

      let max_ask_price max [ask-price] of eligible-sellers
      let lowest_ask_price max_ask_price
      let lowest_ask_seller nobody

      ask same_room_type with [ask-price <= buyer_price][
        ; Get the other sellers on the same patch as the current_seller
        let other_sellers other turtles-on patch-here
        let my-list sort (turtle-set other_sellers)

          ; Initialize counters for each ethnicity
          let count_chinese 0
          let count_indian 0
          let count_malay 0
          let count_others 0
          ; Loop through each other seller
          foreach my-list [other_seller ->
            ; Check the ethnicity of the other_seller and update the corresponding counter
            if [race] of other_seller = "Chinese" [
              set count_chinese count_chinese + 1
            ]
            if [race] of other_seller = "Indian" [
              set count_indian count_indian + 1
            ]
            if [race] of other_seller = "Malay" [
              set count_malay count_malay + 1
            ]
            if [race] of other_seller = "Others" [
              set count_others count_others + 1
            ]
          ]

          ;; from the list of sellers check what races they are able to sell to
          let races_to_sell_to eip_checker count_chinese count_indian count_malay count_others

          ;; check if the current seller has the same race as the races_to_sell_to list and if the ask_price is lower than the previous agent
        if member? [race] of self races_to_sell_to and ask-price <= lowest_ask_price [
          set lowest_ask_price ask-price
          set lowest_ask_seller self
        ]

        ]

      print(word"sold to" lowest_ask_seller)

      if lowest_ask_seller != nobody[
          ask lowest_ask_seller [
          set color yellow   ; Change color of the seller to yellow
          set selling? false  ; Set the selling? variable of the seller to false
          set race [race] of self ; Change the race of the seller to the buyers race (since they have alr sold it)

        ]
        set total_houses_sold total_houses_sold + 1

        ifelse race = "Chinese"[
          set total_houses_sold_chinese total_houses_sold_chinese + 1
            set total_houses_sold_chinese_price total_houses_sold_chinese_price + offer_price
        ][
          ifelse race = "Indian" [
              set total_houses_sold_indian total_houses_sold_indian + 1
              set total_houses_sold_indian_price total_houses_sold_indian_price + offer_price
          ][
            ifelse race = "Malay" [
                set total_houses_sold_malay total_houses_sold_malay + 1
                set total_houses_sold_malay_price total_houses_sold_malay_price + offer_price
            ][
                set total_houses_sold_others total_houses_sold_others + 1
                set total_houses_sold_others_price total_houses_sold_others_price + offer_price
            ]
          ]
        ]
      ]

      ] [
        print "No affordable sellers found"
      ]
    ] [
      print "No sellers with the same room type"
    ]
end

to buyer-initiate-meeting_no_eip
  print("buyer-initiate-meeting_no_eip")
  ; Get all eligible sellers (if selling? is true)
  let eligible-sellers sellers with [selling? = true]
  ; If there are any eligible sellers
  ifelse any? eligible-sellers [
    ; Check if the sellers have the same room type as the buyers
    let same_room_type eligible-sellers with [room_type = [room_type] of self]
    ; Calculate the grant that the buyers would get (their new offer price)
    ifelse any? same_room_type [
      let buyer_price offer_price
      if first_time? [
        let fam_grant family_grant_discount room_type income
        let ehg enhanced_housing_grant income
        set buyer_price offer_price - fam_grant - ehg
        print (word "After grant: " buyer_price)
      ]
      ; Check if the sellers' ask price <= buyer's offer price
      print(word "same_room_type" same_room_type)

      let max_ask_price max [ask-price] of eligible-sellers
      let lowest_ask_price max_ask_price
      let lowest_ask_seller nobody

      ask same_room_type with [ask-price <= buyer_price][
        ;; check if the ask_price is lower than the previous agent
        if ask-price <= lowest_ask_price [
          set lowest_ask_price ask-price
          set lowest_ask_seller self
        ]

      ]

      print(word"sold to" lowest_ask_seller)

      if lowest_ask_seller != nobody[
        ask lowest_ask_seller [
          set color yellow   ; Change color of the seller to yellow
          set selling? false  ; Set the selling? variable of the seller to false
          set race [race] of self ; Change the race of the seller to the buyers race (since they have alr sold it)

        ]
        set total_houses_sold total_houses_sold + 1
      ]
    ][
      print "No affordable sellers found"
    ]

  ] [
    print "No affordable sellers found"
  ]
end

to buyer-initiate-meeting_no_eip_no_governemnt
  print("buyer-initiate-meeting_no_eip_no_governemnt")
  ; Get all eligible sellers (if selling? is true)
  let eligible-sellers sellers with [selling? = true]
  ; If there are any eligible sellers
  ifelse any? eligible-sellers [
    ; Check if the sellers have the same room type as the buyers
    let same_room_type eligible-sellers with [room_type = [room_type] of self]
    ; Calculate the grant that the buyers would get (their new offer price)
    ifelse any? same_room_type [
      let buyer_price offer_price

      ; Check if the sellers' ask price <= buyer's offer price
      print(word "same_room_type" same_room_type)

      let max_ask_price max [ask-price] of eligible-sellers
      let lowest_ask_price max_ask_price
      let lowest_ask_seller nobody

      ask same_room_type with [ask-price <= buyer_price][
        ;; check if the ask_price is lower than the previous agent
        if ask-price <= lowest_ask_price [
          set lowest_ask_price ask-price
          set lowest_ask_seller self
        ]

      ]

      print(word"sold to" lowest_ask_seller)

      if lowest_ask_seller != nobody[
        ask lowest_ask_seller [
          set color yellow   ; Change color of the seller to yellow
          set selling? false  ; Set the selling? variable of the seller to false
          set race [race] of self ; Change the race of the seller to the buyers race (since they have alr sold it)

        ]
        set total_houses_sold total_houses_sold + 1
      ]
    ][
      print "No affordable sellers found"
    ]

  ] [
    print "No affordable sellers found"
  ]
end



to-report eip_checker [count_chinese count_indian count_malay count_others]
  let results []
  let total count_chinese + count_indian + count_malay + count_others

  if count_chinese / total < eip_chinese[
    set results lput "Chinese" results
  ]

  if count_indian / total < eip_indian[
    set results lput "Indian" results
  ]

  if count_malay / total < eip_malay[
    set results lput "Malay" results
  ]

  if count_others / total < eip_others[
    set results lput "Others" results
  ]

  report results
end




;; Check if seller willing to sell again
to seller-selling-again
  ask sellers with [selling? = true] [
    ask one-of sellers-here [
      ; -----------TO CHANGE
      let selling_var? random-float 1.0 < 0.2  ; 50% chance of being a first-time buyer

      ;; If seller is willing to sell again
      if selling?[
        set color green ; Change the patch color back to green
        set selling? selling_var? ; Change the variable back to true

        ; -----------TO CHANGE
        set ask-price (random-float 100) + 100 ; set the ask price of the house
      ]
    ]
  ]

  ;; else it would remain as they are not willing to sell
end

to-report houses_not_sold
  let houses count turtles with [color = green]
  let not_sold houses - total_houses_sold
  report not_sold
end

to avg_prices_by_race
  ifelse total_houses_sold_malay != 0 [
    set total_houses_sold_malay_average total_houses_sold_malay_price / total_houses_sold_malay
  ] [
    set total_houses_sold_malay_average 0
  ]

  ifelse total_houses_sold_chinese != 0 [
    set total_houses_sold_chinese_average total_houses_sold_chinese_price / total_houses_sold_chinese
  ] [
    set total_houses_sold_chinese_average 0
  ]

  ifelse total_houses_sold_indian != 0 [
    set total_houses_sold_indian_average total_houses_sold_indian_price / total_houses_sold_indian
  ] [
    set total_houses_sold_indian_average 0
  ]

  ifelse total_houses_sold_others != 0 [
    set total_houses_sold_others_average total_houses_sold_others_price / total_houses_sold_others
  ] [
    set total_houses_sold_others_average 0
  ]

end

to-report family_grant_discount [chosen_room_type fam_income]
  ifelse fam_income < family_grant_income_level [
    ifelse chosen_room_type = "2-room" or chosen_room_type = "3-room" or chosen_room_type = "4-room" [
      report 80000
    ] [
      report 50000
    ]
  ][
    report 0
  ]
end

to-report enhanced_housing_grant [fam_income]
  ifelse fam_income <= 1500[
    report 80000
  ][
    ifelse fam_income <= 2000[
      report 75000
    ][
      ifelse fam_income <= 2500[
        report 70000
      ][
        ifelse fam_income <= 3000[
          report 65000
        ][
          ifelse fam_income <= 3500[
            report 60000
          ][
            ifelse fam_income <= 4000[
              report 55000
            ][
              ifelse fam_income <= 4500[
                report 50000
              ][
                ifelse fam_income <= 5000[
                  report 45000
                ][
                  ifelse fam_income <= 5500[
                    report 40000
                  ][
                    ifelse fam_income <= 6000[
                      report 35000
                    ][
                      ifelse fam_income <= 6500[
                        report 30000
                      ][
                        ifelse fam_income <= 7000[
                          report 25000
                        ][
                          ifelse fam_income <= 7500[
                            report 20000
                          ][
                            ifelse fam_income <= 8000[
                              report 15000
                            ][
                              ifelse fam_income <= 8500[
                                report 10000
                              ][
                                ifelse fam_income <= 9000[
                                  report 5000
                                ][
                                  report 0
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
end

to update-total-houses-sold-histogram
  ; Clear existing histogram
  set-current-plot "Houses Sold by Ethnicity"
  set-current-plot-pen "sold_ethnicity"
  let houses_sold []
  set houses_sold lput total_houses_sold_chinese houses_sold
  set houses_sold lput total_houses_sold_indian houses_sold
  set houses_sold lput total_houses_sold_malay houses_sold
  set houses_sold lput total_houses_sold_others houses_sold

  ; Create histogram
  histogram houses_sold
end

; Additional functions to simulate buyer and seller interactions, transactions,
; and the effects of government policies will be needed here.

; For example:
; to simulate-transactions
; to apply-government-policies

; This foundational model needs to be expanded with real data integration and
; more detailed interactions based on the project proposal.
@#$#@#$#@
GRAPHICS-WINDOW
271
12
880
472
-1
-1
2.993
1
10
1
1
1
0
1
1
1
-100
100
-75
75
0
0
1
ticks
30.0

BUTTON
18
17
84
50
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
955
141
1088
186
Number of Sellers Selling
count turtles with [color = green]
17
1
11

BUTTON
18
60
81
93
Go
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
955
52
1061
97
Number of Buyers
count turtles with [color = blue]
17
1
11

BUTTON
17
105
113
138
Go (forever)
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1273
15
1720
194
Number of Buyers, Sellers (selling and not selling)
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Buyer" 1.0 0 -13791810 true "" "plot count turtles with [color = blue]"
"Seller not Selling" 1.0 0 -1184463 true "" "plot count turtles with [color = yellow]"
"Seller" 1.0 0 -11085214 true "" "plot count turtles with [color = green]"

MONITOR
1070
52
1168
97
Number of Seller
count turtles with [color = green or color = yellow]
17
1
11

MONITOR
1105
140
1253
185
Number of sellers not selling
count turtles with [color = yellow]
17
1
11

TEXTBOX
952
19
1172
53
Number of agents in the model
14
0.0
1

TEXTBOX
957
106
1175
140
Number of sellers in the model
14
0.0
1

PLOT
1275
223
1724
403
Number of Houses 
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Total Houses " 1.0 0 -14070903 true "" "plot count turtles with [color = green]"
"Houses Sold" 1.0 0 -11085214 true "" "plot total_houses_sold"
"Houses Not Sold" 1.0 0 -2674135 true "" "plot houses_not_sold"

MONITOR
959
314
1094
359
Total number of houses
count turtles with [color = green]
17
1
11

MONITOR
1069
260
1170
305
Total houses sold
total_houses_sold
17
1
11

MONITOR
959
258
1054
303
Houses not sold
houses_not_sold
17
1
11

TEXTBOX
959
230
1197
264
Number of houses in the model
14
0.0
1

TEXTBOX
961
414
1144
448
Houses Sold by ethinicity 
14
0.0
1

MONITOR
958
447
1050
492
Sold to Chinese
total_houses_sold_chinese
17
1
11

MONITOR
1061
447
1160
492
Sold to Indians
total_houses_sold_indian
17
1
11

MONITOR
959
505
1041
550
Sold to Malay
total_houses_sold_malay
17
1
11

MONITOR
1061
503
1145
548
Sold to others
total_houses_sold_others
17
1
11

PLOT
1277
422
1710
602
Average Price of houses by ethnicity 
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Chinese" 1.0 0 -11085214 true "" "plot total_houses_sold_chinese_average"
"Indian" 1.0 0 -14070903 true "" "plot total_houses_sold_indian_average"
"Malay" 1.0 0 -2674135 true "" "plot total_houses_sold_malay_average"
"Others" 1.0 0 -955883 true "" "plot total_houses_sold_others_average"

SWITCH
15
182
171
215
government_policy?
government_policy?
0
1
-1000

SLIDER
14
224
201
257
family_grant_income_level
family_grant_income_level
0
30000
21000.0
1000
1
NIL
HORIZONTAL

SLIDER
15
344
129
377
eip_chinese
eip_chinese
0
1
0.87
0.1
1
NIL
HORIZONTAL

SLIDER
136
345
252
378
eip_indian
eip_indian
0
1
0.12
0.1
1
NIL
HORIZONTAL

SLIDER
15
387
129
420
eip_malay
eip_malay
0
1
0.25
0.1
1
NIL
HORIZONTAL

SLIDER
135
388
251
421
eip_others
eip_others
0
1
0.15
0.1
1
NIL
HORIZONTAL

TEXTBOX
15
158
165
176
Government Variables
14
0.0
1

TEXTBOX
18
276
210
310
Ethinic Integration Policy
14
0.0
1

SLIDER
17
462
175
495
buyer_mean_income
buyer_mean_income
0
100000
21000.0
1000
1
NIL
HORIZONTAL

TEXTBOX
19
438
169
456
Affodability
14
0.0
1

PLOT
694
490
894
640
Houses Sold by Ethnicity
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"sold_ethnicity" 1.0 1 -16777216 true "" ""

SWITCH
16
301
203
334
ethnic-integration_policy?
ethnic-integration_policy?
0
1
-1000

SLIDER
18
513
176
546
inflation
inflation
0
10
2.0
0.1
1
%
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
