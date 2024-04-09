extensions [rnd]

globals[
  total_houses_sold
  total_houses_sold_per_tick
  total_houses_sold_price
  total_houses_sold_average
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
  mean_house_price
  seller_house_constant
  seller_lease_year_constant
  seller_constant
  buyer_house_constant
  buyer_neighborhood_constant
  buyer_family_constant
  buyer_lease_constant
  buyer_constant
  affordability

]
breed [sellers seller]
breed [buyers buyer]

sellers-own[
  seller-id
  race
  ask-price
  selling?
  room_type
  family_size
  lease_years
  neighborhood_score
]

buyers-own[
  income
  race
  first_time?
  offer_price
  room_type
  family_size
  neighborhood_score
]

to setup
  clear-all
  clear-output
  setup-house-price
  setup-hdb
  setup-seller-constant
  setup-buyer-constant
  setup-buyers 100
  reset-ticks
end

to paint-hdb
  if mouse-down?[
    ask patch mouse-xcor mouse-ycor [
      ifelse not any? sellers-here[
        if pcolor = red [
          if paint-hdb-as = "selling"[
            setup-sellers_paint number_of_hdb_units paint-hdb-as
          ]
          if paint-hdb-as = "not-selling"[
            setup-sellers_paint number_of_hdb_units paint-hdb-as
          ]
        ]
      ][
        if paint-hdb-as = "remove"[
         ask sellers-here [die]
        ]
      ]
      display
    ]
  ]
end

to setup-house-price
  ;; starting mean price from jan 2017
  set mean_house_price 427506.984
end

to setup-seller-constant
  ; Mean values for the constants
  let seller_constant1_mean 0.946
  let seller_constant2_mean 0.00887
  let seller_constant3_mean -0.0144

  ; Generate normally distributed constants
  set seller_house_constant generate-normal seller_constant1_mean (seller_constant1_mean * 0.01)
  set seller_lease_year_constant generate-normal seller_constant2_mean (seller_constant2_mean * 0.01)
  set seller_constant generate-normal seller_constant3_mean (abs(seller_constant3_mean) * 0.01)

end
to setup-buyer-constant
  ; Generate normally distributed constants with 1% STD
  set buyer_house_constant generate-normal 0.902 (0.902 * 0.01)
  set buyer_neighborhood_constant generate-normal 0.0132 (0.0132 * 0.01)
  set buyer_family_constant generate-normal 0.235 (0.235 * 0.01)
  set buyer_lease_constant generate-normal 0.00603 (0.00603 * 0.01)
  set buyer_constant generate-normal -0.263 (abs(-0.263) * 0.01)
end


to-report generate-normal [average stddev]
  ; NetLogo's random-normal function generates a normally distributed random float
  report random-normal average stddev
end


; Setup the sellers
to setup-sellers_paint [num-sellers selling_variable]
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

    ; Randomly assign the lease year to the blocks
    set lease_years random 50 + 50

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
    let my-family-size pick-household-size  ; random fammily size between 1-6
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

    ; set ask-price (random-float 100) + 100  ; Example: random ask-price between 100 and 200
    set ask-price exp((seller_house_constant * ln(mean_house_price) + seller_lease_year_constant * lease_years) + seller_constant)

    ifelse selling_variable = "selling"[
      set selling? true
      set color green
    ][
      set selling? false
      set color yellow
    ]


  ]
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
  ;  print "No red patches found"
  ]

  ; Adjust HDB distribution according to Singapore map
  ask patches with [pcolor = red] [
    let distance-from-center distancexy center-x center-y
    ; Fine-tune this formula to reduce the overall density of HDB blocks
    let prob-hdb (0.3 - (distance-from-center / (world-width / 2)) * density_of_hdb_blocks)
    if random-float 1 < prob-hdb [
      ;; set pcolor green  ; Mark as HDB block
      setup-sellers number_of_hdb_units
    ]
  ]

end

to-report pick-neighborhood-score
  let scores [1 2 3 4 5 6 7 8 9 10]
  let weights [0.0142 0.0739 0.106 0.123 0.127 0.123 0.106 0.0739 0.0142 0.00926]
  ; Combine the items and weights into pairs
  let pairs (map [ [i w] -> (list i w) ] scores weights)

  ; Use rnd:weighted-one-of-list to pick an item based on weights
  let chosen-pair rnd:weighted-one-of-list pairs [ [p] -> last p ]

  ; Report the first item of the chosen pair, which is the household size
  report first chosen-pair
end

to-report pick-household-size
  let items [1 2 3 4 5 6]
  let weights [0.155 0.245 0.22 0.2 0.11 0.07]

  ; Combine the items and weights into pairs
  let pairs (map [ [i w] -> (list i w) ] items weights)

  ; Use rnd:weighted-one-of-list to pick an item based on weights
  let chosen-pair rnd:weighted-one-of-list pairs [ [p] -> last p ]

  ; Report the first item of the chosen pair, which is the household size
  report first chosen-pair
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

    ; Randomly assign the lease year to the blocks
    set lease_years random 50 + 50

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
    let my-family-size pick-household-size  ; random fammily size between 1-6
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

    set ask-price exp((seller_house_constant * ln(mean_house_price) + seller_lease_year_constant * lease_years) + seller_constant)


    let selling_var? random-float 1.0 < prob_seller_selling

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

  let income-brackets [
      ["HDB 1- & 2-Room Flats" 1879 0.056]
      ["HDB 3-Room Flats" 4800 0.244]
      ["HDB 4-Room Flats" 6483 0.388]
      ["HDB 5-Room & Executive Flats" 9186 0.312]
    ]

  create-buyers num_to_create ; Adjust the number of buyers as needed
  [
    set shape "person"

    ; Determine the buyer's race
    let races ["Chinese" "Indian" "Malay" "Others"]
    let my-race one-of races
    ifelse my-race = "Chinese" [
      set color sky
    ][
      ifelse my-race = "Indian"[
        set color pink
      ][
        ifelse my-race = "Malay" [
          set color violet
        ][
          set color orange
        ]
      ]
    ]


    ; PLACEHOLDER Determine if its firsttimer or not
    let first-timer? random-float 1.0 < prob_of_first_timer

    ; -----------TO CHANGE
    let my-family-size pick-household-size; random fammily size between 1-6
    let my-neighborhood-score pick-neighborhood-score
    set family_size my-family-size
    set neighborhood_score my-neighborhood-score
    ;; 2,3,4,5,
    ifelse (my-family-size <= 2) [
      set room_type "2-room"
      set size 4
    ] [
      ifelse (my-family-size <= 3) [
        set room_type "3-room"
        set size 5
      ] [
        ifelse (my-family-size <= 4) [
          set room_type "4-room"
          set size 6
        ] [
          set room_type "5-room"
          set size 7
        ]
      ]
    ]
    let my-income 0
    ifelse (my-family-size <= 2)[
      let std-deviation 0.1 * 1879
      set my-income random-normal 1879 std-deviation
    ][
      ifelse (my-family-size <= 3) [
        let std-deviation 0.1 * 4800
        set my-income random-normal 4800 std-deviation
      ] [
        ifelse (my-family-size <= 4) [
          let std-deviation 0.1 * 6483
          set my-income random-normal 6483 std-deviation
        ] [
          let std-deviation 0.1 * 9186
          set my-income random-normal 9186 std-deviation
        ]
      ]
    ]
    let remaining-lease random 50 + 50
        ; -----------TO CHANGE
    ; Determine the offer price of the buyer
    ; let my-offer-price random 980000 + 20000
    let my-offer-price exp((buyer_house_constant * ln(mean_house_price)) + (buyer_neighborhood_constant * neighborhood_score) + (buyer_family_constant * family_size) + (buyer_lease_constant * remaining-lease) - buyer_constant)


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
  set affordability total_houses_sold_average / mean [income] of buyers
end

; The main simulation loop
to go
  ;set total_houses_sold 0
  set total_houses_sold_per_tick 0
  ;set total_houses_sold_chinese 0
  ;set total_houses_sold_indian 0
  ;set total_houses_sold_others 0
  ;set total_houses_sold_malay 0

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
  setup-seller-constant
  setup-buyer-constant
  tick
  decrease_lease_year
  seller-selling-again
  avg_prices_by_race
  ;; update-mean-offer-price-histogram

  if ticks mod 6 = 0 [
    ask buyers [
      set income (income * (1 + income_growth / 2))
    ]
    set mean_house_price (mean_house_price * (1 + inflation / 2))
  ]

  setup-buyers number_of_buyers
 ; print(total_houses_sold_per_tick)
end

to decrease_lease_year
  ask sellers [
    set lease_years lease_years - 0.833333
    if lease_years = 0 [
      set lease_years 99
    ]
  ]

end

to buyer-initiate-meeting
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
        set buyer_price offer_price + fam_grant + ehg
        ;print (word "After grant: " buyer_price)
      ]
      ; Check if the sellers' ask price <= buyer's offer price
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

      ifelse lowest_ask_seller != nobody[
        ask lowest_ask_seller [
          set color yellow   ; Change color of the seller to yellow
          set selling? false  ; Set the selling? variable of the seller to false
          set race [race] of self ; Change the race of the seller to the buyers race (since they have alr sold it)

        ]
        set total_houses_sold total_houses_sold + 1
        set total_houses_sold_per_tick total_houses_sold_per_tick + 1

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
        ; Now, let the buyer die
        set total_houses_sold_price total_houses_sold_price + offer_price
        die
      ][
        ;print "Seller ask price is too high"
      ]

    ] [
     ; print "No sellers with the same room type"
    ]
  ] [
   ; print "No eligible sellers found"
  ]
end


to buyer-initiate-meeting_no_government
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

      ifelse lowest_ask_seller != nobody[
        ask lowest_ask_seller [
          set color yellow   ; Change color of the seller to yellow
          set selling? false  ; Set the selling? variable of the seller to false
          set race [race] of self ; Change the race of the seller to the buyers race (since they have alr sold it)

        ]
        set total_houses_sold total_houses_sold + 1
        set total_houses_sold_per_tick total_houses_sold_per_tick + 1

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
        set total_houses_sold_price total_houses_sold_price + offer_price
        ; Now, let the buyer die
        die
      ][
       ; print "Seller ask price is too high"
      ]

    ] [
     ; print "No sellers with the same room type"
    ]
  ] [
  ;  print "No eligible sellers found"
  ]
end

to buyer-initiate-meeting_no_eip
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
        set buyer_price offer_price + fam_grant + ehg
      ;  print (word "After grant: " buyer_price)
      ]
      ; Check if the sellers' ask price <= buyer's offer price
      let max_ask_price max [ask-price] of eligible-sellers
      let lowest_ask_price max_ask_price
      let lowest_ask_seller nobody

      ask same_room_type with [ask-price <= buyer_price][
        ;; check if the current seller has the same race as the races_to_sell_to list and if the ask_price is lower than the previous agent
        if ask-price <= lowest_ask_price [
          set lowest_ask_price ask-price
          set lowest_ask_seller self
        ]

      ]

      ifelse lowest_ask_seller != nobody[
        ask lowest_ask_seller [
          set color yellow   ; Change color of the seller to yellow
          set selling? false  ; Set the selling? variable of the seller to false
          set race [race] of self ; Change the race of the seller to the buyers race (since they have alr sold it)

        ]
        set total_houses_sold total_houses_sold + 1
        set total_houses_sold_per_tick total_houses_sold_per_tick + 1
        set total_houses_sold_price total_houses_sold_price + offer_price
        ; Now, let the buyer die
        die
      ][
    ;    print "Seller ask price is too high"
      ]

    ] [
    ;  print "No sellers with the same room type"
    ]
  ] [
  ;  print "No eligible sellers found"
  ]
end

to buyer-initiate-meeting_no_eip_no_governemnt
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
      let max_ask_price max [ask-price] of eligible-sellers
      let lowest_ask_price max_ask_price
      let lowest_ask_seller nobody

      ask same_room_type with [ask-price <= buyer_price][
        ;; check if the current seller has the same race as the races_to_sell_to list and if the ask_price is lower than the previous agent
        if ask-price <= lowest_ask_price [
          set lowest_ask_price ask-price
          set lowest_ask_seller self
        ]

      ]

      ifelse lowest_ask_seller != nobody[
        ask lowest_ask_seller [
          set color yellow   ; Change color of the seller to yellow
          set selling? false  ; Set the selling? variable of the seller to false
          set race [race] of self ; Change the race of the seller to the buyers race (since they have alr sold it)

        ]
        set total_houses_sold total_houses_sold + 1
        set total_houses_sold_per_tick total_houses_sold_per_tick + 1
        set total_houses_sold_price total_houses_sold_price + offer_price
        ; Now, let the buyer die
        die
      ][
     ;   print "Seller ask price is too high"
      ]

    ] [
    ;  print "No sellers with the same room type"
    ]
  ] [
   ; print "No eligible sellers found"
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
  ask sellers with [selling? = false] [
    ask one-of sellers-here [

      let selling_var? random-float 1.0 < prob_seller_selling

      ;; If seller is willing to sell again
      if selling_var?[
        set color green ; Change the patch color back to green
        set selling? selling_var? ; Change the variable back to true


        ;set ask-price (random-float 100) + 100 ; set the ask price of the house
        set ask-price exp((seller_house_constant * ln(mean_house_price) + seller_lease_year_constant * lease_years) + seller_constant)
      ]
    ]
  ]
  ask sellers with [selling? = true][
    ask one-of sellers-here [
      set ask-price exp((seller_house_constant * ln(mean_house_price) + seller_lease_year_constant * lease_years) + seller_constant)
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
  set total_houses_sold_average total_houses_sold_price / total_houses_sold

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
@#$#@#$#@
GRAPHICS-WINDOW
275
94
884
554
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
1137
126
1270
171
Number of Sellers Selling
count turtles with [color = green]
17
1
11

BUTTON
92
18
155
51
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
905
126
1011
171
Number of Buyers
count turtles with [color = sky or color = violet or color = pink or color = orange]
17
1
11

BUTTON
163
18
259
51
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
906
182
1437
361
Number of Buyers, Sellers
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
"Buyer" 1.0 0 -13791810 true "" "plot count turtles with [shape = \"person\"]"
"Seller" 1.0 0 -11085214 true "" "plot count turtles with [color = green]"

MONITOR
1023
126
1125
171
Number of Flats
count turtles with [color = green or color = yellow]
17
1
11

MONITOR
1287
125
1454
170
Number of sellers not selling
count turtles with [color = yellow]
17
1
11

TEXTBOX
907
96
1127
130
Number of agents in the model
14
0.0
1

TEXTBOX
1138
96
1356
130
Number of sellers in the model
14
0.0
1

PLOT
1369
459
1689
639
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
"Houses Sold" 1.0 0 -11085214 true "" "plot total_houses_sold"
"Houses Not Sold" 1.0 0 -2674135 true "" "plot houses_not_sold"

MONITOR
1370
404
1525
449
Total number of houses
count turtles with [shape = \"house\"]
17
1
11

MONITOR
1535
404
1689
449
Total houses sold
total_houses_sold
17
1
11

TEXTBOX
912
369
1095
403
Houses Sold by ethinicity 
14
0.0
1

MONITOR
909
402
1001
447
Sold to Chinese
total_houses_sold_chinese
17
1
11

MONITOR
1012
402
1111
447
Sold to Indians
total_houses_sold_indian
17
1
11

MONITOR
1122
402
1204
447
Sold to Malay
total_houses_sold_malay
17
1
11

MONITOR
1216
402
1300
447
Sold to others
total_houses_sold_others
17
1
11

PLOT
908
459
1341
639
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
23
99
179
132
government_policy?
government_policy?
0
1
-1000

SLIDER
22
143
209
176
family_grant_income_level
family_grant_income_level
0
30000
16000.0
1000
1
NIL
HORIZONTAL

SLIDER
27
253
141
286
eip_chinese
eip_chinese
0
1
0.8
0.1
1
NIL
HORIZONTAL

SLIDER
147
254
263
287
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
26
293
140
326
eip_malay
eip_malay
0
1
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
146
294
262
327
eip_others
eip_others
0
1
0.1
0.1
1
NIL
HORIZONTAL

TEXTBOX
22
75
172
93
Government Variables
14
0.0
1

TEXTBOX
25
188
217
222
Ethinic Integration Policy
14
0.0
1

TEXTBOX
29
335
179
353
Affordability
14
0.0
1

SWITCH
27
213
214
246
ethnic-integration_policy?
ethnic-integration_policy?
0
1
-1000

SLIDER
27
360
185
393
inflation
inflation
0
10
2.0
0.1
1
%
HORIZONTAL

SLIDER
646
20
818
53
number_of_buyers
number_of_buyers
0
300
100.0
5
1
NIL
HORIZONTAL

SLIDER
1015
19
1175
52
prob_seller_selling
prob_seller_selling
0
0.1
0.01
0.01
1
NIL
HORIZONTAL

TEXTBOX
184
500
291
575
Legend:\nChinese: Sky\nIndian: Pink\nMalay: Violet\nOthers: Orange\n
12
0.0
1

SLIDER
465
19
637
52
number_of_hdb_units
number_of_hdb_units
10
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
831
19
1003
52
prob_of_first_timer
prob_of_first_timer
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
284
18
456
51
density_of_hdb_blocks
density_of_hdb_blocks
0
1
0.75
0.05
1
NIL
HORIZONTAL

TEXTBOX
287
55
437
81
the higher the density the lower the number of hdb blocks
10
0.0
1

CHOOSER
27
450
165
495
paint-hdb-as
paint-hdb-as
"selling" "not-selling" "remove"
0

BUTTON
29
504
110
537
NIL
paint-hdb
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
28
401
186
434
income_growth
income_growth
0
10
2.3
0.1
1
%
HORIZONTAL

TEXTBOX
1370
374
1608
408
Number of houses in the model
14
0.0
1

PLOT
1322
659
1694
898
Average Price of Houses
Time
Price
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total_houses_sold_average"

PLOT
911
658
1300
897
Number of Transactions
time
Transactions
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total_houses_sold_per_tick"

PLOT
536
570
882
799
Affordability (Mean house prices / mean income)
Ticks
Years needed to pay off a house
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"Affordability" 1.0 0 -13345367 true "" "plot affordability"

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
<experiments>
  <experiment name="Stagnant" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt;= 50</exitCondition>
    <metric>total_houses_sold_average</metric>
    <enumeratedValueSet variable="density_of_hdb_blocks">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inflation">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_of_first_timer">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_seller_selling">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income_growth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paint-hdb-as">
      <value value="&quot;selling&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Hyperinflation" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt;= 50</exitCondition>
    <metric>total_houses_sold_average</metric>
    <enumeratedValueSet variable="density_of_hdb_blocks">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inflation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_of_first_timer">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_seller_selling">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income_growth">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="government_policy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_hdb_units">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paint-hdb-as">
      <value value="&quot;selling&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="grant policy" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt;= 50</exitCondition>
    <metric>total_houses_sold_average</metric>
    <enumeratedValueSet variable="ethnic-integration_policy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density_of_hdb_blocks">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="family_grant_income_level">
      <value value="30000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eip_others">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_of_first_timer">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eip_chinese">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_seller_selling">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inflation">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income_growth">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="government_policy?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_hdb_units">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eip_malay">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paint-hdb-as">
      <value value="&quot;selling&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eip_indian">
      <value value="0.12"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Ethnic policy" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt;= 50</exitCondition>
    <metric>total_houses_sold_chinese_average</metric>
    <metric>total_houses_sold_indian_average</metric>
    <metric>total_houses_sold_malay_average</metric>
    <metric>total_houses_sold_others_average</metric>
    <metric>total_houses_sold_average</metric>
    <enumeratedValueSet variable="ethnic-integration_policy?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density_of_hdb_blocks">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="family_grant_income_level">
      <value value="30000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eip_others">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inflation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_of_first_timer">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eip_chinese">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_seller_selling">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="income_growth">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="government_policy?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_hdb_units">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eip_malay">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_buyers">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paint-hdb-as">
      <value value="&quot;selling&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="eip_indian">
      <value value="0.12"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
