extensions[
  csv
]
globals [
  T ; trust values of common neighbour to X
  X ; selected linked neighbour
  data_to_file ; list to hold data
  total_nodes
  exiled_nodes

  ;;; for single node observation
  data_of_single_node
  single_node
  sending_package_x
  comm_neghbour_y
]

turtles-own [
  capacity
  package
]

links-own [
  trust
  rank
  alpha ; positive feedback
  beta ; negative feedback
]

to setup
  ca
  set total_nodes no-of-nodes
  set exiled_nodes 0
  set data_to_file [["aij" "bij" "aik" "akj" "bk" "new_aij" "new_bij" "GC" "BC" "R" "direct-trust" "C" "S" "estimated-trust" "expected-trust" "M" "overall-trust" "exiled nodes"]]
  set data_of_single_node [["aij" "bij" "aik" "akj" "bk" "new_aij" "new_bij" "GC" "BC" "R" "direct-trust" "C" "S" "estimated-trust" "expected-trust" "M" "overall-trust" "final-trust"]]

  crt no-of-nodes [
    setxy random-xcor random-ycor
    set color orange
    set shape "circle"
    set size 0.8
    set capacity 100 + random (501 - 100) ; get capacity from 100-500
  ]

  ask turtles[
    set package 0
    let current self
    repeat create-links [create-link-to one-of other turtles]

    ask out-link-neighbors [
      create-link-to current
      ask my-out-links [
        set rank 1 + random 10
        set trust random-float 1 ; random trust from 0 to less than 1
        set alpha random-float 1
        set beta random-float 1
      ]
    ]
    ask my-out-links [
      set rank 1 + random 10
      set trust random-float 1
      set alpha random-float 1
      set beta random-float 1
    ]
  ]

  ;; get the single good node
  set sending_package_x nobody
  set single_node nobody
  set comm_neghbour_y nobody
  let i 1
  while [single_node = nobody] [
    type i print " times loop has eat has runned"
    ask one-of turtles [
    let current_node self
      ask one-of my-out-links [
        set sending_package_x other-end
      ]
      let com_x_n common-neighbors (turtle-set current_node sending_package_x) 2
      if com_x_n != nobody [
        set single_node current_node
        ask one-of com_x_n[
          set comm_neghbour_y self
          ask my-out-links[
          set trust initial_trust_single_node ; any one connected to single_node will have this initial trust value it.
          set alpha initial_trust_single_node
        set beta initial_trust_single_node

          ]
          ]
        ask sending_package_x [
          ask my-out-links[
          set trust initial_trust_single_node ; any one connected to single_node will have this initial trust value it.
          set alpha initial_trust_single_node
        set beta initial_trust_single_node

          ]
        ]
        print "reached stop condition"
      ]
    ]
    set i (i + 1)
  ]
  reset-ticks
end

to go-single-node
    ;; reset colors
  ask turtles [
    set color orange
    ask my-links [set color gray ]
  ]

  ask single_node [
    ;; visualize
    print "----------------"
    set color blue
    ask my-out-links [
      if other-end = sending_package_x [set color brown]
      if other-end = comm_neghbour_y [set color red]
    ]
    ask sending_package_x[
      set color brown
      ask my-out-links [
      if other-end = single_node [set color brown]
      if other-end = comm_neghbour_y [set color red]
      ]
    ]
    ask comm_neghbour_y[
      set color red
      ask my-out-links [
      if other-end = single_node [set color red]
      if other-end = sending_package_x [set color red]
      ]
    ]
    ;;

    let single_node_has_capacity? false

    let row_data []
    let aij 0
    let bij 0

    let GC  0;;random 1.5 ;; 0 or 1
    let BC (1 - GC) ;; opposite of GC

    let R 0 ; recomendation
    let C 0.5 ; Centrality
    ;set C random-float 1 ; for now randomizing cetrality

    ;;setting service score randomly
    let S 1;;-2 ; service score
    ;;if (random 1.5) = 1 [set S 1]
    ;;

    let estimated-trust 0 ; Estimated Trust
    let expected-trust 0 ; expected trust
    let direct-trust 0 ; direct trust
    let M 0.05 ;; marginal error
    let overall-trust 0 ; overall trust
    let final-trust 0 ;

    set T [] ; empty the previous tursts values


    let from_x_to_single_node_link nobody
    let from_x_to_y_link nobody
    let from_y_to_single_node_link nobody

    ask sending_package_x [ask my-out-links [if other-end = single_node [set from_x_to_single_node_link self]]]
    ask sending_package_x [ask my-out-links [if other-end = comm_neghbour_y [set from_x_to_y_link self]]]
    ask comm_neghbour_y [ask my-out-links [if other-end = single_node [set from_y_to_single_node_link self]]]

    ask from_x_to_single_node_link [
      set direct-trust trust ; this links trust value is
                             ; current node trust value of single_node from sending_package_x
      set aij alpha
      set bij beta
    ]

    ;;
    set row_data lput  aij row_data
    set row_data lput  bij row_data
    ;;

   if capacity > 0 [set single_node_has_capacity? true]
   ifelse single_node_has_capacity? [
       print "single node has capacity"
      let com_x_n common-neighbors (turtle-set single_node sending_package_x) 2
      ask com_x_n [
        ask my-out-links [if other-end = single_node [ set T lput trust T ]]
      ]

        ;; apha beta calculations
        let aik 0
        let bik 0
        let akj 0
        let bkj 0
        ask from_x_to_y_link [
            set aik alpha
            set bik beta
          ]


    ask from_y_to_single_node_link [
      set akj alpha
      set bkj beta
    ]
        set row_data lput aik row_data
        set row_data lput akj row_data
        set row_data lput bkj row_data

        set aij (cal-pos-feedback aij bij akj bkj aik)
        set bij (cal-neg-feedback aik bik akj bkj)

        set row_data lput aij row_data ;; new_aij
        set row_data lput bij row_data ;; new_bij

        ask from_x_to_single_node_link[
          set alpha aij ;; setting new values of aplha
          set beta bij  ;; beta
        ]
        ;; GC BC
        set row_data lput GC row_data
        set row_data lput BC row_data
        ;; calulating recomendation
        set R (cal-recommendation aij bij)
        set row_data lput R row_data

        ;; calculating direct-trust
        let new-direct-trust (cal-direct-trust T direct-trust)
        ask from_x_to_single_node_link [ set trust new-direct-trust ] ;update direct-trust

        set row_data lput new-direct-trust row_data

        ;; calculating centrality
        set row_data lput C row_data

        ;;Service Score
        set row_data lput S row_data

        ;; calculating Estimated trust
        set estimated-trust (cal-estimated-trust R new-direct-trust C S)
        set row_data lput estimated-trust row_data

        ;; calculating Expected trust
        set expected-trust (cal-expected-trust GC BC)
        set row_data lput expected-trust row_data

        ;; calculating overall trust
        set row_data lput M row_data ; marginal error
        set overall-trust (cal-overall-trust estimated-trust expected-trust M)
        set row_data lput overall-trust row_data

       ;; calculating final trust
        set final-trust (cal-final-trust direct-trust new-direct-trust  M)
        set row_data lput final-trust row_data

        ;; append row data to data_to_file
        set data_of_single_node lput row_data data_of_single_node
        type "calculated row = " print row_data

        ifelse new-direct-trust < 0.5 [
          ask from_x_to_single_node_link [
            set rank (rank - 1)
            if rank <= 0 [
              set rank 0
              print "rank is 0" ;
              ;ask from_single_node_to_x_link [die]
              ;die ; killing out going link
            ]
          ]


        ]
        [ ; else trust >=0.5
          set capacity (capacity - 1)
          set package (package + 1)
        ]
    ]
    ; else no capacity
    [print "single_node does not have capacity"]
  ]
  ;; testing

  ;;
  tick
end

to go
  ;; reset colors
  ask turtles [
    set color orange
    ask my-links [set color gray ]
  ]
  ;;;
  ask one-of turtles [

    print "----------------"
    ask my-out-links [set color brown]
    set color blue

    let current_node  self
    let x_has_capacity? false


    let row_data []
    let aij 0
    let bij 0

    let GC random 1.5 ;; 0 or 1
    let BC (1 - GC) ;; opposite of GC

    let R 0 ; recomendation
    let C 0 ; Centrality
    set C random-float 1 ; for now randomizting cetrality

    ;;setting service score randomly
    let S -2 ; service score
    if (random 1.5) = 1 [set S 1]
    ;;

    let estimated-trust 0 ; Estimated Trust
    let expected-trust 0 ; expected trust
    let direct-trust 0 ; direcr trust
    let M 0.05 ;; marginal error
    let overall-trust 0 ; overall trust

    set T [] ; empty the previous tursts values


    let current-link one-of my-out-links

    if current-link = nobody [
      print "Not linked to anyone"
      stop
    ]

    ask current-link [
      set X other-end ; get node at the other send of selected link
      set direct-trust trust ; this links trust value is current node trust value of X

      set aij alpha
      set bij beta
    ]
    ;;
    set row_data lput  aij row_data
    set row_data lput  bij row_data
    ;;

    ask X [ if capacity > 0 [set x_has_capacity? true set color red] ask my-out-links [set color pink]]
    ifelse x_has_capacity? [
      print "x has capacity"
      let com_x_n common-neighbors (turtle-set self X) 2

      ifelse com_x_n != nobody[

        ;; visulization
        ask com_x_n [
          set color green
          ask my-out-links [
            if other-end = X [ set T lput trust T ]
          ]
        ]
        type length(T) print " common neighbours"
        ;;;;;

        ;; apha beta calculations
        let aik 0
        let bik 0
        let akj 0
        let bkj 0
        let K one-of com_x_n ;; middle common node
        ask my-out-links [
          if other-end = K [
            set aik alpha
            set bik beta
          ]
        ]
        ask K [
          ask my-out-links[
            if other-end = X [
              set akj alpha
              set bkj beta
            ]
          ]
        ]
        set row_data lput aik row_data
        set row_data lput akj row_data
        set row_data lput bkj row_data

        set aij (cal-pos-feedback aij bij akj bkj aik)
        set bij (cal-neg-feedback aik bik akj bkj)

        set row_data lput aij row_data ;; new_aij
        set row_data lput bij row_data ;; new_bij

        ask current-link[
          set alpha aij ;; setting new values of aplha
          set beta bij  ;; beta
        ]
        ;; GC BC
        set row_data lput GC row_data
        set row_data lput BC row_data
        ;; calulating recomendation
        set R (cal-recommendation aij bij)
        set row_data lput R row_data

        ;; calculating direct-trust
        let new-direct-trust (cal-direct-trust T direct-trust)
        ask current-link [ set trust new-direct-trust ] ;update direct-trust

        set row_data lput new-direct-trust row_data

        ;; calculating centrality
        set row_data lput C row_data

        ;;Service Score
        set row_data lput S row_data

        ;; calculating Estimated trust
        set estimated-trust (cal-estimated-trust R new-direct-trust C S)
        set row_data lput estimated-trust row_data

        ;; calculating Expected trust
        set expected-trust (cal-expected-trust GC BC)
        set row_data lput expected-trust row_data

        ;; calculating overall trust
        set row_data lput M row_data ; marginal error
        set overall-trust (cal-overall-trust estimated-trust expected-trust M)
        set row_data lput overall-trust row_data


        ifelse new-direct-trust < 0.5 [
          ask current-link [
            set rank (rank - 1)
            if rank <= 0 [
              print "rank is 0 ending link with X" ;
               ask X [
                ask my-out-links[
                  if other-end = current_node [die] ; killing in going link
                ]
              ]
              set exiled_nodes (exiled_nodes + 1)
              die ; killing out going link
            ]
          ]


        ]
        [ ; else trust >=0.5
          ask X [
            set capacity (capacity - 1)
            set package (package + 1)
          ]
        ]

        ;; append row data to data_to_file
        set row_data lput exiled_nodes row_data
        set data_to_file lput row_data data_to_file
        type "calculated row = " print row_data
      ]
      ;else comm_x_n is empty
      [ print "no common neighbours"]
    ]
    ; else no capacity
    [print "x does not have capacity"]
  ]
  ;; testing

  ;;
  tick
end

to-report cal-pos-feedback [aij bij akj bkj aik]
  let numerator (2 * aij * akj )
  let denominator ( ( (bij + 2) * (akj + bkj + 2) ) + (2 * aik) )
  report (numerator / denominator)
end

to-report cal-neg-feedback [aik bik akj bkj]
  let numerator (2 * aik * bkj )
  let denominator ( ( (bik + 2) * (akj + bkj + 2) ) + (2 * aik) )
  report (numerator / denominator)
end

to-report cal-recommendation [aij bij]
  let denominator aij + bij
  if denominator = 0 [set denominator 1] ; avoid division by zero
  report aij / denominator
end

to-report cal-direct-trust [trusts-list direct-trust]
  let new-trust 0
  let numerator 0
  let denominator 0

  let total-nodes (length trusts-list  + 1)

  foreach trusts-list
  [ val ->
    set numerator numerator + (val * random-float 1)
    set denominator denominator + val
  ]
  if denominator = 0 [set denominator 1] ;; avoid division by zero

  set new-trust ( numerator / denominator )
  report new-trust
end

to-report cal-estimated-trust [R D C S]
  report (0.1 * R) + (0.5 * D) + (0.5 * C) + (0.4 * S)
end

to-report cal-expected-trust [GC BC]
  report (GC + 1) / (GC + BC + 2)
end

to-report cal-overall-trust [estimated-trust expected-trust M]
  report (estimated-trust + expected-trust - M) / 2
end

;; for single node
to-report cal-final-trust [initial-trust ij-trust M]
  report (initial-trust * ij-trust - M)
end

to write-to-file
  let first_row lput no-of-nodes ["Total nodes"]
  set data_to_file fput first_row data_to_file
  ;; write all data to file
  csv:to-file file_name data_to_file
end

to write-to-file-single-node
  csv:to-file fname_single_node data_of_single_node
end


to-report show-exiled-nodes
  ;show exiled_nodes
  report exiled_nodes
end

to-report common-neighbors [ in-turtles min-links ]
  let adjacents nobody
  ask in-turtles [ set adjacents (turtle-set adjacents out-link-neighbors) ]
  let out-set nobody
  let intersection nobody
  ask adjacents
  [ set intersection out-link-neighbors with [ member? self in-turtles ]
    if count intersection >= min-links [ set out-set (turtle-set out-set self) ]
  ]
  report out-set
end
@#$#@#$#@
GRAPHICS-WINDOW
547
26
946
426
-1
-1
11.85
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
196
198
259
231
NIL
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

BUTTON
122
198
185
231
NIL
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

BUTTON
35
198
110
231
go once
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

SLIDER
36
55
208
88
no-of-nodes
no-of-nodes
10
800
100.0
10
1
NIL
HORIZONTAL

BUTTON
173
380
269
413
write-to-file
write-to-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
33
299
268
359
file_name
100-nodes-record_test.csv
1
0
String

TEXTBOX
32
11
182
42
Setup
25
121.0
0

TEXTBOX
37
154
187
185
Run
25
121.0
0

TEXTBOX
34
254
184
285
Output
25
121.0
0

SLIDER
36
108
208
141
create-links
create-links
0
10
2.0
1
1
NIL
HORIZONTAL

INPUTBOX
286
297
537
357
fname_single_node
single_bad_node.csv
1
0
String

BUTTON
251
107
364
140
NIL
go-single-node
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
302
379
498
412
NIL
write-to-file-single-node
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
378
107
491
140
NIL
go-single-node
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
248
54
432
87
initial_trust_single_node
initial_trust_single_node
0
1
0.3
0.1
1
NIL
HORIZONTAL

MONITOR
35
369
152
414
NIL
show-exiled-nodes
0
1
11

TEXTBOX
278
10
428
41
Single Node
25
0.0
0

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
NetLogo 6.1.1
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
