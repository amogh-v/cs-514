(deftemplate question
    (slot text)
    (slot type)
    (slot ident))

(deftemplate answer
    (slot ident)
    (slot text))
(deftemplate food
    (slot food-name)
    (slot food-meal)
    (slot food-cuisine)
    (slot veg-nonveg)
    (slot food-calories)
    )
(deftemplate food-not-found-flag
    (slot flag(default 0))
    )
(deffacts questions
    "The information required from the user."
    (question (ident food-meal) (type string)
        (text "Please select a meal(enter the name):
            1. Breakfast
            2. Lunch
            3. Snacks
            4. Dinner
            "))
    (question (ident food-cuisine) (type string)
        (text "Please select a cuisine from below (enter the name):
            1. North
            2. South
            "))
    (question (ident veg-nonveg) (type string)
        (text "Please select your food preference from below:
            1. Veg
            2. Nonveg
            "))
    (question (ident food-calories) (type string)
        (text "Please enter the calorie count:
            1. High
            2. Low
            "))			)

(deffacts food-data
    "The data about the food menu"
    ;Breakfast
    (food (food-name "Chicken Samosa") (food-meal "Breakfast") (food-cuisine "North") (veg-nonveg "Nonveg")(food-calories "High") )
    (food (food-name "Chole Bhature")(food-meal "Breakfast") (food-cuisine "North") (veg-nonveg "Veg")(food-calories "High") )
    (food (food-name "Moong Dal Cheela")(food-meal "Breakfast") (food-cuisine "North") (veg-nonveg "Veg")(food-calories "Low") )
    (food (food-name "Masala Dosa")(food-meal "Breakfast") (food-cuisine "South") (veg-nonveg "Veg")(food-calories "High") )
    (food (food-name "Idly Sambhar")(food-meal "Breakfast") (food-cuisine "South") (veg-nonveg "Veg")(food-calories "Low") )
    
    ;Lunch
    (food (food-name "Butter Chicken and Rice")(food-meal "Lunch") (food-cuisine "North") (veg-nonveg "Nonveg")(food-calories "High") )
    (food (food-name "Mutton Paya and Chappati")(food-meal "Lunch") (food-cuisine "North") (veg-nonveg "Nonveg")(food-calories "Low") )
    (food (food-name "Kadai Paneer and Naan/ Pulav")(food-meal "Lunch") (food-cuisine "North") (veg-nonveg "Veg")(food-calories "High") )
    (food (food-name "Moong Dal Kichdi")(food-meal "Lunch") (food-cuisine "North") (veg-nonveg "Veg")(food-calories "Low") )
    (food (food-name "Hyderabadi Dum Biryani")(food-meal "Lunch") (food-cuisine "South") (veg-nonveg "Nonveg")(food-calories "High") )
    (food (food-name "Chettinad Chicken and Neer Dosa")(food-meal "Lunch") (food-cuisine "South") (veg-nonveg "Nonveg")(food-calories "Low") )
    (food (food-name "Bisibelebath")(food-meal "Lunch") (food-cuisine "South") (veg-nonveg "Veg")(food-calories "High") )
    (food (food-name "Rasam Rice and Vegetable Curry")(food-meal "Lunch") (food-cuisine "South") (veg-nonveg "Veg")(food-calories "Low") )
    
    
    ;Dinner
    (food (food-name "Tandoori Chicken + Roti and Dal")(food-meal "Dinner") (food-cuisine "North") (veg-nonveg "Nonveg")(food-calories "High") )
    (food (food-name "Lamb Curry and Pulka")(food-meal "Dinner") (food-cuisine "North") (veg-nonveg "Nonveg")(food-calories "Low") )
    (food (food-name "Aloo Parota")(food-meal "Dinner") (food-cuisine "North") (veg-nonveg "Nonveg")(food-calories "High") )
    (food (food-name "Dal and Pulka")(food-meal "Dinner") (food-cuisine "North") (veg-nonveg "Veg")(food-calories "Low") )
    (food (food-name "Chicken Nizami and Rice")(food-meal "Dinner") (food-cuisine "South") (veg-nonveg "Nonveg")(food-calories "High") )
    (food (food-name "Chicken Salna and Rice")(food-meal "Dinner") (food-cuisine "South") (veg-nonveg "Nonveg")(food-calories "Low") )
    (food (food-name "Dosa and Potato Sagu")(food-meal "Dinner") (food-cuisine "South") (veg-nonveg "Veg")(food-calories "High") )
    (food (food-name "Dosa and Chutney")(food-meal "Dinner") (food-cuisine "South") (veg-nonveg "Veg")(food-calories "Low") )
    
    ;Snacks
    (food (food-name "Chicken Kabab")(food-meal "Snacks") (food-cuisine "North") (veg-nonveg "Nonveg")(food-calories "High") )
    (food (food-name "Samosa")(food-meal "Snacks") (food-cuisine "North") (veg-nonveg "Veg")(food-calories "High") )
    (food (food-name "Bajji")(food-meal "Snacks") (food-cuisine "South") (veg-nonveg "Veg")(food-calories "High") )
    (food (food-name "Churmuri")(food-meal "Snacks") (food-cuisine "South") (veg-nonveg "Veg")(food-calories "Low") )
    )

;
(deffunction display-food-menu (?fn ?fm ?fc ?vnv ?c)
    (printout t "><><><><><><><><><><><><><><><><><><><><><><" crlf)
    (printout t "><><><><><><><><><><><><><><><><><><><><><><" crlf)
    (disp "Food Name" ?fn )
    (disp "Meal" ?fm )
    (disp "Cuisine" ?fc )
    (disp "Veg/Nonveg" ?vnv )
    (disp "Calories" ?c )
    (printout t "><><><><><><><><><><><><><><><><><><><><><><" crlf)
    
    (return)
    )

(deffunction disp (?food-prop ?value)
    (printout t (str-cat ?food-prop ": " ?value) crlf )
    (return)
    )

(defrule food-recommendation
    (answer (ident ?food-meal)(text ?t))
    (answer (ident ?food-cuisine) (text ?tfc))
    (answer (ident ?veg-nonveg) (text ?tvnv))
    (answer (ident ?food-calories) (text ?tc))
    ?flag-fact <-(food-not-found-flag (flag ?f&:(or (eq ?f 1) (eq ?f 2))))
    ?food<-(food (food-name ?fn)
        (food-meal ?fm&:(eq (str-compare ?fm ?t) 0))
        (food-cuisine ?fc&:(eq (str-compare ?fc ?tfc) 0))
        (veg-nonveg ?vnv&:(eq (str-compare ?vnv ?tvnv) 0))
        (food-calories ?c&:(eq (str-compare ?c ?tc) 0)))
    =>
    (display-food-menu ?fn ?fm ?fc ?vnv ?c)
    (retract ?food)
    (modify ?flag-fact (flag 2))
    ;(printout t "flag 0" crlf)
    ;(facts)
    )

;;;;;;;;;;; No funds can be found
(defrule food-not-found
    (food-not-found-flag (flag 1))
    =>
    (printout t "><><><><><><><><><><><><><><><><><><><><><><" crlf)
    (printout t "><><><><><><><><><><><><><><><><><><><><><><" crlf)
    (printout t
        "Sorry! Currently we do not have any food reccomendation based on the criteria entered by you.
    We request you to try again with a different criteria for shortlisting." crlf)
    
    (printout t "><><><><><><><><><><><><><><><><><><><><><><" crlf)
    )
 
(defmodule ask)
(deffunction ask-user (?question ?type)
    "Ask a question, and return the answer"
    (bind ?answer "")
    (while (not (is-of-type ?answer ?type)) do
        (printout t ?question " ")
        (if (eq ?type yes-no) then
            (printout t "(yes or no) "))
        (bind ?answer (read)))
    (return ?answer))

(deffunction is-of-type (?answer ?type)
    "Check that the answer has the right form"
    (if (eq ?type yes-no) then
        (return (or (eq ?answer yes) (eq ?answer no)))
        else (if (eq ?type number) then
            (return (numberp ?answer))
            else (return (> (str-length ?answer) 0)))))

(defrule ask::ask-question-by-id
    "Given the identifier of a question, ask it and assert the answer"
    (declare (auto-focus TRUE))
    (MAIN::question (ident ?id) (text ?text) (type ?type))
    (not (MAIN::answer (ident ?id)))
    ?ask <- (MAIN::ask ?id)
    =>
    (bind ?answer (ask-user ?text ?type))
    (assert (answer (ident ?id) (text ?answer)))
    (retract ?ask)
    (return))

(defmodule on-start)
(defrule to-print
    =>
    ;(printout t "Type your name and press Enter> ")
    ;(bind ?name (read))
    (printout t crlf crlf crlf crlf)
    (printout t crlf "***************************************************************************" crlf)
    (printout t "FOOD RECOMMENDATION SYSTEM" crlf)
    (printout t "***************************************************************************" crlf crlf)
    ;(printout t "Hello, " ?name "." crlf)
    (printout t "Welcome to the Indian Food Recommendation System." crlf)
    (printout t "Please answer the below questions." crlf)
    (printout t "We will recommend you the food." crlf)
    (printout t crlf "***************************************************************************" crlf)
    (printout t "***************************************************************************" crlf crlf)
    )

(defmodule input)
(defrule request-food-meal
    (declare (salience 100))
    =>
    (assert (ask food-meal)))

(defrule request-food-cuisine
    (declare (salience 99))
    =>
    (assert (ask food-cuisine)))

(defrule request-veg-nonveg
    (declare (salience 98))
    =>
    (assert (ask veg-nonveg)))

(defrule request-food-calories
    (declare (salience 97))
    =>
    (assert (ask food-calories))
    (assert(food-not-found-flag (flag 1))))
(deffunction init ()
    (reset)
    (focus on-start input  )
    (run)
    )
(init)
