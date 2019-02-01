(import nrc.fuzzy.*)
(import nrc.fuzzy.jess.FuzzyMain)
(import nrc.fuzzy.jess.*)
(load-package nrc.fuzzy.jess.FuzzyFunctions)

(defglobal ?*height* = (new FuzzyVariable "height" 130 240.0 "centimeters"))
(defglobal ?*bmi* = (new FuzzyVariable "bmi" 10 40.0 "percentage"))
(defglobal ?*activity* = (new FuzzyVariable "activity" 0 10.0 "percentage"))
(defglobal ?*calorie* = (new FuzzyVariable "calorie" 1000 3000 "cal"))


(defglobal ?*theheight* = "" )
(defglobal ?*thebmi* = "" )
(defglobal ?*theactivity* = "" )
(defglobal ?*thecalorie* = "" )

(defrule init
    (declare (salience 110))
    =>
	(printout t "Welcome to Vegeta Fitness Center" crlf)
	(printout t "I'll be taking in some inputs about your personal health to suggest you the amount of calories you need to consume everyday! " crlf)
	(printout t crlf "************************************************" crlf crlf)
	
	(printout t "enter the height value [1.tall, 2.medium, 3.short] <Type in numbers>" crlf)
	(bind ?theheight (read))
    (if (= ?theheight 1)then
        (bind ?*theheight* "tall")
    else(if (= ?theheight 2)then
            (bind ?*theheight* "medium")
            else (bind ?*theheight* "short")))
	(printout t ?*theheight* crlf)
	(printout t "enter the bmi value [1.underweight, 2.normal, 3.overweight] <Type in numbers>" crlf)
	(bind ?thebmi (read))
        (if (= ?thebmi 1)then
        (bind ?*thebmi* "underweight")
    else(if (= ?thebmi 2)then
            (bind ?*thebmi* "normal")
            else (bind ?*thebmi* "overweight")))
	(printout t ?*thebmi* crlf)
	(printout t "enter the scale of activity everyday [1.light, 2.normal, 3.excess] <Type in numbers>" crlf)
	(bind ?theactivity (read))
      (if (= ?theactivity 1)then
        (bind ?*theactivity* "light")
    else(if (= ?theactivity 2)then
            (bind ?*theactivity* "normal")
            else (bind ?*theactivity* "excess"))  )   
	(printout t ?*theactivity* crlf))

(defrule initialize-fuzzy-variables
    (declare (salience 100))
    =>
    /*
    |short||medium||tall   |
	|<130|150-180|211-240|
    */

    (printout t crlf)
    (printout t "==============Data initialization  and assertion================" crlf)
    (printout t "#*#*#*#*#*#*#*#*Initializing the height data" crlf)
 	(?*height* addTerm "short" (new ZFuzzySet 130 160))
	(?*height* addTerm "medium" (new TriangleFuzzySet 190 30))
    (?*height* addTerm "tall" (new SFuzzySet 211 240))
    
    /*
adding terms for bmi
|underweight||normal||overweight|
|10-18.5|19.5-21.5|21.5-40|
*/
    
    (printout t "#*#*#*#*#*#*#*#*Initializing the bmi data" crlf)
 	(?*bmi* addTerm "underweight" (new ZFuzzySet 10 18.5))
	(?*bmi* addTerm "normal" (new TriangleFuzzySet 19.5 2))
    (?*bmi* addTerm "overweight" (new SFuzzySet 21.5 40))
    
    
     /*
adding terms for activity temp
|light||normal||excess|
|0-2|3-5|6-10|
*/
    
    (printout t "#*#*#*#*#*#*#*#*Initializing the acitvity data" crlf)
 	(?*activity* addTerm "light" (new ZFuzzySet 0 2))
	(?*activity* addTerm "normal" (new TriangleFuzzySet 5 3))
    (?*activity* addTerm "excess" (new SFuzzySet 8 10))
    
    /*
adding terms for calories temp
|low||medium||high   |
|1000-1800|1800-2200|2200-3000|
*/
 	(?*calorie* addTerm "low" (new ZFuzzySet 1000 1800))
	(?*calorie* addTerm "medium" (new TriangleFuzzySet 2200 400))
    (?*calorie* addTerm "high" (new SFuzzySet 2600 3000))
    
    (printout t "#*#*#*#*#*#*#*#*Asserting the data given by the user " crlf)
 	(assert (theheighttemp (new FuzzyValue ?*height* ?*theheight*)))
  	(assert (thebmitemp (new FuzzyValue ?*bmi* ?*thebmi*)))
	(assert (theactivitytemp (new FuzzyValue ?*activity* ?*theactivity*)))
    
      (printout t crlf)
    (printout t "=======================Rule trigger==========================="crlf)
    (printout t "Following rules are trigged with the given input" crlf)
    (printout t crlf)
    
  
    )


;rule 2
(defrule Height-Low
    (declare (salience 3))
    (theheighttemp ?t&:(fuzzy-match ?t "short"))
      =>
    (assert (thecalorie (new FuzzyValue ?*calorie* "slightly medium")))
    (printout t "Triggering rule 2 for short height: " (?t momentDefuzzify) crlf)
)

;rule 3
(defrule Height-Medium
    (declare (salience 2))
    (theheighttemp ?t&:(fuzzy-match ?t "medium"))
      =>
    (assert (thecalorie (new FuzzyValue ?*calorie* "slightly high")))
    (printout t "Triggering rule 3 for medium height: " (?t momentDefuzzify) crlf)
    )

;rule 4
(defrule Height-High
    (declare (salience 1))
    (theheighttemp ?t&:(fuzzy-match ?t "tall"))
      =>
    (assert (thecalorie (new FuzzyValue ?*calorie* "very high")))
    (printout t "Triggering rule 4 for tall height: " (?t momentDefuzzify) crlf)
    )
;Rule 5 
(defrule Bmi-High
        (declare (salience 3))
    (thebmitemp ?t&:(fuzzy-match ?t "overweight"))
      =>
    (assert (thecalorie (new FuzzyValue ?*calorie* "very high")))
    (printout t "Triggering rule 5 for high bmi: " (?t momentDefuzzify) crlf)    
    )

;rule 6
(defrule Bmi-Medium
    (declare (salience 2))
    (thebmitemp ?t&:(fuzzy-match ?t "normal"))
      =>
    (assert (thecalorie (new FuzzyValue ?*calorie* "low")))
    (printout t "Triggering rule 6 for medium bmi: " (?t momentDefuzzify) crlf)
)

;rule 7
(defrule Bmi-Low
    (declare (salience 1))
    (thebmitemp ?t&:(fuzzy-match ?t "underweight"))
      =>
    (assert (thecalorie (new FuzzyValue ?*calorie* "medium")))
    (printout t "Triggering rule 7 for low bmi: " (?t momentDefuzzify) crlf)
)
;rule 8
(defrule Activity-excess
    (declare (salience 3))
    (theactivitytemp ?t&:(fuzzy-match ?t "excess"))
      =>
    (assert (thecalorie (new FuzzyValue ?*calorie* "very low")))
    (printout t "Triggering rule 8 for high activity: " (?t momentDefuzzify) crlf)
)
;rule 9
(defrule Activity-normal
    (declare (salience 2))
    (theactivitytemp ?t&:(fuzzy-match ?t "normal"))
      =>
    (assert (thecalorie (new FuzzyValue ?*calorie* "slightly low")))
    (printout t "Triggering rule 9 for medium activity: " (?t momentDefuzzify) crlf)
)

;rule 10
(defrule Activity-light
    (declare (salience 1))
    (theactivitytemp ?t&:(fuzzy-match ?t "light"))
      =>
    (assert (thecalorie (new FuzzyValue ?*calorie* "very high")))
    (printout t "Triggering rule 10 for light activity: " (?t momentDefuzzify) crlf)
)

;rule for printing
(defrule print-results
    (declare (salience -100))
    (thecalorie ?fc)
    =>
    (printout t crlf)
   (printout t " ======================Analysis and result====================" crlf)        
    (printout t crlf)
    (printout t "Your ideal daily calorie intake should be " (?fc momentDefuzzify) crlf)         
)

(reset)
(run)

