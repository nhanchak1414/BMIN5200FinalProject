;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT DATA TEMPLATES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate patient
    (slot name_is (type STRING)))

;; Has the patient ever had dialysis before?
(deftemplate previous_dialysis 
    (slot had_previous_dialysis_is (type SYMBOL)
        (allowed-symbols yes no)))

;; Has the patient had a kidney transplant?       
(deftemplate kidney_transplant 
    (slot had_kidney_transplant_is (type SYMBOL)
        (allowed-symbols yes no)))
        
;; Does patient have available baseline SCr values
(deftemplate available_baseline_SCr
    (slot available_baseline_SCr_is (type SYMBOL)
        (allowed-symbols yes no)))

;; What is the patient's normal baseline SCr value?
(deftemplate baseline_SCr
    (slot baseline_SCr_is (type FLOAT)))
        
;; Does patient have normal SCr values
(deftemplate normal_SCr
    (slot normal_SCr_is (type SYMBOL)
        (allowed-symbols yes no)))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCr Data During the Presentation Window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate SCr_day1
    (slot SCr_day1_is (type FLOAT)))
            
(deftemplate SCr_day2
    (slot SCr_day2_is (type FLOAT)))
        
(deftemplate SCr_day3
    (slot SCr_day3_is (type FLOAT)))
        
(deftemplate SCr_day4
    (slot SCr_day4_is (type FLOAT)))
        
(deftemplate SCr_day5
    (slot SCr_day5_is (type FLOAT)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tracking AKI status (yes, no, possible, unknown) and severity and type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate AKI_status_type
    (slot AKI_status (type SYMBOL)
        (allowed-symbols yes no possible unknown)))
        


(deftemplate AKI_stage_one
    (slot AKI_stage_one_is (type SYMBOL)
        (allowed-symbols yes no possible unknown)))        
        
(deftemplate AKI_stage_two
    (slot AKI_stage_two_is (type SYMBOL)
        (allowed-symbols yes no possible unknown)))

(deftemplate AKI_stage_three
    (slot AKI_stage_three_is (type SYMBOL)
        (allowed-symbols yes no possible unknown)))
        
        
        
(deftemplate tAKI_type
    (slot tAKI_type_is (type SYMBOL)
        (allowed-symbols yes no possible unknown)))
        
(deftemplate sAKI_type
    (slot sAKI_type_is (type SYMBOL)
        (allowed-symbols yes no possible unknown)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Does the patient's SCr values exceed baseline by at least 1.5x?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate SCr_day1_exceed
    (slot SCr_day1_exceed_is (type SYMBOL)
        (allowed-symbols yes no)))
            
(deftemplate SCr_day2_exceed
    (slot SCr_day2_exceed_is (type SYMBOL)
        (allowed-symbols yes no)))
        
(deftemplate SCr_day3_exceed
    (slot SCr_day3_exceed_is (type SYMBOL)
        (allowed-symbols yes no)))

(deftemplate SCr_day4_exceed
    (slot SCr_day4_exceed_is (type SYMBOL)
        (allowed-symbols yes no)))

(deftemplate SCr_day5_exceed
    (slot SCr_day5_exceed_is (type SYMBOL)
        (allowed-symbols yes no)))
        
        
        
        
        
(deftemplate ask_staging
    (slot ask_staging_is (type SYMBOL)
        (allowed-symbols yes no possible unknown)))
        
        
(deftemplate ask_type
    (slot ask_type_is (type SYMBOL)
        (allowed-symbols yes no possible unknown)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Defrules in the decision tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        

(defrule initialize
    =>
    (assert (AKI_status_type (AKI_status unknown)))
    (assert (AKI_stage_one (AKI_stage_one_is unknown)))
    (assert (AKI_stage_two (AKI_stage_two_is unknown)))
    (assert (AKI_stage_three (AKI_stage_three_is unknown)))
    (assert (tAKI_type (tAKI_type_is unknown)))
    (assert (sAKI_type (sAKI_type_is unknown)))
    (assert (ask_staging (ask_staging_is unknown)))
    (assert (ask_type (ask_type_is unknown)))

)

;; Get initial facts from patient, and assess name, if they have had dialysis and if they have had a transplant before
(defrule get_initial_facts
    =>
    (printout t "Enter patient name: ")
    (bind ?name (readline))
    (assert (patient (name_is ?name)))

    (printout t "Has this patient had dialysis prior to the presentation window?: ")
    (bind ?dialysis (read))
    (assert (previous_dialysis (had_previous_dialysis_is ?dialysis)))
    
    (printout t "Has this patient ever had a kidney transplant prior to the presentation window?: ")
    (bind ?transplant (read))
    (assert (kidney_transplant (had_kidney_transplant_is ?transplant)))
)

   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DECISION BASED ON AKI DIALYSIS OR TRANSPLANT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If patient has dialysis or transplant, AKI is unknown
(defrule AKI_criteria_notmet
    (logical
        (or 
            (previous_dialysis(had_previous_dialysis_is yes))
            (kidney_transplant(had_kidney_transplant_is yes))
        )
    )
    
    ?f1 <-(AKI_status_type (AKI_status unknown))
    
    =>
    
    (modify ?f1 (AKI_status unknown))
)

;; If patient does not have dialysis or AKI transplant, AKI is possible
(defrule AKI_criteria_met
    (logical
        (and
            (previous_dialysis(had_previous_dialysis_is no))
            (kidney_transplant(had_kidney_transplant_is no))
        )
    )
    
    ?f1 <-(AKI_status_type (AKI_status unknown))
    
    =>
    
    (modify ?f1 (AKI_status possible))
) 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASK IF BASELINE IS AVAILABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If patient has no dialysis or transplant, ask if patient has baseline SCr available
(defrule patient_has_baseline_SCr
    (logical
        (and
            (previous_dialysis(had_previous_dialysis_is no))
            (kidney_transplant(had_kidney_transplant_is no))
        )
    )
    
    =>
    
    (printout t "Does the patient have available baseline SCr data (yes or no)?: ")
    (bind ?baseline (read))
    (assert (available_baseline_SCr (available_baseline_SCr_is ?baseline)))
)
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DECISION IF BASELINE SCR IS AVAILABLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If patient has baseline SCr value, AKI is possible
(defrule patient__has_baseline_SCr
    (available_baseline_SCr (available_baseline_SCr_is yes))
    
    
    ?f1 <-(AKI_status_type (AKI_status possible))
    
    =>
    
    (modify ?f1 (AKI_status possible))
) 

;; If patient has no baseline SCr value, AKI is unknown
(defrule patient__has_no_baseline_SCr
    (available_baseline_SCr (available_baseline_SCr_is no))
    
    ?f1 <-(AKI_status_type (AKI_status possible))
    
    =>
    
    (modify ?f1 (AKI_status unknown))
) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT BASELINE VALUE IF HAS BASELINE VALUE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If patient has no dialysis or transplant, and has available SCr data, what is their baseline SCr value?
(defrule patient_baseline_SCr_values
    (AKI_status_type (AKI_status possible))
    
    =>
    
    (printout t "What is the patient's baseline SCr value (decimal)?: ")
    (bind ?baseline_value (read))
    (assert (baseline_SCr (baseline_SCr_is ?baseline_value)))
)
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT BASELINE VALUE IF HAS BASELINE VALUE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; What are the patient's SCr values during the presentation window?
(defrule patient_SCr_presentation_values
    (and
        (AKI_status_type (AKI_status possible))
        (available_baseline_SCr (available_baseline_SCr_is yes)) 
    )
    
    
    
    =>
    
    (printout t "What is the patient's day 1 SCr value (decimal)?: ")
    (bind ?day1value (read))
    (assert (SCr_day1 (SCr_day1_is ?day1value)))
    
    (printout t "What is the patient's day 2 SCr value (decimal)?: ")
    (bind ?day2value (read))
    (assert (SCr_day2 (SCr_day2_is ?day2value)))
    
    (printout t "What is the patient's day 3 SCr value (decimal)?: ")
    (bind ?day3value (read))
    (assert (SCr_day3 (SCr_day3_is ?day3value)))

    (printout t "What is the patient's day 4 SCr value (decimal)?: ")
    (bind ?day4value (read))
    (assert (SCr_day4 (SCr_day4_is ?day4value)))
    
    (printout t "What is the patient's day 5 SCr value (decimal)?: ")
    (bind ?day5value (read))
    (assert (SCr_day5 (SCr_day5_is ?day5value)))
)
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DETERMINE IF SCR EXCEEDS BASELINE TO QUALIFY FOR AN AKI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
;; Do at least one of the patient's presenting SCr values exceed baseline by at least 1.5x?
(defrule SCr_exceeding_baseline
    (logical
        (available_baseline_SCr (available_baseline_SCr_is yes)) 
        (baseline_SCr (baseline_SCr_is ?baseline))
        (AKI_status_type (AKI_status possible))
        (SCr_day1 (SCr_day1_is ?day1value))
        (SCr_day2 (SCr_day2_is ?day2value))
        (SCr_day3 (SCr_day3_is ?day3value))
        (SCr_day4 (SCr_day4_is ?day4value))
        (SCr_day5 (SCr_day5_is ?day5value))
            (or
                (test (>= (/ ?day1value ?baseline) 1.5))
                (test (>= (/ ?day2value ?baseline) 1.5))
                (test (>= (/ ?day3value ?baseline) 1.5))
                (test (>= (/ ?day4value ?baseline) 1.5))
                (test (>= (/ ?day5value ?baseline) 1.5))
            )
        
    )
     
    ?f1 <-(AKI_status_type (AKI_status possible))
    
    =>
    
    (modify ?f1 (AKI_status yes))
)


;; Do at least one of the patient's presenting SCr values exceed baseline by at least 1.5x?
(defrule SCr_not_exceeding_baseline
    (logical
        (available_baseline_SCr (available_baseline_SCr_is yes)) 
        (baseline_SCr (baseline_SCr_is ?baseline))
        (AKI_status_type (AKI_status possible))
        (SCr_day1 (SCr_day1_is ?day1value))
        (SCr_day2 (SCr_day2_is ?day2value))
        (SCr_day3 (SCr_day3_is ?day3value))
        (SCr_day4 (SCr_day4_is ?day4value))
        (SCr_day5 (SCr_day5_is ?day5value))
            (and
                (test (< (/ ?day1value ?baseline) 1.5))
                (test (< (/ ?day2value ?baseline) 1.5))
                (test (< (/ ?day3value ?baseline) 1.5))
                (test (< (/ ?day4value ?baseline) 1.5))
                (test (< (/ ?day5value ?baseline) 1.5))
            )
        
    )
     
    ?f2 <-(AKI_status_type (AKI_status possible))
    
    =>
    
    (modify ?f2 (AKI_status no))
)
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AKI REPORTING RULES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule has_AKI
    (AKI_status_type (AKI_status yes))
    =>
    (printout t "___________" crlf)
    (printout t "Based on the patient's SCr values, this patient has an AKI." crlf)
    (printout t "___________" crlf)
    (assert (start_AKI yes))
)

(defrule no_AKI
    (AKI_status_type (AKI_status no))
    =>
    (printout t "___________" crlf)
    (printout t "This patient does not have an AKI." crlf)
    (printout t "___________" crlf)
)

(defrule patient_AKI_unknown
    (or
        (previous_dialysis (had_previous_dialysis_is yes))
        (kidney_transplant (had_kidney_transplant_is yes))
        (available_baseline_SCr (available_baseline_SCr_is no))
    )
   
    =>
    (printout t "___________" crlf)
    (printout t "This patient's AKI status is unknown" crlf)
    (printout t "___________" crlf)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASK TO ASSESS FOR STAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule ask_stage
    (start_AKI yes)
    (AKI_status_type (AKI_status yes))
    
    =>
    
    (printout t "Would you like to assess for AKI stage (yes or no)?: ")
    (bind ?stage_answer (read))
    (assert (ask_staging (ask_staging_is ?stage_answer)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DETERMINE AKI STAGING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Does the patient have stage 1 AKI?
(defrule SCr_exceeding_baseline_one
    (logical
        (ask_staging (ask_staging_is yes))
        (available_baseline_SCr (available_baseline_SCr_is yes)) 
        (baseline_SCr (baseline_SCr_is ?baseline))
        (SCr_day1 (SCr_day1_is ?day1value))
        (SCr_day2 (SCr_day2_is ?day2value))
        (SCr_day3 (SCr_day3_is ?day3value))
        (SCr_day4 (SCr_day4_is ?day4value))
        (SCr_day5 (SCr_day5_is ?day5value))
            (and
                (or
                    (test (and (>= (/ ?day1value ?baseline) 1.5) (< (/ ?day1value ?baseline) 2)))
                    (test (and (>= (/ ?day2value ?baseline) 1.5) (< (/ ?day2value ?baseline) 2)))
                    (test (and (>= (/ ?day3value ?baseline) 1.5) (< (/ ?day3value ?baseline) 2)))
                    (test (and (>= (/ ?day4value ?baseline) 1.5) (< (/ ?day4value ?baseline) 2)))
                    (test (and (>= (/ ?day5value ?baseline) 1.5) (< (/ ?day5value ?baseline) 2)))
                )
                (and
                    (test (< (/ ?day1value ?baseline) 2))
                    (test (< (/ ?day2value ?baseline) 2))
                    (test (< (/ ?day3value ?baseline) 2))
                    (test (< (/ ?day4value ?baseline) 2))
                    (test (< (/ ?day5value ?baseline) 2))
                )
            )
            
    )
     
    ?f2 <-(AKI_stage_one (AKI_stage_one_is unknown))
    ?f3 <-(AKI_stage_two (AKI_stage_two_is unknown))
    ?f4 <-(AKI_stage_three (AKI_stage_three_is unknown))

    =>
    
    (modify ?f2 (AKI_stage_one_is yes))
    (modify ?f3 (AKI_stage_two_is no))
    (modify ?f4 (AKI_stage_three_is no))

)



;; Does the patient have stage 2 AKI?
(defrule SCr_exceeding_baseline_two
    (logical
        (ask_staging (ask_staging_is yes))
        (available_baseline_SCr (available_baseline_SCr_is yes)) 
        (baseline_SCr (baseline_SCr_is ?baseline))
        (SCr_day1 (SCr_day1_is ?day1value))
        (SCr_day2 (SCr_day2_is ?day2value))
        (SCr_day3 (SCr_day3_is ?day3value))
        (SCr_day4 (SCr_day4_is ?day4value))
        (SCr_day5 (SCr_day5_is ?day5value))
        (and
            (or
                (test (and (>= (/ ?day1value ?baseline) 2) (< (/ ?day1value ?baseline) 3)))
                (test (and (>= (/ ?day2value ?baseline) 2) (< (/ ?day2value ?baseline) 3)))
                (test (and (>= (/ ?day3value ?baseline) 2) (< (/ ?day3value ?baseline) 3)))
                (test (and (>= (/ ?day4value ?baseline) 2) (< (/ ?day4value ?baseline) 3)))
                (test (and (>= (/ ?day5value ?baseline) 2) (< (/ ?day5value ?baseline) 3)))
            )
            (and
                (test (< (/ ?day1value ?baseline) 3))
                (test (< (/ ?day2value ?baseline) 3))
                (test (< (/ ?day3value ?baseline) 3))
                (test (< (/ ?day4value ?baseline) 3))
                (test (< (/ ?day5value ?baseline) 3))
            )
        )
    )
     
    ?f2 <-(AKI_stage_one (AKI_stage_one_is unknown))
    ?f3 <-(AKI_stage_two (AKI_stage_two_is unknown))
    ?f4 <-(AKI_stage_three (AKI_stage_three_is unknown))

    =>
    
    (modify ?f2 (AKI_stage_one_is no))
    (modify ?f3 (AKI_stage_two_is yes))
    (modify ?f4 (AKI_stage_three_is no))

)



;; Does the patient have stage 3 AKI?
(defrule SCr_exceeding_baseline_three
    (logical
        (ask_staging (ask_staging_is yes))
        (available_baseline_SCr (available_baseline_SCr_is yes)) 
        (baseline_SCr (baseline_SCr_is ?baseline))
        (SCr_day1 (SCr_day1_is ?day1value))
        (SCr_day2 (SCr_day2_is ?day2value))
        (SCr_day3 (SCr_day3_is ?day3value))
        (SCr_day4 (SCr_day4_is ?day4value))
        (SCr_day5 (SCr_day5_is ?day5value))
        (or
            (test (>= (/ ?day1value ?baseline) 3))
            (test (>= (/ ?day2value ?baseline) 3))
            (test (>= (/ ?day3value ?baseline) 3))
            (test (>= (/ ?day4value ?baseline) 3))
            (test (>= (/ ?day5value ?baseline) 3))
            
        )
    )
     
    ?f2 <-(AKI_stage_one (AKI_stage_one_is unknown))
    ?f3 <-(AKI_stage_two (AKI_stage_two_is unknown))
    ?f4 <-(AKI_stage_three (AKI_stage_three_is unknown))

    =>
    
    (modify ?f2 (AKI_stage_one_is no))
    (modify ?f3 (AKI_stage_two_is no))
    (modify ?f4 (AKI_stage_three_is yes))

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AKI REPORTING RULES FOR STAGING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule has_AKI_one
    (AKI_stage_one (AKI_stage_one_is yes))
    =>
    (printout t "___________" crlf)
    (printout t "Based on the patient's SCr values, this patient has a stage 1 AKI." crlf)
    (printout t "___________" crlf)
    (assert (second_AKI yes))

)

(defrule has_AKI_two
    (AKI_stage_two (AKI_stage_two_is yes))
    =>
    (printout t "___________" crlf)
    (printout t "Based on the patient's SCr values, this patient has a stage 2 AKI." crlf)
    (printout t "___________" crlf)
    (assert (second_AKI yes))

)


(defrule has_AKI_three
    (AKI_stage_three (AKI_stage_three_is yes))
    =>
    (printout t "___________" crlf)
    (printout t "Based on the patient's SCr values, this patient has a stage 3 AKI." crlf)
    (printout t "___________" crlf)
    (assert (second_AKI yes))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASK TO ASSESS FOR TYPE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule ask_AKI_type
    (second_AKI yes)
    (or
        (ask_staging (ask_staging_is no))
        (AKI_stage_one (AKI_stage_one_is yes))
        (AKI_stage_two (AKI_stage_two_is yes))
        (AKI_stage_three (AKI_stage_three_is yes))
    )
    
    =>
    
    (printout t "Would you like to assess for AKI type (yes or no)?: ")
    (bind ?type_answer (read))
    (assert (ask_type (ask_type_is ?type_answer)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DETERMINE IF PATIENT HAS tAKI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     

;; Conditions so that patient has tAKI
(defrule has_tAKI
    (logical
        (ask_type (ask_type_is yes))
        (available_baseline_SCr (available_baseline_SCr_is yes)) 
        (baseline_SCr (baseline_SCr_is ?baseline))
        (SCr_day1 (SCr_day1_is ?day1value))
        (SCr_day2 (SCr_day2_is ?day2value))
        (SCr_day3 (SCr_day3_is ?day3value))
        (SCr_day4 (SCr_day4_is ?day4value))
        (SCr_day5 (SCr_day5_is ?day5value))
            (or
                (and
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )
                 
                (and
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )

            )
        
    )
     
    ?f5 <-(tAKI_type (tAKI_type_is unknown))
    ?f6 <-(sAKI_type (sAKI_type_is unknown))
    
    =>
         
    (modify ?f5 (tAKI_type_is yes))
    (modify ?f6 (sAKI_type_is unknown))

)

     
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DETERMINE IF PATIENT HAS sAKI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
(defrule has_sAKI
    (logical
        (ask_type (ask_type_is yes))
        (available_baseline_SCr (available_baseline_SCr_is yes)) 
        (baseline_SCr (baseline_SCr_is ?baseline))
        (SCr_day1 (SCr_day1_is ?day1value))
        (SCr_day2 (SCr_day2_is ?day2value))
        (SCr_day3 (SCr_day3_is ?day3value))
        (SCr_day4 (SCr_day4_is ?day4value))
        (SCr_day5 (SCr_day5_is ?day5value))
            (or 
                (and
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                 (and
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )
                
                (and 
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
               
                (and
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                 )
                 
                (and
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                 
                (and
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )
                
                 (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (>= (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (>= (/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (>= (/ ?day4value ?baseline) 1.5))
                    (test (< (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (>=(/ ?day2value ?baseline) 1.5))
                    (test (< (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )
                
                (and
                    (test (< (/ ?day1value ?baseline) 1.5))
                    (test (< (/ ?day2value ?baseline) 1.5))
                    (test (>= (/ ?day3value ?baseline) 1.5))
                    (test (< (/ ?day4value ?baseline) 1.5))
                    (test (>= (/ ?day5value ?baseline) 1.5))
                )



            )
        
    )
     
    ?f5 <-(tAKI_type (tAKI_type_is unknown))
    ?f6 <-(sAKI_type (sAKI_type_is unknown))
    
    =>
        
    (modify ?f5 (tAKI_type_is unknown)) 
    (modify ?f6 (sAKI_type_is yes))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATIENT HAS AKI TYPE REPORTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defrule patient_has_sAKI
    (sAKI_type (sAKI_type_is yes))

    =>
    (printout t "___________" crlf)
    (printout t "This patient has an sAKI" crlf)
    (printout t "___________" crlf)
)


(defrule patient_has_tAKI
    (tAKI_type (tAKI_type_is yes))

    =>
    (printout t "___________" crlf)
    (printout t "This patient has a tAKI" crlf)
    (printout t "___________" crlf)
)

