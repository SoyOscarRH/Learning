; ==== Start rule (create 10 herbivores) ====
(defrule start
	:group :initialization
	:when
	:do   
    (set-entities :herbivore 3 :grass)
    (set-entities :carnivore 3 :desert)
    (set-entities :scavenger 3 :desert))

(defrule move
	:group :herbivores
	:when
		(search-cell @cell1
      (equal :desert (get-cell-type @cell1))
			(not (area-around @cell1 :water))
			(equal 0 (get-cell-entity-type-number @cell1 :herbivore))
		  (simulate-move @cell2 @cell @cell1 :orthogonal))
	:do
		(move-entity-to @id @cell1 :orthogonal))

(defrule move-scab
	:group :scavengers
	:when
		(search-distant-cell @cell1
		  (simulate-move @cell2 @cell @cell1 :orthogonal))
	:do
		(move-entity-to @id @cell1 :orthogonal))

; ==== Eat stuff ====
(defrule feed
	:group :all
	:when
    (< (get-entity-food @id) 70)

		(view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id)))
		(simulate-move @cell1
			(get-entity-coordinates @id)
			(get-entity-coordinates @id1)
			:diagonal)
	:do
		(move-entity-to @id (get-entity-coordinates @id1) :diagonal)
		(feed-entity @id @id1))

; ==== Drink stuff ====
(defrule drink
	:group :all
	:when
    (< (get-entity-water @id) 50)
		(search-cell @cell1 
      (area-around @cell1 :water)
      (not (equal :contamination (get-cell-type @cell1))))
	:do
		(move-entity-to @id @cell1 :diagonal)
		(drink-water @id))

; ==== Not contamited ====
(defrule move-contamination
	:group :carnivores-herbivores
	:when	
		(equal (get-entity-cell-type @id) :contamination)
    (search-cell @cell1 
      (not (equal :contamination (get-cell-type @cell1)))
      (not (equal :water (get-cell-type @cell1)))
      (simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal))
	:do
		(move-entity-to @id @cell1 :orthogonal))

; Find water
(defrule search-water
  :group :all
  :when
    (not (search-cell-lim @cell1 3 (equal (get-cell-type @cell1) :water)))
  :do 
    (move-entity @id :south 3))

(defrule reproduce-her
	:group
			:herbivores
	:when	
			(>= (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
			(>= (get-entity-food @id) (/ (get-type-food (get-entity-type @id)) 2))
			(>= (get-entity-days @id) 4)
	:do	
			(reproduce-entity @id))


(defrule reproduce-car
	:group
			:carnivores
	:when	
			(>= (get-entity-water @id) 70)
			(>= (get-entity-food @id) 70)
			(>= (get-entity-days @id) 20)
	:do	
			(reproduce-entity @id))


(defrule reproduce-scar
	:group
			:scavengers
	:when
	:do	
			(reproduce-entity @id))