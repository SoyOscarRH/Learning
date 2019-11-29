; ==== Start rule (create 10 herbivores) ====
(defrule start
	:group :initialization
	:when
	:do (set-entities :herbivore 10 :desert))

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

(defrule move-disperse
	:group :herbivores
	:when
		(view-field-vision @id1 
			(> 2 (get-entity-type-count @id1 :herbivore))
			(equal :desert (get-cell-type (get-entity-coordinates @id1)))
			(simulate-move @cell2 @cell (get-entity-coordinates @id1) :diagonal))
	:do
		(move-entity-to @id (get-entity-coordinates @id1) :diagonal))

; ==== Eat stuff ====
(defrule feed
	:group :herbivores
	:when
    (< (get-entity-food @id) 80)

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
	:group :herbivores
	:when
    (< (get-entity-water @id) 30)
		(search-cell @cell1 
      (area-around @cell1 :water)
      (not (equal :contamination (get-cell-type @cell1))))
	:do
		(move-entity-to @id @cell1 :diagonal)
		(drink-water @id))

; ==== Not contamited ====
(defrule move-contamination
	:group 	:herbivores
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
  :group :herbivores
  :when
    (not (search-cell-lim @cell1 3 (equal (get-cell-type @cell1) :water)))
  :do 
    (move-entity @id :south 3))