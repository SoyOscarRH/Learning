; ==== Start rule (create 10 herbivores) ====
(defrule start
	:group :initialization
	:when
	:do (set-entities :herbivore 10 :desert))

; ==== Move around to find food ====
(defrule move
	:group :herbivores
	:when
		(not (view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id))))
		(search-distant-cell @cell1
      (equal :desert (get-cell-type @cell1))
      (not (area-around @cell1 :water))
		  (simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal))
	:do
		(move-entity-to @id @cell1 :orthogonal))

; ==== Eat stuff ====
(defrule feed-alone
	:group :herbivores
	:when
    (equal 1 (get-entity-type-count @id :herbivore))

		(view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id)))
		(simulate-move @cell1
			(get-entity-coordinates @id)
			(get-entity-coordinates @id1)
			:orthogonal)
	:do
		(move-entity-to @id (get-entity-coordinates @id1) :orthogonal)
		(feed-entity @id @id1))

(defrule feed-group
	:group :herbivores
	:when
    (< 1 (get-entity-type-count @id :herbivore))

    (< (get-entity-food @id) 40)
		(view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id)))
		(simulate-move @cell1
			(get-entity-coordinates @id)
			(get-entity-coordinates @id1)
			:orthogonal)
	:do
		(move-entity-to @id @cell1 :orthogonal)
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

; ==== Move around to find water ====
(defrule search-water
  :group :herbivores
  :when
    (< (get-entity-water @id) 30)
    (not (search-cell @cell1 (equal (get-cell-type @cell1) :water)))
  :do 
    (move-entity @id :north 3))

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