; ==== Start rule (create 10 herbivores) ====
(defrule start
	:group :initialization
	:when
	:do (set-entities :herbivore 10 :desert))

; ==== Eat stuff ====
(defrule feed
	:group :herbivores
	:when
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
		(search-cell @cell1 (area-around @cell1 :water))
	:do
		(move-entity-to @id @cell1 :diagonal)
		(drink-water @id))

; ==== Move around to find food ====
(defrule search-food
	:group :herbivores
	:when
		(search-distant-cell @cell1
      (equal :desert (get-cell-type @cell1))
		  (simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal))
	:do
		(move-entity-to @id @cell1 :orthogonal))

; ==== Move around to find water ====
; Find water
(defrule search-water
  :group :herbivores
  :when
    (not (search-cell @cell1
      (equal (get-cell-type @cell1) :water)))
  :do 
    (move-entity @id :south 3))