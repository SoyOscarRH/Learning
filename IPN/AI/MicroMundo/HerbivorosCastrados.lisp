; ==== Start rule (create 10 herbivores) ====
(defrule start
	:group :initialization
	:when
	:do (set-entities :herbivore 10 :desert))

; ==== Eat stuff ====
(defrule feed-alone
	:group :herbivores
	:when
    (not (view-field-vision @id2 (equal (get-entity-type @id2) :herbivores)))
    (view-field-vision @id2 (equal (get-entity-type @id2) :herbivores))
		(view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id)))
		(simulate-move @cell1
			(get-entity-coordinates @id)
			(get-entity-coordinates @id1)
			:orthogonal)
	:do
		(move-entity-to @id @cell1 :orthogonal)
		(feed-entity @id @id1))


(defrule feed-group
	:group :herbivores
	:when
    (view-field-vision @id2 (equal (get-entity-type @id2) :herbivores))
    (< (get-entity-water @id) 40)
		(view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id)))
		(simulate-move @cell1
			(get-entity-coordinates @id)
			(get-entity-coordinates @id1)
			:orthogonal)
	:do
		(move-entity-to @id @cell1 :orthogonal)
		(feed-entity @id @id1))

; ==== Move around to find food ====
(defrule move
	:group :herbivores
	:when
		(search-distant-cell @cell1
      (equal :desert (get-cell-type @cell1))
      (not (area-around @cell1 :water))
		  (simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal))
	:do
		(move-entity-to @id @cell1 :orthogonal))


; ==== Drink stuff ====
(defrule drink
	:group :herbivores
	:when
    (< (get-entity-water @id) 50)
		(search-cell @cell1 (area-around @cell1 :water))
	:do
		(move-entity-to @id @cell1 :diagonal)
		(drink-water @id))

; ==== Move around to find water ====
; Find water
(defrule search-water
  :group :herbivores
  :when
    (< (get-entity-water @id) 50)
    (not (search-cell @cell1 (equal (get-cell-type @cell1) :water)))
  :do 
    (move-entity @id :north 3))