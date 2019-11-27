;Start rule
(defrule start
	:group 
		:initialization
	:when	
	:do
	  (set-entities :herbivore 10 :desert)
)

(defrule get-foot
	:group
		:herbivores
	:when	
		(view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id)))
		
		(simulate-move @cell1
			(get-entity-coordinates @id) 
			(get-entity-coordinates @id1)
			:orthogonal
		)
	:do
		(move-entity-to @id @cell1 :orthogonal)
		(feed-entity @id @id1)
)

(defrule drink
	:group
		:herbivores
	:when
		(< (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 3))

		(search-cell @cell1 (area-around @cell1 :water))
		
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal)
		(drink-water @id)
)

(defrule contamination
	:group
		:herbivores
	:when	
		(equal (get-entity-cell-type @id) :contamination)
		(search-cell-lim @cell1 3
			(or 
				(equal (get-cell-type @cell1) :desert) 
				(equal (get-cell-type @cell1) :grass)
			)
		)
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal)
)

(defrule search
	:group
		:herbivores
	:when	
		(< (get-entity-food @id) (/ (get-type-food (get-entity-type @id)) 2))
		(< (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))

		(search-distant-cell @cell1 (not (equal (get-cell-type @cell1) :water)))

		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
		(move-entity-to @id @cell2 :diagonal)
)
