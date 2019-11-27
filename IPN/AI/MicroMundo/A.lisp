(defrule start
	:group 
		:initialization
	:when	
	:do
	  (set-entities :herbivore 3 :desert)
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
		(< (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 1))

		(search-cell @cell1 (area-around @cell1 :water))
		
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal)
		(drink-water @id)
)
