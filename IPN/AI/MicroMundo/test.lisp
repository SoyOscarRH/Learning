; ==== Start rule (create 10 herbivores) ====
(defrule start
	:group :initialization
	:when
	:do (set-entities :herbivore 10 :desert))

(defrule move-dos
	:group :herbivores
	:when
		(view-field-vision @id1 
			(> 2 (get-entity-type-count @id1 :herbivore))
			(equal :desert (get-cell-type (get-entity-coordinates @id1))))
	:do
		(move-entity-to @id (get-entity-coordinates @id1) :diagonal))