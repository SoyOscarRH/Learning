; Start rule (create 10 herbivores)
(defrule start
	:group 
    :initialization
	:when
	:do
	  (set-entities :herbivore 10 :desert))


(defrule search-food
	:group :herbivores
	:when
		(> (get-entity-movements @id) 1)
		(search-distant-cell @cell1 (equal (get-cell-type @cell1) :grass))
	:do
		(move-entity-to @id @cell1 :diagonal))
