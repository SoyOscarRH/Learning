; Start rule (create 10 herbivores)
(defrule start
	:group 
    :initialization
	:when
	:do
	  (set-entities :herbivore 10 :desert))


; Find food
(defrule search-food
  :group 
    :herbivores
  :when
		(< (get-entity-food @id) 50)

    (search-distant-cell @cell1
      (equal (get-cell-type @cell1) :desert)
      (simulate-move @cell1 (get-entity-coordinates @id) @cell1 :diagonal))
  :do 
    (move-entity-to @id @cell1 :diagonal))



(defrule get-foot
	:group
		:herbivores
	:when
    
		(view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id)))
		
		(simulate-move @cell1
			(get-entity-coordinates @id)
			(get-entity-coordinates @id1)
			:diagonal)
	:do
		(move-entity-to @id @cell1 :diagonal)
		(feed-entity @id @id1))
