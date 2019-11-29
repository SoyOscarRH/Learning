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


(defrule get-foot-group
	:group
		:herbivores
	:when
    (view-field-vision @id2 (equal (get-entity-type @id2) :herbivores))

		(view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id)))
		(< (get-entity-food @id) 30)
		
		(simulate-move @cell1
			(get-entity-coordinates @id)
			(get-entity-coordinates @id1)
			:diagonal)
	:do
		(move-entity-to @id @cell1 :diagonal)
		(feed-entity @id @id1))


(defrule get-foot-alone
	:group
		:herbivores
	:when
    (not (view-field-vision @id2 (equal (get-entity-type @id2) :herbivores)))

		(view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id)))
		
		(simulate-move @cell1
			(get-entity-coordinates @id)
			(get-entity-coordinates @id1)
			:diagonal)
	:do
		(move-entity-to @id @cell1 :diagonal)
		(feed-entity @id @id1))


(defrule drink
	:group
		:herbivores
	:when
		(< (get-entity-water @id) 50)
		(search-cell @cell1 (area-around @cell1 :water))
	:do
		(move-entity-to @id @cell1 :diagonal)
		(drink-water @id))

; Find water
(defrule search-water
  :group 
    :herbivores
  :when
    (not (search-cell-lim @cell1 3
      (equal (get-cell-type @cell1) :water)))
  :do 
    (move-entity @id :north 3))


; Find food
(defrule search-food
  :group 
    :herbivores
  :when
		(not (view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id))))
      
  :do 
    (move-entity-to @id @cell1 :diagonal))