; Start rule (create 10 herbivores)
(defrule start
	:group :initialization
	:when
	:do (set-entities :herbivore 10 :desert))

; Get foot
(defrule get-foot
	:group
		:herbivores
	:when
		(< (get-entity-food @id) 70)
		(view-field-vision @id1 (in (get-entity-type @id1) (get-consumable-type @id)))
		(simulate-move @cell1
			(get-entity-coordinates @id)
			(get-entity-coordinates @id1)
			:diagonal)
	:do
		(move-entity-to @id @cell1 :diagonal)
		(feed-entity @id @id1))

; drink
(defrule drink
	:group
		:herbivores
	:when
		(< (get-entity-water @id) 40)
		(search-cell @cell1 (area-around @cell1 :water))
	:do
		(move-entity-to @id @cell1 :diagonal)
		(drink-water @id))


(defrule search-food
	:group :herbivores
	:when
		(> (get-entity-movements @id) 1)
		(search-distant-cell @cell1 (equal (get-cell-type @cell1) :desert))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
		(move-entity-to @id @cell2 :diagonal))

; Find water
(defrule search-water
  :group
    :herbivores
  :when
		(< (get-entity-water @id) 60)

    (not (search-cell-lim @cell1 3
      (equal (get-cell-type @cell1) :water)))
  :do 
    (move-entity @id :north 3))