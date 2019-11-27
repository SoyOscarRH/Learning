;Manadas de herbívoros y comiendo carroñeros
;Regla de inicialización de entidades
(defrule start
	:group
		:initialization
	:when	
	:do
		(set-entities :herbivore 5 :desert)
		(set-entities :carnivore 5 :desert)
		(set-entities :scavenger 5 :desert)
)

;Regla para recuperar puntos de :food 
(defrule food-herbivores
	:group
		:herbivores
	:when	
		(> (get-entity-movements @id) 1)
		(< (get-entity-food @id) 100)
		(view-field-vision @id1
			(in (get-entity-type @id1) (get-consumable-type @id))
			(> (get-entity-food @id1) 20))
		(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal)
		(feed-entity @id @id1))

;Regla de reproducción en grupos grandes
(defrule reproduce-herbivores
	:group
		:herbivores
	:when	
		(>= (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
		(>= (get-entity-food @id) (/ (get-type-food (get-entity-type @id)) 2))
		(< (get-entity-type-count @id :herbivore) 15)
		(>= (get-entity-days @id) 4)
	:do	
		(reproduce-entity @id))
;Regla de agrupamiento
(defrule herd-herbivores
	:group
		:herbivores
	:when	
		(search-cell @cell1
			(not (equal (get-cell-type @cell1) :water))
			(> (get-cell-entity-type-number @cell1 :herbivore) 2))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal))
;Regla para evitar permanece en una celda contaminada
(defrule contamination-herbivores
	:group
		:herbivores
	:when	
		(equal (get-entity-cell-type @id) :contamination)
		(search-cell-lim @cell1 2
			(or (equal (get-cell-type @cell1) :desert) (equal (get-cell-type @cell1) :grass)))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal))
;;Reglas que solo se aplican a entidades del grupo :carnivores 
;Regla para el consumo de :herbivores por los :carnivores


(defrule eating-herbivores
	:group
		:carnivores
	:when	
		(< (get-entity-food @id) (/ (get-entity-food @id) 2))
		(view-field-vision @id1
			(equal  (get-entity-type @id1) :herbivore)
			(> (get-entity-food @id) 20))
		(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal)
		(feed-entity @id @id1))
				

;Regla para el consumo de :scavengers por los :carnivores
(defrule eating-scavengers
	:group
		:carnivores
	:when	
		(< (get-entity-food @id) (/ (get-entity-food @id) 4))
		(view-field-vision @id1
			(equal  (get-entity-type @id1) :scavenger))
		(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal)
		(feed-entity @id @id1))

;Regla de reproducción para una población pequeña
(defrule reproduce-carnivores
	:group
		:carnivores
	:when	
		(>= (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
		(>= (get-entity-food @id) (/ (get-type-food (get-entity-type @id)) 2))
		(<= (get-entity-type-count @id :carnivore) 10)
		(>= (get-entity-days @id) 4)
	:do
		(reproduce-entity @id))

;Si en su rango de visión existen más de 5 entidades del tipo :carnivore, 
;se moviliza a la celda tipo :grass mas lejana

(defrule emigrate-carnivores
	:group
		:carnivores
	:when	
		(< (get-entity-type-count @id :carnivore) 5)
		(search-distant-cell @cell1
			(equal (get-cell-type @cell1) :grass))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
	:do
		(move-entity-to @id @cell2 :orthogonal))
;Regla para evitar permanece en una celda contaminada
(defrule contamination-carnivores
	:group
		:carnivores
	:when	
		(equal (get-entity-cell-type @id) :contamination)
		(search-cell-lim @cell1 2
			(or (equal (get-cell-type @cell1) :desert) (equal (get-cell-type @cell1) :grass)))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
		(move-entity-to @id @cell2 :diagonal))

;;Reglas que solo se aplican a entidades del grupo :scavengers
;Regla para el consumo de las entidades :scavengers
(defrule food-view-scavengers
	:group
		:scavengers
	:when	
		(> (get-entity-movements @id) 1)
		(view-field-vision @id1
			(in (get-entity-type @id1) (get-consumable-type @id))
			(< (manhattan-distance @cell (get-entity-coordinates @id1)) 6))
		(simulate-move @cell2 (get-entity-coordinates @id) (get-entity-coordinates @id1) :diagonal)
	:do
		(move-entity-to @id @cell2 :diagonal)
		(feed-entity @id @id1))
;Regla de reproducción en grupos muy grandes
(defrule reproduce-scavengers
	:group
		:scavengers
	:when	
		(>= (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
		(>= (get-entity-food @id) (/ (get-type-food (get-entity-type @id)) 2))
		(< (get-entity-type-count @id :scavenger) 25)
		(>= (get-entity-days @id) 4)
	:do
		(reproduce-entity @id))
;Regla para la agrupación en manadas
(defrule herd-scavengers
	:group
		:scavengers
	:when	
		(search-cell @cell1
			(not (equal (get-cell-type @cell1) :water))
			(> (get-cell-entity-type-number @cell1 :scavenger) 2))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
		(move-entity-to @id @cell2 :diagonal))
;Regla para evitar permanecer en una celda contaminada
(defrule contamination-scavengers
	:group
		:scavengers
	:when	
		(equal (get-entity-cell-type @id) :contamination)
		(search-cell-lim @cell1 2
			(or (equal (get-cell-type @cell1) :desert) (equal (get-cell-type @cell1) :grass)))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
		(move-entity-to @id @cell2 :diagonal))

;;Las siguientes reglas aplican para los grupos de entidades :carnivores, :scavengers y :herbivores
;Regla para la búsqueda de celdas del tipo :water
(defrule search-water
	:group
		:all
	:when	
		(not(search-cell @cell1
			(equal (get-cell-type @cell1) :water)))
		(search-distant-cell @cell1
			(equal (get-cell-type @cell1) :grass))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
		(move-entity-to @id @cell2 :diagonal))

		
;Regla para recuperar puntos de :water
(defrule drink
	:group
			:all
	:when	
		(< (get-entity-water @id) (/ (get-type-water (get-entity-type @id)) 2))
		(search-cell @cell1
			(or (equal (get-cell-type @cell1) :desert) 
				(equal (get-cell-type @cell1) :grass))
			(area-around @cell1 :water))
		(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
		(move-entity-to @id @cell2 :diagonal)
		(drink-water @id))

