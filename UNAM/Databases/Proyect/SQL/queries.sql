-- ( 1 ) Ventas por sucural
SELECT cashier.id_establishment, SUM(total) AS total_sales 
	FROM purchase, cashier 
	WHERE cashier.curp = purchase.cashier 
	GROUP BY cashier.id_establishment
	ORDER BY id_establishment ASC;


-- ( 2 ) Consultas por doctor
SELECT 
		CONCAT(p.first_lastname, ' ', p.second_lastname, ' ', p.name) AS fullname,
		d.curp, 
		COUNT(d.curp) AS total_consultations
	FROM doctor as d, consultation as c, person as p
		WHERE
		d.curp = c.doctor AND
		p.curp = d.curp
	GROUP BY 
		d.curp, fullname
	ORDER BY
		total_consultations DESC;

-- ( 3 ) Consultas por mes y año

SELECT 
		TO_CHAR(date, 'Mon') AS month, 
		EXTRACT(year from date) AS year, 
		COUNT(*) AS consultations_per_month
	FROM consultation
	GROUP BY 
		year, month;

-- ( 4 ) Productos más vendidos
SELECT COUNT(b.id_drug) AS sales, d.*
	FROM drug as d, buys as b
WHERE
d.id_drug = b.id_drug
GROUP BY 
	d.id_drug
ORDER BY 
	sales DESC;

	
-- ( 5 ) Consultas por especialidadd

SELECT s.*, COUNT(c.id_consultation) AS consultations
FROM specialty s 
INNER JOIN consultation c ON c.id_specialty = s.id_specialty
GROUP BY s.id_specialty 
ORDER BY consultations DESC;

-- ( 6 ) Mayor ganancia por especialidad

SELECT s.*, SUM(c.total_cost) AS profit
FROM specialty s 
INNER JOIN consultation c ON c.id_specialty = s.id_specialty
GROUP BY s.id_specialty 
ORDER BY profit DESC;

-- ( 7 ) Balance promedio de los clientes y el de los pacientes

SELECT AVG(e.balance) AS avg_balance, c.is_patient
FROM ewallet e 
INNER JOIN customer c ON c.curp = e.curp
GROUP BY c.is_patient;

-- ( 8 ) Cajeros que atienden más clientes

SELECT c.curp,  CONCAT(p.first_lastname, ' ', p.second_lastname, ' ', p.name) AS fullname , COUNT(p2.id_purchase) AS total_sales
FROM cashier c 
INNER JOIN person p ON p.curp = c.curp
INNER JOIN purchase p2 ON p2.cashier = c.curp
GROUP BY c.curp, fullname
ORDER BY total_sales DESC;


-- ( 9 ) Ventas por hora

SELECT  EXTRACT(hour FROM p.timestamp) AS hour_of_day, COUNT(p.id_purchase) AS sales
FROM purchase p
GROUP BY hour_of_day
ORDER BY hour_of_day ASC;

-- ( 10 ) Cuenta de doctores que generan una próxima cita
SELECT d.curp,  CONCAT(p.first_lastname, ' ', p.second_lastname, ' ', p.name) AS fullname , COUNT(c.id_consultation) AS consultations_next_appointment
FROM doctor d
INNER JOIN person p ON p.curp = d.curp 
INNER JOIN consultation c ON c.doctor = d.curp
INNER JOIN prescription p2 ON p2.id_consultation = c.id_consultation
WHERE p2.next_appointment IS NOT NULL
GROUP BY d.curp, fullname
ORDER BY consultations_next_appointment DESC;

-- ( 11 ) Total de medicamentos en receta por especialidad cuyo precio es mayor a 500
SELECT s.id_specialty, s.name, s.current_cost , COUNT(i.id_drug) AS drugs_prescription
FROM specialty s 
INNER JOIN consultation c ON c.id_specialty = s.id_specialty
INNER JOIN prescription p ON p.id_consultation = c.id_consultation
INNER JOIN indicates i ON p.invoice = i.invoice
WHERE s.current_cost >= 500
GROUP BY s.id_specialty, s.name, s.current_cost;


-- ( 12 ) Posible ganancia por farmacos que se han recetado
SELECT d.id_drug, d.name, COUNT(i.id_drug) * d.price AS possible_revenue
FROM drug d 
INNER JOIN indicates i ON i.id_drug = d.id_drug
GROUP BY d.id_drug, d.name;


-- ( 13 ) Demografía por estado pacientes
SELECT s.id_state , s."name" , COUNT(c.curp) AS patients
FROM state s 
INNER JOIN zip z ON z.id_state = s.id_state 
INNER JOIN person p ON p.zip_code = z.code 
INNER JOIN customer c ON p.curp = c.curp
WHERE c.is_patient IS TRUE
GROUP BY s.id_state , s."name"
ORDER BY s."name" ASC;

-- ( 14 ) La ubicación por estado que genera más ganancia a través de compras por clientes
SELECT s.id_state , s."name" , SUM(p2.total) AS sales
FROM state s 
INNER JOIN zip z ON z.id_state = s.id_state 
INNER JOIN person p ON p.zip_code = z.code 
INNER JOIN customer c ON p.curp = c.curp
INNER JOIN purchase p2 ON p2.customer = c.curp
GROUP BY s.id_state , s."name"
ORDER BY s."name" ASC;


-- ( 15 ) Ventas farmacos que necesitan receta 
SELECT d.id_drug, d.name, (COUNT(b.id_drug) * d.price)  as sales
FROM drug d 
INNER JOIN buys b ON b.id_drug = d.id_drug
WHERE d.need_prescription IS TRUE
GROUP BY d.id_drug, d.name;

