BEGIN;

CREATE TABLE IF NOT EXISTS state(
  	id_state INTEGER  		NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  	name     VARCHAR(31) 	NOT NULL
);

CREATE TABLE IF NOT EXISTS zip(
 	code     VARCHAR(5)  	NOT NULL PRIMARY KEY ,
 	id_state INTEGER  		NOT NULL,
 	neighbor VARCHAR(50) 	NOT NULL,
 	city     VARCHAR(40) 	NULL,
 	FOREIGN KEY (id_state) REFERENCES state (id_state)
);

CREATE TABLE IF NOT EXISTS specialty(
  	id_specialty INTEGER  			NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
   	name         VARCHAR(50) 		NOT NULL,
  	current_cost NUMERIC(15,2) 	NOT NULL
);

CREATE TABLE IF NOT EXISTS drug(
  	id_drug           INTEGER  			NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, 
  	name              VARCHAR(400) 	NOT NULL,
  	presentation      VARCHAR(100) 	NOT NULL,
  	brand             VARCHAR(100) 	NOT NULL,
  	price             NUMERIC(15,2) NOT NULL,
  	need_prescription BOOLEAN 			NOT NULL
);

CREATE TABLE IF NOT EXISTS substance(
  	id_substance 	INTEGER  			NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  	name     			VARCHAR(200) 	NOT NULL
);

CREATE TABLE IF NOT EXISTS is_composed(
  	id_drug 		 	INTEGER  NOT NULL REFERENCES drug(id_drug),
  	id_substance 	INTEGER  NOT NULL REFERENCES substance(id_substance)
);

CREATE TABLE  IF NOT EXISTS establishment(
  	id_establishment INTEGER  		NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  	street           VARCHAR(50) 	NOT NULL,
  	street_number    INTEGER  	 	NOT NULL,
  	zip_code         VARCHAR(5)  	NOT NULL,
  	FOREIGN KEY (zip_code) REFERENCES zip(code)
);

CREATE TABLE IF NOT EXISTS phone_number(
  	id_establishment  INTEGER  			NOT NULL REFERENCES establishment(id_establishment),
  	phone     				VARCHAR(13)  	NOT NULL
);

CREATE TYPE  gender AS ENUM ('male', 'female', 'other');

CREATE TABLE IF NOT EXISTS person(
   	curp            CHAR(18) 			NOT NULL PRIMARY KEY,
  	name            VARCHAR(100) 	NOT NULL,
  	first_lastname  VARCHAR(100) 	NOT NULL,
  	second_lastname VARCHAR(100) 	NOT NULL,
  	gender          GENDER 				NOT NULL,
 	phone           	VARCHAR(13)  	NOT NULL,
  	street          VARCHAR(100) 	NOT NULL,
  	email           VARCHAR(100) 	NOT NULL,
  	zip_code        CHAR(5)  			NOT NULL,
  	street_number   INTEGER  			NOT NULL,
  	FOREIGN KEY (zip_code) REFERENCES zip(code)
);

CREATE TABLE IF NOT EXISTS customer(
   	curp       CHAR(18) NOT NULL PRIMARY KEY,
  	is_patient BOOLEAN 	NOT NULL,
  	FOREIGN KEY (curp) REFERENCES person(curp)
);

CREATE TABLE public.ewallet (
	ewallet_number 	VARCHAR(16) 	NOT NULL PRIMARY KEY,
	curp 						VARCHAR(18) 	NOT NULL,
	balance 				NUMERIC(15,2) NOT NULL
);

CREATE TABLE IF NOT EXISTS doctor(
   	curp                 CHAR(18) 		NOT NULL PRIMARY KEY,
   	id_establishment     INTEGER  		NOT NULL,
  	rfc                  VARCHAR(13) 	NOT NULL,
  	ssn                  CHAR(11)  		NOT NULL,
  	professional_license VARCHAR(8)  	NOT NULL,
  	FOREIGN KEY (curp) REFERENCES person(curp),
  	FOREIGN KEY (id_establishment) REFERENCES establishment(id_establishment)
);

CREATE TABLE IF NOT EXISTS manager(
   	curp                 CHAR(18) 		NOT NULL PRIMARY KEY,
   	id_establishment     INTEGER  		NOT NULL,
  	rfc                  VARCHAR(13) 	NOT NULL,
  	ssn                  CHAR(11)  		NOT NULL,
  	FOREIGN KEY (curp) REFERENCES person(curp),
  	FOREIGN KEY (id_establishment) REFERENCES establishment(id_establishment)
);

CREATE TYPE vehicle AS ENUM ('bike', 'motorbike');


CREATE TABLE IF NOT EXISTS deliveryman(
   	curp                 CHAR(18) 		NOT NULL PRIMARY KEY,
   	id_establishment     INTEGER  		NOT NULL,
  	rfc                  VARCHAR(13) 	NOT NULL,
  	ssn                  CHAR(11)  		NOT NULL,
  	license			     VARCHAR(11),
  	vehicle				 VEHICLE NOT NULL,
  	FOREIGN KEY (curp) REFERENCES person(curp),
  	FOREIGN KEY (id_establishment) REFERENCES establishment(id_establishment)
);

CREATE TABLE IF NOT EXISTS general(
   	curp                 CHAR(18) 		NOT NULL PRIMARY KEY,
   	id_establishment     INTEGER  		NOT NULL,
  	rfc                  VARCHAR(13) 	NOT NULL,
  	ssn                  CHAR(11)  		NOT NULL,
  	FOREIGN KEY (curp) REFERENCES person(curp),
  	FOREIGN KEY (id_establishment) REFERENCES establishment(id_establishment)
);

CREATE TABLE IF NOT EXISTS cleaner(
   	curp                 CHAR(18) 		NOT NULL PRIMARY KEY,
   	id_establishment     INTEGER  		NOT NULL,
  	rfc                  VARCHAR(13) 	NOT NULL,
  	ssn                  CHAR(11)  		NOT NULL,
  	FOREIGN KEY (curp) REFERENCES person(curp),
  	FOREIGN KEY (id_establishment) REFERENCES establishment(id_establishment)
);

CREATE TABLE IF NOT EXISTS cashier(
   	curp                 CHAR(18) 		NOT NULL PRIMARY KEY,
   	id_establishment     INTEGER  		NOT NULL,
  	rfc                  VARCHAR(13) 	NOT NULL,
  	ssn                  CHAR(11)  		NOT NULL,
  	FOREIGN KEY (curp) REFERENCES person(curp),
  	FOREIGN KEY (id_establishment) REFERENCES establishment(id_establishment)
);

CREATE TABLE IF NOT EXISTS offers(
  	curp		 		CHAR(18)  			NOT NULL REFERENCES doctor(curp),
  	id_specialty     	INTEGER 	NOT NULL REFERENCES specialty(id_specialty)
);

CREATE TYPE shift AS ENUM ('morning', 'afternoon');


CREATE TABLE consultation(
   	id_consultation  INTEGER  			NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, 
  	patient          CHAR(18) 			NOT NULL,
 	  id_establishment INTEGER  			NOT NULL,
 	  doctor           CHAR(18) 			NOT NULL,
  	turn             INTEGER  			NOT NULL,
  	total_cost       NUMERIC(15,2)  NOT NULL,
  	shift            SHIFT 					NOT NULL,
  	id_specialty     INTEGER  			NOT NULL,
  	date			 			 DATE 					NOT NULL,
  	FOREIGN KEY (patient) REFERENCES customer(curp),
  	FOREIGN KEY (id_establishment) REFERENCES establishment(id_establishment),
  	FOREIGN KEY (doctor) REFERENCES doctor(curp)
);

CREATE TABLE prescription(
   	invoice                INTEGER  NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, 
  	id_consultation        INTEGER  NOT NULL,
  	next_appointment       DATE,
  	digital_signature      TEXT 		NOT NULL,
  	additional_indications TEXT,
  	FOREIGN KEY (id_consultation) REFERENCES consultation(id_consultation)
);

CREATE TABLE IF NOT EXISTS indicates(
  	invoice		 	INTEGER  	NOT NULL REFERENCES prescription(invoice),
  	id_drug     INTEGER 	NOT NULL REFERENCES drug(id_drug),
  	dosis				SMALLINT 	NOT NULL,
  	hours				SMALLINT 	NOT NULL
);

CREATE TYPE payment AS ENUM ('cash', 'creditcard', 'debitcard', 'ewallet');

CREATE TABLE IF NOT EXISTS purchase(
   	id_purchase    		INTEGER  				NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY, 
  	customer       		CHAR(18) 				NOT NULL,
  	cashier        		CHAR(18)  			NOT NULL,
  	timestamp      		TIMESTAMP 			NOT NULL,
  	payment_method 		PAYMENT 				NOT NULL,
  	total          		NUMERIC(15,2) 	NOT NULL,
  	id_establishment 	INTEGER 				NOT NULL,
  	FOREIGN KEY (customer) REFERENCES customer(curp),
  	FOREIGN KEY (cashier) REFERENCES cashier(curp),
  	FOREIGN KEY (id_establishment) REFERENCES establishment(id_establishment)
);

CREATE TABLE IF NOT EXISTS buys(
  	id_purchase			INTEGER  	NOT NULL REFERENCES purchase(id_purchase),
  	id_drug     		INTEGER 	NOT NULL REFERENCES drug(id_drug),
  	quantity				SMALLINT 	NOT NULL
);

CREATE TABLE IF NOT EXISTS has(
  	id_purchase		 	INTEGER  	NOT NULL REFERENCES purchase(id_purchase),
  	invoice     		INTEGER 	NOT NULL REFERENCES prescription(invoice)
);

COMMIT;